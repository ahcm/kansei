#![feature(portable_simd)]

mod ast;
mod eval;
mod intern;
#[path = "std/mod.rs"]
mod kansei_std;
mod lexer;
mod parser;
mod sexpr;
mod source;
mod value;
mod wasm;
mod pm;
mod lsp;
mod wasm_pm;

use directories::ProjectDirs;
use rustc_hash::FxHashMap;
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::panic;
use std::path::PathBuf;
use std::process;
use std::rc::Rc;

fn native_program_exit(args: &[value::Value]) -> Result<value::Value, String>
{
    let code = match args.get(0)
    {
        None | Some(value::Value::Nil) => 0,
        Some(value::Value::Integer { value, .. }) =>
        {
            i32::try_from(*value).map_err(|_| "program.exit expects a valid i32".to_string())?
        }
        Some(value::Value::Unsigned { value, .. }) =>
        {
            i32::try_from(*value).map_err(|_| "program.exit expects a valid i32".to_string())?
        }
        _ => return Err("program.exit expects an optional integer".to_string()),
    };
    process::exit(code);
}

fn main() -> rustyline::Result<()>
{
    let args: Vec<String> = env::args().collect();
    if args.iter().any(|arg| arg == "--version" || arg == "-V")
    {
        println!("{}", version_string());
        return Ok(());
    }
    if args.iter().any(|arg| arg == "--get-dirs")
    {
        run_get_dirs();
        return Ok(());
    }
    if let Some(code) = handle_subcommand(&args)
    {
        std::process::exit(code);
    }
    let mut dump_ast = false;
    let mut dump_ast_sexpr = false;
    let mut dump_bytecode = false;
    let mut bytecode_mode = eval::BytecodeMode::Simple;
    let mut script_path: Option<String> = None;
    let mut evaluate_source: Option<String> = None;
    let mut log_path: Option<PathBuf> = None;
    let mut script_args: Vec<String> = Vec::new();

    let mut idx = 1;
    while idx < args.len()
    {
        let mut handled = false;
        match args[idx].as_str()
        {
            "-h" | "--help" =>
            {
                print_usage(&args[0]);
                return Ok(());
            }
            "--dump-ast" => dump_ast = true,
            "--dump-ast-sexpr" => dump_ast_sexpr = true,
            "--dump-bytecode" => dump_bytecode = true,
            "-e" | "--evaluate" =>
            {
                if idx + 1 >= args.len()
                {
                    eprintln!("-e/--evaluate requires a value.");
                    return Ok(());
                }
                idx += 1;
                let src = args[idx].clone();
                append_eval_source(&mut evaluate_source, src);
            }
            "-l" | "--log" =>
            {
                if idx + 1 >= args.len()
                {
                    eprintln!("-l/--log requires a file path.");
                    return Ok(());
                }
                idx += 1;
                log_path = Some(PathBuf::from(&args[idx]));
            }
            "--bytecode" =>
            {
                if idx + 1 >= args.len()
                {
                    eprintln!("--bytecode requires a value (off|simple|advanced).");
                    return Ok(());
                }
                idx += 1;
                bytecode_mode = match args[idx].as_str()
                {
                    "off" => eval::BytecodeMode::Off,
                    "simple" => eval::BytecodeMode::Simple,
                    "advanced" => eval::BytecodeMode::Advanced,
                    _ =>
                    {
                        eprintln!(
                            "Unknown --bytecode value '{}'. Use off, simple, or advanced.",
                            args[idx]
                        );
                        return Ok(());
                    }
                };
            }
            arg =>
            {
                if let Some(rest) = arg.strip_prefix("--bytecode=")
                {
                    bytecode_mode = match rest
                    {
                        "off" => eval::BytecodeMode::Off,
                        "simple" => eval::BytecodeMode::Simple,
                        "advanced" => eval::BytecodeMode::Advanced,
                        _ =>
                        {
                            eprintln!(
                                "Unknown --bytecode value '{}'. Use off, simple, or advanced.",
                                rest
                            );
                            return Ok(());
                        }
                    };
                    handled = true;
                }
                if !handled
                {
                    if let Some(rest) = arg.strip_prefix("-e=")
                    {
                        append_eval_source(&mut evaluate_source, rest.to_string());
                        handled = true;
                    }
                }
                if !handled
                {
                    if let Some(rest) = arg.strip_prefix("-l=")
                    {
                        log_path = Some(PathBuf::from(rest));
                        handled = true;
                    }
                }
                if !handled
                {
                    if let Some(rest) = arg.strip_prefix("--evaluate=")
                    {
                        append_eval_source(&mut evaluate_source, rest.to_string());
                        handled = true;
                    }
                }
                if !handled
                {
                    if let Some(rest) = arg.strip_prefix("--log=")
                    {
                        log_path = Some(PathBuf::from(rest));
                        handled = true;
                    }
                }
                if !handled && script_path.is_none()
                {
                    if evaluate_source.is_none()
                    {
                        script_path = Some(arg.to_string());
                    }
                    else
                    {
                        script_args.push(arg.to_string());
                    }
                }
                else if !handled
                {
                    script_args.push(arg.to_string());
                }
            }
        }
        idx += 1;
    }

    if script_path.is_none()
        && evaluate_source.is_none()
        && (dump_ast || dump_ast_sexpr || dump_bytecode)
    {
        eprintln!("Dump flags require a file path or -e/--evaluate.");
        return Ok(());
    }
    if script_path.is_some() && evaluate_source.is_some()
    {
        eprintln!("Cannot use both -e/--evaluate and a script path.");
        return Ok(());
    }

    let mut interpreter = eval::Interpreter::new();
    interpreter.set_bytecode_mode(bytecode_mode);
    if let Some(path) = log_path.as_ref()
    {
        if let Err(e) = interpreter.set_log_file(path)
        {
            eprintln!("Error opening log file '{}': {}", path.display(), e);
            std::process::exit(1);
        }
    }

    // Prepare program.args
    let args_val = value::Value::Array(Rc::new(RefCell::new(
        script_args
            .iter()
            .map(|s| value::Value::String(intern::intern_owned(s.clone())))
            .collect(),
    )));

    let mut env_map = FxHashMap::default();
    for (key, val) in env::vars()
    {
        env_map.insert(intern::intern_owned(key), value::Value::String(intern::intern_owned(val)));
    }
    let env_val = value::Value::Map(Rc::new(RefCell::new(value::MapValue::new(env_map))));

    let mut program_map = FxHashMap::default();
    program_map.insert(
        intern::intern("name"),
        value::Value::String(intern::intern_owned(args[0].clone())),
    );
    program_map.insert(intern::intern("args"), args_val);
    program_map.insert(intern::intern("env"), env_val);
    program_map.insert(
        intern::intern("wasm_backend"),
        value::Value::String(intern::intern_owned("wasmtime".to_string())),
    );
    program_map.insert(
        intern::intern("wasm_backends"),
        value::Value::Array(Rc::new(RefCell::new(
            wasm::available_wasm_backends()
                .into_iter()
                .map(|name| value::Value::String(intern::intern_owned(name.to_string())))
                .collect(),
        ))),
    );
    program_map.insert(intern::intern("exit"), value::Value::NativeFunction(native_program_exit));

    interpreter.define_global(
        intern::intern_symbol("program"),
        value::Value::Map(Rc::new(RefCell::new(value::MapValue::new(program_map)))),
    );

    if let Some(path) = script_path
    {
        interpreter.set_main_path(std::path::Path::new(&path));
        run_file(&path, interpreter, dump_ast, dump_ast_sexpr, dump_bytecode, bytecode_mode);
        Ok(())
    }
    else if let Some(source) = evaluate_source
    {
        if let Ok(dir) = env::current_dir()
        {
            interpreter.set_main_path(&dir);
        }
        run_source(
            "<eval>",
            source,
            interpreter,
            dump_ast,
            dump_ast_sexpr,
            dump_bytecode,
            bytecode_mode,
        );
        Ok(())
    }
    else
    {
        if let Ok(dir) = env::current_dir()
        {
            interpreter.set_main_path(&dir);
        }
        run_repl(interpreter)
    }
}

fn run_file(
    path: &str,
    mut interpreter: eval::Interpreter,
    dump_ast: bool,
    dump_ast_sexpr: bool,
    dump_bytecode: bool,
    bytecode_mode: eval::BytecodeMode,
)
{
    let source = match fs::read_to_string(path)
    {
        Ok(content) => content,
        Err(e) =>
        {
            eprintln!("Error reading file '{}': {}", path, e);
            std::process::exit(1);
        }
    };

    // 1. Parse
    let parse_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
        let lexer = lexer::Lexer::new(&source);
        let mut parser = parser::Parser::new(lexer);
        parser.parse()
    }));

    match parse_result
    {
        Ok(ast) =>
        {
            let mut ast = ast;
            eval::resolve_slots(&mut ast);
            if dump_bytecode
            {
                println!("{}", eval::dump_bytecode(&ast, bytecode_mode));
                return;
            }
            if dump_ast_sexpr
            {
                println!("{}", sexpr::expr_to_sexpr(&ast).to_string());
                return;
            }
            if dump_ast
            {
                println!("{:#?}", ast);
                return;
            }
            // 2. Evaluate
            let eval_result =
                panic::catch_unwind(panic::AssertUnwindSafe(|| interpreter.eval(&ast, &mut [])));

            match eval_result
            {
                Ok(Ok(_)) =>
                {}
                Ok(Err(e)) =>
                {
                    eprintln!("{}", format_runtime_error(&e));
                    std::process::exit(1);
                }
                Err(_) =>
                {
                    eprintln!("Unexpected Runtime Panic");
                    std::process::exit(1);
                }
            }
        }
        Err(e) =>
        {
            if let Some(s) = e.downcast_ref::<&str>()
            {
                eprintln!("Syntax Error: {}", s);
            }
            else if let Some(s) = e.downcast_ref::<String>()
            {
                eprintln!("Syntax Error: {}", s);
            }
            else
            {
                eprintln!("Syntax Error");
            }
            std::process::exit(1);
        }
    }
}

fn run_source(
    label: &str,
    source: String,
    mut interpreter: eval::Interpreter,
    dump_ast: bool,
    dump_ast_sexpr: bool,
    dump_bytecode: bool,
    bytecode_mode: eval::BytecodeMode,
)
{
    let parse_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
        let lexer = lexer::Lexer::new(&source);
        let mut parser = parser::Parser::new(lexer);
        parser.parse()
    }));

    match parse_result
    {
        Ok(ast) =>
        {
            let mut ast = ast;
            eval::resolve_slots(&mut ast);
            if dump_bytecode
            {
                println!("{}", eval::dump_bytecode(&ast, bytecode_mode));
                return;
            }
            if dump_ast_sexpr
            {
                println!("{}", sexpr::expr_to_sexpr(&ast).to_string());
                return;
            }
            if dump_ast
            {
                println!("{:#?}", ast);
                return;
            }
            let eval_result =
                panic::catch_unwind(panic::AssertUnwindSafe(|| interpreter.eval(&ast, &mut [])));

            match eval_result
            {
                Ok(Ok(_)) =>
                {}
                Ok(Err(e)) =>
                {
                    eprintln!("{}", format_runtime_error(&e));
                    std::process::exit(1);
                }
                Err(_) =>
                {
                    eprintln!("Unexpected Runtime Panic");
                    std::process::exit(1);
                }
            }
        }
        Err(e) =>
        {
            if let Some(s) = e.downcast_ref::<&str>()
            {
                eprintln!("Syntax Error in {}: {}", label, s);
            }
            else if let Some(s) = e.downcast_ref::<String>()
            {
                eprintln!("Syntax Error in {}: {}", label, s);
            }
            else
            {
                eprintln!("Syntax Error in {}", label);
            }
            std::process::exit(1);
        }
    }
}

fn append_eval_source(target: &mut Option<String>, src: String)
{
    match target
    {
        Some(existing) =>
        {
            existing.push('\n');
            existing.push_str(&src);
        }
        None =>
        {
            *target = Some(src);
        }
    }
}

fn format_runtime_error(err: &eval::RuntimeError) -> String
{
    let mut out = String::new();
    if err.column > 0
    {
        out.push_str(&format!(
            "Error at line {}:{}: {}",
            err.line, err.column, err.message
        ));
    }
    else
    {
        out.push_str(&format!("Error at line {}: {}", err.line, err.message));
    }
    if !err.source.is_empty()
    {
        out.push('\n');
        out.push_str(err.source.as_str());
    }
    out
}

fn print_usage(bin: &str)
{
    println!(
        "Usage: {bin} [options] [script] [args...]
  -V, --version         Show version
      --get-dirs        Show module/wasm search paths
  -h, --help            Show this help
  -e, --evaluate <src>  Evaluate a one-liner
      --dump-ast        Dump AST
      --dump-ast-sexpr  Dump AST as S-Expr
      --dump-bytecode   Dump bytecode
      --bytecode <mode> Bytecode mode: off|simple|advanced
  -l, --log <path>      Write log output to a file (default: stderr)

Commands:
  {bin} fmt <path>      Format .ks files in place
  {bin} fmt --stdin     Format stdin to stdout
  {bin} check <path>    Parse .ks files and exit non-zero on errors
  {bin} test <path>     Run .ks files and compare against .out/.err if present
  {bin} install [path]  Install modules from kansei.toml or local paths
  {bin} wasm install <name> [--wasm-modules-repo <path-or-url>]
                      Build and install a wasm module from a repo
  {bin} lsp             Start language server on stdio

Version: {version}",
        version = version_string()
    );
}

fn version_string() -> String
{
    format!("Kansei v{}", env!("CARGO_PKG_VERSION"))
}

fn run_get_dirs() -> i32
{
    let cwd = env::current_dir().ok();
    let main_path = cwd.as_deref();
    let module_paths = eval::module_search_paths_for(main_path);
    let wasm_paths = eval::wasm_search_paths_for(main_path);

    println!("Modules:");
    for path in module_paths
    {
        println!("  {}", path.display());
    }

    println!("Wasm:");
    for path in wasm_paths
    {
        println!("  {}", path.display());
    }

    0
}

fn run_repl(mut interpreter: eval::Interpreter) -> rustyline::Result<()>
{
    println!("Kansei v0.0.1");
    println!("Have fun!");

    let mut input_buffer = String::new();
    let mut rl = DefaultEditor::new()?;

    let history_path = if let Some(proj_dirs) = ProjectDirs::from("com", "ahcm", "kansei")
    {
        let data_dir = proj_dirs.data_dir();
        if let Err(e) = fs::create_dir_all(data_dir)
        {
            eprintln!("Warning: Could not create data directory: {}", e);
            PathBuf::from("history.txt")
        }
        else
        {
            data_dir.join("history.txt")
        }
    }
    else
    {
        PathBuf::from("history.txt")
    };

    if rl.load_history(&history_path).is_err()
    {}

    loop
    {
        let is_continuation = !input_buffer.is_empty();
        let prompt = if is_continuation { ".. " } else { "k> " };
        let readline = rl.readline(prompt);

        match readline
        {
            Ok(line) =>
            {
                let trimmed_line = line.trim();
                if trimmed_line == "exit"
                {
                    break;
                }
                if !trimmed_line.is_empty()
                {
                    rl.add_history_entry(line.as_str())?;
                }
                if trimmed_line.is_empty()
                {
                    if !is_continuation
                    {
                        continue;
                    }
                    input_buffer.push('\n');
                }
                else
                {
                    input_buffer.push_str(&line);
                    input_buffer.push('\n');
                }

                if is_balanced(&input_buffer)
                {
                    let source = input_buffer.clone();
                    input_buffer.clear();

                    let parse_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                        let lexer = lexer::Lexer::new(&source);
                        let mut parser = parser::Parser::new(lexer);
                        parser.parse()
                    }));

                    match parse_result
                    {
                        Ok(ast) =>
                        {
                            let mut ast = ast;
                            eval::resolve_slots(&mut ast);
                            // 2. Evaluate
                            // We use catch_unwind to handle runtime errors
                            let eval_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                                interpreter.eval(&ast, &mut [])
                            }));
                            match eval_result
                            {
                                Ok(Ok(result)) =>
                                {
                                    if !source.trim().starts_with("puts")
                                    {
                                        println!("=> {}", result.inspect());
                                    }
                                }
                                Ok(Err(e)) => println!("{}", format_runtime_error(&e)),
                                Err(_) => println!("Unexpected Runtime Panic"),
                            }
                        }
                        Err(e) =>
                        {
                            if let Some(s) = e.downcast_ref::<&str>()
                            {
                                println!("Syntax Error: {}", s);
                            }
                            else if let Some(s) = e.downcast_ref::<String>()
                            {
                                println!("Syntax Error: {}", s);
                            }
                            else
                            {
                                println!("Syntax Error");
                            }
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) =>
            {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) =>
            {
                println!("CTRL-D");
                break;
            }
            Err(err) =>
            {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history(&history_path)
}

// Counts keywords to see if blocks are closed
fn is_balanced(input: &str) -> bool
{
    // We assume Lexer won't panic on valid chars.
    // If it panics on invalid chars (like '!'), catch_unwind prevents crash.
    let check = panic::catch_unwind(|| {
        let mut lexer = lexer::Lexer::new(input);
        let mut depth = 0;
        loop
        {
            let span = lexer.next_token();
            match span.token
            {
                lexer::Token::If
                | lexer::Token::While
                | lexer::Token::For
                | lexer::Token::Loop
                | lexer::Token::Fn => depth += 1,
                lexer::Token::End | lexer::Token::RightBrace => depth -= 1, // Handle { } blocks?
                // Wait, lexer doesn't count braces in is_balanced?
                // `is_balanced` uses Lexer. Lexer emits RightBrace.
                // Block `{ ... }` uses braces.
                // `fn` uses `End`.
                // `call { }` uses braces.
                // So `is_balanced` should count braces too!
                // Also `Fn` token was added to lexer? Yes.
                lexer::Token::LeftBrace => depth += 1,
                lexer::Token::EOF => break,
                _ =>
                {}
            }
        }
        depth
    });

    match check
    {
        Ok(depth) => depth <= 0,
        Err(_) => true, // If lexer crashed, let parser handle (and crash/report) it
    }
}

fn handle_subcommand(args: &[String]) -> Option<i32>
{
    if args.len() < 2
    {
        return None;
    }
    match args[1].as_str()
    {
        "fmt" =>
        {
            let fmt_args = args[2..].to_vec();
            if fmt_args.iter().any(|arg| arg == "--stdin")
            {
                return Some(run_fmt_stdin());
            }
            if fmt_args.is_empty()
            {
                eprintln!("fmt expects a file or directory path, or --stdin.");
                return Some(2);
            }
            Some(run_fmt(&fmt_args))
        }
        "check" =>
        {
            if args.len() < 3
            {
                eprintln!("check expects a file or directory path.");
                return Some(2);
            }
            Some(run_check(&args[2..]))
        }
        "test" =>
        {
            if args.len() < 3
            {
                eprintln!("test expects a file or directory path.");
                return Some(2);
            }
            Some(run_tests(&args[2..]))
        }
        "install" =>
        {
            let code = pm::run_install(&args[2..]);
            Some(code)
        }
        "lsp" =>
        {
            let code = lsp::run_lsp();
            Some(code)
        }
        "wasm" =>
        {
            let code = wasm_pm::run_wasm_command(&args[2..]);
            Some(code)
        }
        _ => None,
    }
}

fn collect_ks_files(path: &std::path::Path, out: &mut Vec<PathBuf>)
{
    if path.is_dir()
    {
        if let Ok(entries) = fs::read_dir(path)
        {
            for entry in entries.flatten()
            {
                collect_ks_files(&entry.path(), out);
            }
        }
    }
    else if path.extension().and_then(|s| s.to_str()) == Some("ks")
    {
        out.push(path.to_path_buf());
    }
}

fn parse_source_string(source: &str) -> Result<ast::Expr, String>
{
    let parse_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
        let lexer = lexer::Lexer::new(source);
        let mut parser = parser::Parser::new(lexer);
        parser.parse()
    }));
    match parse_result
    {
        Ok(ast) => Ok(ast),
        Err(e) =>
        {
            if let Some(s) = e.downcast_ref::<&str>()
            {
                Err(s.to_string())
            }
            else if let Some(s) = e.downcast_ref::<String>()
            {
                Err(s.clone())
            }
            else
            {
                Err("Syntax Error".to_string())
            }
        }
    }
}

fn run_fmt(paths: &[String]) -> i32
{
    let mut files = Vec::new();
    for path in paths
    {
        collect_ks_files(std::path::Path::new(path), &mut files);
    }
    if files.is_empty()
    {
        eprintln!("fmt found no .ks files.");
        return 1;
    }
    let mut failures = 0;
    let mut formatted = 0;
    for file in files
    {
        let content = match fs::read_to_string(&file)
        {
            Ok(content) => content,
            Err(e) =>
            {
                eprintln!("fmt failed to read {}: {}", file.display(), e);
                failures += 1;
                continue;
            }
        };
        match parse_source_string(&content)
        {
            Ok(ast) =>
            {
                let formatted_source = source::expr_to_source(&ast);
                if formatted_source != content
                {
                    if let Err(e) = fs::write(&file, formatted_source)
                    {
                        eprintln!("fmt failed to write {}: {}", file.display(), e);
                        failures += 1;
                        continue;
                    }
                    formatted += 1;
                }
            }
            Err(err) =>
            {
                eprintln!("fmt parse error in {}: {}", file.display(), err);
                failures += 1;
            }
        }
    }
    if failures > 0
    {
        1
    }
    else
    {
        if formatted > 0
        {
            println!("formatted {} file(s)", formatted);
        }
        0
    }
}

fn run_fmt_stdin() -> i32
{
    use std::io::Read;
    let mut input = String::new();
    if std::io::stdin().read_to_string(&mut input).is_err()
    {
        eprintln!("fmt --stdin failed to read input.");
        return 1;
    }
    match parse_source_string(&input)
    {
        Ok(ast) =>
        {
            let formatted = source::expr_to_source(&ast);
            print!("{formatted}");
            0
        }
        Err(err) =>
        {
            eprintln!("fmt --stdin parse error: {err}");
            1
        }
    }
}

fn run_check(paths: &[String]) -> i32
{
    let mut files = Vec::new();
    for path in paths
    {
        collect_ks_files(std::path::Path::new(path), &mut files);
    }
    if files.is_empty()
    {
        eprintln!("check found no .ks files.");
        return 1;
    }
    let mut failures = 0;
    for file in files
    {
        let content = match fs::read_to_string(&file)
        {
            Ok(content) => content,
            Err(e) =>
            {
                eprintln!("check failed to read {}: {}", file.display(), e);
                failures += 1;
                continue;
            }
        };
        if let Err(err) = parse_source_string(&content)
        {
            eprintln!("check parse error in {}: {}", file.display(), err);
            failures += 1;
        }
    }
    if failures > 0 { 1 } else { 0 }
}

fn run_tests(paths: &[String]) -> i32
{
    let mut files = Vec::new();
    for path in paths
    {
        collect_ks_files(std::path::Path::new(path), &mut files);
    }
    if files.is_empty()
    {
        eprintln!("test found no .ks files.");
        return 1;
    }

    let exe = match env::current_exe()
    {
        Ok(exe) => exe,
        Err(e) =>
        {
            eprintln!("test failed to resolve current executable: {}", e);
            return 1;
        }
    };

    let mut failures = 0;
    for file in files
    {
        let output = match process::Command::new(&exe).arg(&file).output()
        {
            Ok(output) => output,
            Err(e) =>
            {
                eprintln!("test failed to run {}: {}", file.display(), e);
                failures += 1;
                continue;
            }
        };
        if !output.status.success()
        {
            eprintln!("test failed (non-zero exit): {}", file.display());
            failures += 1;
            continue;
        }

        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        let out_path = file.with_extension("out");
        let err_path = file.with_extension("err");

        if out_path.exists()
        {
            match fs::read_to_string(&out_path)
            {
                Ok(expected) =>
                {
                    if expected != stdout
                    {
                        eprintln!("test stdout mismatch: {}", file.display());
                        failures += 1;
                        continue;
                    }
                }
                Err(e) =>
                {
                    eprintln!("test failed to read {}: {}", out_path.display(), e);
                    failures += 1;
                    continue;
                }
            }
        }
        if err_path.exists()
        {
            match fs::read_to_string(&err_path)
            {
                Ok(expected) =>
                {
                    if expected != stderr
                    {
                        eprintln!("test stderr mismatch: {}", file.display());
                        failures += 1;
                        continue;
                    }
                }
                Err(e) =>
                {
                    eprintln!("test failed to read {}: {}", err_path.display(), e);
                    failures += 1;
                    continue;
                }
            }
        }
    }

    if failures > 0 { 1 } else { 0 }
}
