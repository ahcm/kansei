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
    let mut dump_ast = false;
    let mut dump_ast_sexpr = false;
    let mut dump_bytecode = false;
    let mut bytecode_mode = eval::BytecodeMode::Simple;
    let mut script_path: Option<String> = None;
    let mut evaluate_source: Option<String> = None;
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
                    if let Some(rest) = arg.strip_prefix("--evaluate=")
                    {
                        append_eval_source(&mut evaluate_source, rest.to_string());
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
        value::Value::String(intern::intern_owned("wasmi".to_string())),
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
  -h, --help            Show this help
  -e, --evaluate <src>  Evaluate a one-liner
      --dump-ast        Dump AST
      --dump-ast-sexpr  Dump AST as S-Expr
      --dump-bytecode   Dump bytecode
      --bytecode <mode> Bytecode mode: off|simple|advanced"
    );
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
