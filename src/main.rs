mod ast;
mod eval;
mod intern;
mod lexer;
mod parser;
mod value;

use directories::ProjectDirs;
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use rustc_hash::FxHashMap;
use std::env;
use std::fs;
use std::panic;
use std::path::PathBuf;
use std::rc::Rc;
use std::cell::RefCell;

fn main() -> rustyline::Result<()>
{
    let args: Vec<String> = env::args().collect();

    let mut interpreter = eval::Interpreter::new();

    // Prepare program.args
    let script_args = if args.len() > 2 {
        args[2..].to_vec()
    } else {
        Vec::new()
    };

    let args_val = value::Value::Array(
        Rc::new(RefCell::new(script_args.iter()
            .map(|s| value::Value::String(intern::intern_owned(s.clone())))
            .collect()))
    );

    let mut env_map = FxHashMap::default();
    for (key, val) in env::vars() {
        env_map.insert(intern::intern_owned(key), value::Value::String(intern::intern_owned(val)));
    }
    let env_val = value::Value::Map(Rc::new(RefCell::new(env_map)));

    let mut program_map = FxHashMap::default();
    program_map.insert(intern::intern("name"), value::Value::String(intern::intern_owned(args[0].clone())));
    program_map.insert(intern::intern("args"), args_val);
    program_map.insert(intern::intern("env"), env_val);

    interpreter.define_global(intern::intern("program"), value::Value::Map(Rc::new(RefCell::new(program_map))));

    if args.len() > 1
    {
        run_file(&args[1], interpreter);
        Ok(())
    }
    else
    {
        run_repl(interpreter)
    }
}

fn run_file(path: &str, mut interpreter: eval::Interpreter)
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
            // 2. Evaluate
            let eval_result =
                panic::catch_unwind(panic::AssertUnwindSafe(|| interpreter.eval(&ast, &mut [])));

            match eval_result {
                Ok(Ok(_)) => {},
                Ok(Err(e)) => {
                    eprintln!("Error at line {}: {}", e.line, e.message);
                    std::process::exit(1);
                }
                Err(_) => {
                    eprintln!("Unexpected Runtime Panic");
                    std::process::exit(1);
                }
            }
        }
        Err(e) =>
        {
            if let Some(s) = e.downcast_ref::<&str>() {
                eprintln!("Syntax Error: {}", s);
            } else if let Some(s) = e.downcast_ref::<String>() {
                eprintln!("Syntax Error: {}", s);
            } else {
                eprintln!("Syntax Error");
            }
            std::process::exit(1);
        }
    }
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

    if rl.load_history(&history_path).is_err() {}

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
                if trimmed_line == "exit" { break; }
                if !trimmed_line.is_empty() { rl.add_history_entry(line.as_str())?; }
                if trimmed_line.is_empty()
                {
                    if !is_continuation { continue; }
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
                                Ok(Err(e)) => println!("Error at line {}: {}", e.line, e.message),
                                Err(_) => println!("Unexpected Runtime Panic"),
                            }
                        }
                        Err(e) => {
                            if let Some(s) = e.downcast_ref::<&str>() {
                                println!("Syntax Error: {}", s);
                            } else if let Some(s) = e.downcast_ref::<String>() {
                                println!("Syntax Error: {}", s);
                            } else {
                                println!("Syntax Error");
                            }
                        },
                    }
                }
            }
            Err(ReadlineError::Interrupted) => { println!("CTRL-C"); break; }
            Err(ReadlineError::Eof) => { println!("CTRL-D"); break; }
            Err(err) => { println!("Error: {:?}", err); break; }
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
                lexer::Token::If | lexer::Token::While | lexer::Token::For | lexer::Token::Fn => depth += 1,
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
