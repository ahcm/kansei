mod ast;
mod eval;
mod lexer;
mod parser;
mod value;

use directories::ProjectDirs;
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use std::env;
use std::fs;
use std::panic;
use std::path::PathBuf;

fn main() -> rustyline::Result<()>
{
    let args: Vec<String> = env::args().collect();

    let mut interpreter = eval::Interpreter::new();

    // Prepare program.args
    // If running file: args[0]=bin, args[1]=file, args[2...]=script args.
    // If running REPL: args[0]=bin, args[1...]=REPL args? Usually ignore.
    let script_args = if args.len() > 2 {
        args[2..].to_vec()
    } else {
        Vec::new()
    };

    let args_val = value::Value::Array(
        script_args.iter().map(|s| value::Value::String(s.clone())).collect()
    );

    let mut program_map = std::collections::HashMap::new();
    program_map.insert("args".to_string(), args_val);

    interpreter.define_global("program".to_string(), value::Value::Map(program_map));

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
            // 2. Evaluate
            let eval_result =
                panic::catch_unwind(panic::AssertUnwindSafe(|| interpreter.eval(&ast)));

            if eval_result.is_err()
            {
                eprintln!("Runtime Error");
                std::process::exit(1);
            }
        }
        Err(_) =>
        {
            eprintln!("Syntax Error");
            std::process::exit(1);
        }
    }
}

fn run_repl(mut interpreter: eval::Interpreter) -> rustyline::Result<()>
{
    // The new identity
    println!("Kansei v0.0.1");
    println!("Have fun!");

    let mut input_buffer = String::new();

    let mut rl = DefaultEditor::new()?;

    // Determine history file path
    let history_path = if let Some(proj_dirs) = ProjectDirs::from("com", "ahcm", "kansei")
    {
        let data_dir = proj_dirs.data_dir();
        // Ensure the directory exists
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
    {
        // No history file found or unable to read
    }

    loop
    {
        // Determine prompt based on buffer status
        let is_continuation = !input_buffer.is_empty();
        let prompt = if is_continuation { ".. " } else { "k> " };

        let readline = rl.readline(prompt);

        match readline
        {
            Ok(line) =>
            {
                let trimmed_line = line.trim();

                // Handle exit command explicitly
                if trimmed_line == "exit"
                {
                    break;
                }

                if !trimmed_line.is_empty()
                {
                    rl.add_history_entry(line.as_str())?;
                }

                // If user just hits enter on empty line
                if trimmed_line.is_empty()
                {
                    if !is_continuation
                    {
                        continue;
                    }
                    // If continuation, we append the newline (keeps formatting)
                    input_buffer.push('\n');
                }
                else
                {
                    input_buffer.push_str(&line);
                    input_buffer.push('\n');
                }

                // Check if the code block is complete
                if is_balanced(&input_buffer)
                {
                    let source = input_buffer.clone();
                    input_buffer.clear(); // Clear immediately for next input

                    // 1. Parse
                    // We use catch_unwind to handle syntax errors (panics in parser)
                    let parse_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                        let lexer = lexer::Lexer::new(&source);
                        let mut parser = parser::Parser::new(lexer);
                        parser.parse()
                    }));

                    match parse_result
                    {
                        Ok(ast) =>
                        {
                            // 2. Evaluate
                            // We use catch_unwind to handle runtime errors
                            let eval_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                                interpreter.eval(&ast)
                            }));

                            match eval_result
                            {
                                Ok(result) =>
                                {
                                    // Heuristic to suppress output for simple prints
                                    if !source.trim().starts_with("puts")
                                    {
                                        println!("=> {}", result.inspect());
                                    }
                                }
                                Err(_) => println!("Runtime Error"),
                            }
                        }
                        Err(_) => println!("Syntax Error"),
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
            let token = lexer.next_token();
            match token
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