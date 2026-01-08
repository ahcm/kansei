mod ast;
mod eval;
mod lexer;
mod parser;
mod value;

use std::io::{self, Write};

fn main()
{
    // The new identity
    println!("Kansei v0.1.0");
    println!("The intuitive scripting language. (Ctrl+C to exit)");

    let mut interpreter = eval::Interpreter::new();

    loop
    {
        // A distinct prompt, perhaps 'k>' or just '>>'
        print!("kansei> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input)
        {
            Ok(_) =>
            {
                let input = input.trim();
                if input == "exit"
                {
                    break;
                }
                if input.is_empty()
                {
                    continue;
                }

                let lexer = lexer::Lexer::new(input);

                // We'll wrap this in a basic catch to prevent crashes on syntax errors
                let mut parser = parser::Parser::new(lexer);

                // Note: Real implementations need Result<Expr, Error>
                // For now, we assume valid input to keep the prototype clean
                let ast = parser.parse();

                let result = interpreter.eval(&ast);
                if !input.starts_with("puts")
                {
                    // The return arrow
                    println!("=> {}", result);
                }
            }
            Err(error) => println!("Error reading input: {}", error),
        }
    }
}
