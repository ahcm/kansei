use kansei::{Interpreter, Program};

fn main() -> Result<(), Box<dyn std::error::Error>>
{
    let mut interpreter = Interpreter::new();
    interpreter.set_program(Program {
        name: "embed-example".into(),
        args: vec!["from Rust".into()],
        ..Program::default()
    });
    interpreter.register_native("host_identity", |args| {
        args.first().cloned().ok_or_else(|| "expected an argument".to_string())
    });
    let answer = interpreter.eval_source(r#"
        puts(program.args[0])
        fn answer()
            host_identity(6 * 7)
        end
        answer()
    "#)?;
    println!("Kansei returned {answer}");
    assert_eq!(interpreter.call("answer", &[])?, answer);
    Ok(())
}
