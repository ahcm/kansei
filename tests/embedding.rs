use kansei::{BytecodeMode, Error, Interpreter, Program, Value};
use std::cell::RefCell;
use std::io::{self, Write};
use std::rc::Rc;

#[derive(Clone, Default)]
struct Capture(Rc<RefCell<Vec<u8>>>);

impl Write for Capture
{
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize>
    {
        self.0.borrow_mut().extend_from_slice(bytes);
        Ok(bytes.len())
    }

    fn flush(&mut self) -> io::Result<()> { Ok(()) }
}

#[test]
fn hosts_can_evaluate_reuse_globals_and_call_functions()
{
    for mode in [BytecodeMode::Off, BytecodeMode::Simple, BytecodeMode::Advanced]
    {
        let mut interpreter = Interpreter::new();
        interpreter.set_bytecode_mode(mode);
        interpreter.register_native("identity", |args| {
            args.first().cloned().ok_or_else(|| "expected an argument".to_string())
        });
        interpreter.set_global("enabled", Value::Boolean(true));
        assert_eq!(interpreter.eval_source("identity(enabled)").unwrap(), Value::Boolean(true));
        interpreter.eval_source("base = 40\nfn add(x)\n base + x\nend").unwrap();
        let two = interpreter.eval_source("2").unwrap();
        assert_eq!(interpreter.call("add", &[two]).unwrap().to_string(), "42");
        assert_eq!(interpreter.get_global("base").unwrap().to_string(), "40");
        assert!(interpreter.get_global("missing").is_none());
        assert!(interpreter.call("missing", &[]).is_err());
    }
}

#[test]
fn hosts_receive_structured_errors_and_can_continue()
{
    let mut interpreter = Interpreter::new();
    let error = interpreter.eval_source("fn broken(").unwrap_err();
    assert!(std::error::Error::source(&error).is_some());
    assert!(matches!(error, Error::Parse(error) if error.line == 1));
    let error = interpreter.eval_source("assert_eq(1, 2)").unwrap_err();
    assert!(matches!(error, Error::Runtime(error) if error.line == 1));
    let missing = interpreter.call("missing", &[]).unwrap_err();
    assert_eq!(missing.line, 0);
    assert!(missing.source.is_empty());
    assert_eq!(interpreter.eval_source("6 * 7").unwrap().to_string(), "42");
}

#[test]
fn host_metadata_does_not_install_process_exit()
{
    let mut interpreter = Interpreter::new();
    interpreter.set_program(Program {
        name: "embedded".into(),
        args: vec!["hello".into(), "世界".into()],
        env: [("MODE".into(), "test".into())].into(),
    });
    interpreter.eval_source(r#"
        assert_eq(program.name, "embedded")
        assert_eq(program.args[1], "世界")
        assert_eq(program.env["MODE"], "test")
    "#).unwrap();
    let Value::Map(program) = interpreter.get_global("program").unwrap() else { panic!() };
    assert!(!program.borrow().data.keys().any(|key| key.as_str() == "exit"));
    interpreter.set_program(Program::default());
    interpreter.eval_source("assert_eq(program.args, [])").unwrap();
}

#[test]
fn language_output_can_be_captured_in_every_execution_mode()
{
    for mode in [BytecodeMode::Off, BytecodeMode::Simple, BytecodeMode::Advanced]
    {
        let stdout = Capture::default();
        let stderr = Capture::default();
        let mut interpreter = Interpreter::new();
        interpreter.set_bytecode_mode(mode);
        interpreter.set_stdout(stdout.clone());
        interpreter.set_stderr(stderr.clone());
        interpreter.eval_source(r#"
            fn emit()
                print("hello ")
                puts("世界")
                eprint("warning ")
                eputs("message")
            end
            emit()
        "#).unwrap();
        assert_eq!(&*stdout.0.borrow(), "hello 世界\n".as_bytes());
        assert_eq!(&*stderr.0.borrow(), b"warning message\n");
    }
}

#[test]
fn output_failures_return_runtime_errors()
{
    struct Broken;
    impl Write for Broken
    {
        fn write(&mut self, _: &[u8]) -> io::Result<usize>
        {
            Err(io::Error::new(io::ErrorKind::BrokenPipe, "closed"))
        }
        fn flush(&mut self) -> io::Result<()> { Ok(()) }
    }
    let mut interpreter = Interpreter::new();
    interpreter.set_stdout(Broken);
    let error = interpreter.eval_source("puts 42").unwrap_err();
    assert!(matches!(error, Error::Runtime(error) if error.message.contains("closed")));
}
