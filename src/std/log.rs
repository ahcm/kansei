use crate::eval::{Interpreter, LogFileMode};
use crate::intern;
use crate::value::{HostFunction, MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

fn log_path_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a path string")),
    }
}

fn log_mode_arg(args: &[Value], idx: usize, name: &str) -> Result<LogFileMode, String>
{
    let mode_str = match args.get(idx)
    {
        None | Some(Value::Nil) => return Ok(LogFileMode::Append),
        Some(Value::String(s)) => s.as_str(),
        _ => return Err(format!("{name} expects mode as a string")),
    };

    match mode_str
    {
        "append" => Ok(LogFileMode::Append),
        "truncate" => Ok(LogFileMode::Truncate),
        "rotate" =>
        {
            let max_bytes = match args.get(idx + 1)
            {
                Some(Value::Integer { value, .. }) =>
                {
                    u64::try_from(*value).map_err(|_| {
                        format!("{name} rotate max_bytes must be a positive integer")
                    })?
                }
                Some(Value::Unsigned { value, .. }) =>
                {
                    u64::try_from(*value).map_err(|_| {
                        format!("{name} rotate max_bytes must be a positive integer")
                    })?
                }
                Some(Value::Nil) | None =>
                {
                    return Err(format!("{name} rotate expects max_bytes as a number"));
                }
                _ => return Err(format!("{name} rotate expects max_bytes as a number")),
            };
            if max_bytes == 0
            {
                return Err(format!("{name} rotate max_bytes must be > 0"));
            }
            Ok(LogFileMode::Rotate { max_bytes })
        }
        _ => Err(format!("{name} expects mode: append|truncate|rotate")),
    }
}

fn native_log_set(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let path = log_path_arg(args, 0, "log.set")?;
    let mode = log_mode_arg(args, 1, "log.set")?;
    interpreter
        .set_log_file_with_mode(std::path::Path::new(&path), mode)
        .map_err(|err| format!("log.set failed: {err}"))?;
    Ok(Value::Nil)
}

fn native_log_stderr(interpreter: &mut Interpreter, _args: &[Value]) -> Result<Value, String>
{
    interpreter.set_log_stderr();
    Ok(Value::Nil)
}

fn native_log_format(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let fmt = match args.get(0)
    {
        Some(Value::String(s)) => s.as_str().to_string(),
        _ => return Err("log.format expects a string".to_string()),
    };
    interpreter.set_log_format(fmt);
    Ok(Value::Nil)
}

fn native_log_flush(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let flush = match args.get(0)
    {
        Some(Value::Boolean(val)) => *val,
        _ => return Err("log.flush expects a boolean".to_string()),
    };
    interpreter.set_log_flush(flush);
    Ok(Value::Nil)
}

fn native_log_level(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    let level = match args.get(0)
    {
        Some(Value::String(s)) => s.as_str(),
        _ => return Err("log.level expects a string".to_string()),
    };
    interpreter.set_log_level_str(level)?;
    Ok(Value::Nil)
}

fn native_log_get(interpreter: &mut Interpreter, _args: &[Value]) -> Result<Value, String>
{
    let (target, mode, format, flush, level) = interpreter.log_config();
    let mut map = FxHashMap::default();
    map.insert(intern::intern("target"), Value::String(intern::intern_owned(target)));
    map.insert(intern::intern("mode"), Value::String(intern::intern_owned(mode)));
    map.insert(intern::intern("format"), Value::String(intern::intern_owned(format)));
    map.insert(intern::intern("flush"), Value::Boolean(flush));
    map.insert(intern::intern("level"), Value::String(intern::intern_owned(level)));
    Ok(Value::Map(Rc::new(RefCell::new(MapValue::new(map)))))
}

fn native_log_emit(
    interpreter: &mut Interpreter,
    args: &[Value],
    level: &'static str,
) -> Result<Value, String>
{
    let mut last = Value::Nil;
    for arg in args
    {
        let msg = arg.to_string();
        interpreter.log_with_level(level, &msg)?;
        last = arg.clone();
    }
    Ok(last)
}

fn native_log_info(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    native_log_emit(interpreter, args, "info")
}

fn native_log_warn(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    native_log_emit(interpreter, args, "warn")
}

fn native_log_error(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    native_log_emit(interpreter, args, "error")
}

pub fn build_log_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("set"), Value::HostFunction(native_log_set as HostFunction));
    map.insert(
        intern::intern("stderr"),
        Value::HostFunction(native_log_stderr as HostFunction),
    );
    map.insert(
        intern::intern("format"),
        Value::HostFunction(native_log_format as HostFunction),
    );
    map.insert(
        intern::intern("flush"),
        Value::HostFunction(native_log_flush as HostFunction),
    );
    map.insert(
        intern::intern("level"),
        Value::HostFunction(native_log_level as HostFunction),
    );
    map.insert(intern::intern("get"), Value::HostFunction(native_log_get as HostFunction));
    map.insert(
        intern::intern("info"),
        Value::HostFunction(native_log_info as HostFunction),
    );
    map.insert(
        intern::intern("warn"),
        Value::HostFunction(native_log_warn as HostFunction),
    );
    map.insert(
        intern::intern("error"),
        Value::HostFunction(native_log_error as HostFunction),
    );
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
