use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::process::Command;
use std::rc::Rc;
use std::thread;
use std::time::Duration;

fn os_string_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn os_string_array_arg(args: &[Value], idx: usize, name: &str) -> Result<Vec<String>, String>
{
    match args.get(idx)
    {
        None | Some(Value::Nil) => Ok(Vec::new()),
        Some(Value::Array(arr)) =>
        {
            let mut out = Vec::new();
            for val in arr.borrow().iter()
            {
                out.push(val.to_string());
            }
            Ok(out)
        }
        _ => Err(format!("{name} expects an array of strings")),
    }
}

fn native_os_env(args: &[Value]) -> Result<Value, String>
{
    let key = os_string_arg(args, 0, "OS.env")?;
    match std::env::var(key)
    {
        Ok(val) => Ok(Value::String(intern::intern_owned(val))),
        Err(_) => Ok(Value::Nil),
    }
}

fn native_os_set_env(args: &[Value]) -> Result<Value, String>
{
    let key = os_string_arg(args, 0, "OS.set_env")?;
    let value = os_string_arg(args, 1, "OS.set_env")?;
    std::env::set_var(key, value);
    Ok(Value::Boolean(true))
}

fn native_os_unset_env(args: &[Value]) -> Result<Value, String>
{
    let key = os_string_arg(args, 0, "OS.unset_env")?;
    std::env::remove_var(key);
    Ok(Value::Boolean(true))
}

fn native_os_sleep(args: &[Value]) -> Result<Value, String>
{
    let ms = match args.get(0)
    {
        Some(Value::Integer { value, .. }) => *value as i64,
        Some(Value::Unsigned { value, .. }) => *value as i64,
        _ => return Err("OS.sleep expects milliseconds".to_string()),
    };
    if ms > 0
    {
        thread::sleep(Duration::from_millis(ms as u64));
    }
    Ok(Value::Nil)
}

fn native_os_run(args: &[Value]) -> Result<Value, String>
{
    let cmd = os_string_arg(args, 0, "OS.run")?;
    let argv = os_string_array_arg(args, 1, "OS.run")?;
    let cwd = match args.get(2)
    {
        None | Some(Value::Nil) => None,
        Some(Value::String(s)) => Some(s.as_str().to_string()),
        _ => return Err("OS.run expects cwd as string or nil".to_string()),
    };
    let env_map = match args.get(3)
    {
        None | Some(Value::Nil) => None,
        Some(Value::Map(map)) => Some(map.clone()),
        _ => return Err("OS.run expects env as map or nil".to_string()),
    };

    let mut command = Command::new(cmd);
    if !argv.is_empty()
    {
        command.args(argv);
    }
    if let Some(cwd) = cwd
    {
        command.current_dir(cwd);
    }
    if let Some(env) = env_map
    {
        for (key, val) in env.borrow().data.iter()
        {
            command.env(key.as_str(), val.to_string());
        }
    }

    let output = command.output().map_err(|e| format!("OS.run failed: {e}"))?;
    let mut map = FxHashMap::default();
    map.insert(
        intern::intern("status"),
        Value::Integer {
            value: output.status.code().unwrap_or(-1) as i128,
            kind: crate::ast::IntKind::I64,
        },
    );
    map.insert(
        intern::intern("success"),
        Value::Boolean(output.status.success()),
    );
    map.insert(
        intern::intern("stdout"),
        Value::String(intern::intern_owned(String::from_utf8_lossy(&output.stdout).to_string())),
    );
    map.insert(
        intern::intern("stderr"),
        Value::String(intern::intern_owned(String::from_utf8_lossy(&output.stderr).to_string())),
    );
    Ok(Value::Map(Rc::new(RefCell::new(MapValue::new(map)))))
}

pub fn build_os_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("env"), Value::NativeFunction(native_os_env));
    map.insert(intern::intern("set_env"), Value::NativeFunction(native_os_set_env));
    map.insert(intern::intern("unset_env"), Value::NativeFunction(native_os_unset_env));
    map.insert(intern::intern("sleep"), Value::NativeFunction(native_os_sleep));
    map.insert(intern::intern("run"), Value::NativeFunction(native_os_run));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
