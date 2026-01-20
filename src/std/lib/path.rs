use super::LibMap;
use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::path::{Component, Path, PathBuf};
use std::rc::Rc;

fn str_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn native_path_join(args: &[Value]) -> Result<Value, String>
{
    let base = str_arg(args, 0, "Path.join")?;
    let next = str_arg(args, 1, "Path.join")?;
    let out = Path::new(&base).join(next).to_string_lossy().to_string();
    Ok(Value::String(intern::intern_owned(out)))
}

fn native_path_basename(args: &[Value]) -> Result<Value, String>
{
    let path = str_arg(args, 0, "Path.basename")?;
    let out = Path::new(&path)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_string();
    Ok(Value::String(intern::intern_owned(out)))
}

fn native_path_dirname(args: &[Value]) -> Result<Value, String>
{
    let path = str_arg(args, 0, "Path.dirname")?;
    let out = Path::new(&path)
        .parent()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|| "".to_string());
    Ok(Value::String(intern::intern_owned(out)))
}

fn native_path_ext(args: &[Value]) -> Result<Value, String>
{
    let path = str_arg(args, 0, "Path.ext")?;
    let out = Path::new(&path)
        .extension()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_string();
    Ok(Value::String(intern::intern_owned(out)))
}

fn native_path_normalize(args: &[Value]) -> Result<Value, String>
{
    let path = str_arg(args, 0, "Path.normalize")?;
    let mut buf = PathBuf::new();
    for comp in Path::new(&path).components()
    {
        match comp
        {
            Component::ParentDir =>
            {
                buf.pop();
            }
            Component::CurDir =>
            {}
            _ => buf.push(comp.as_os_str()),
        }
    }
    Ok(Value::String(intern::intern_owned(buf.to_string_lossy().to_string())))
}

pub fn build_path_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("join"), Value::NativeFunction(native_path_join));
    map.insert(intern::intern("basename"), Value::NativeFunction(native_path_basename));
    map.insert(intern::intern("dirname"), Value::NativeFunction(native_path_dirname));
    map.insert(intern::intern("ext"), Value::NativeFunction(native_path_ext));
    map.insert(intern::intern("normalize"), Value::NativeFunction(native_path_normalize));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("Path"), build_path_module());
}
