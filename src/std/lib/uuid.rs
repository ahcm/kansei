use super::LibMap;
use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use uuid::Uuid;

fn str_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn native_uuid_v4(_args: &[Value]) -> Result<Value, String>
{
    Ok(Value::String(intern::intern_owned(Uuid::new_v4().to_string())))
}

fn native_uuid_parse(args: &[Value]) -> Result<Value, String>
{
    let text = str_arg(args, 0, "Uuid.parse")?;
    match Uuid::parse_str(&text)
    {
        Ok(uuid) => Ok(Value::String(intern::intern_owned(uuid.to_string()))),
        Err(_) => Ok(Value::Nil),
    }
}

fn native_uuid_is_valid(args: &[Value]) -> Result<Value, String>
{
    let text = str_arg(args, 0, "Uuid.is_valid")?;
    Ok(Value::Boolean(Uuid::parse_str(&text).is_ok()))
}

pub fn build_uuid_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("v4"), Value::NativeFunction(native_uuid_v4));
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uuid_parse));
    map.insert(intern::intern("is_valid"), Value::NativeFunction(native_uuid_is_valid));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("Uuid"), build_uuid_module());
}
