use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use serde_json::Value as JsonValue;
use std::cell::RefCell;
use std::rc::Rc;

fn str_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn json_to_value(json: JsonValue) -> Value
{
    super::serde::json_to_value(json)
}

fn value_to_json(value: &Value) -> JsonValue
{
    super::serde::value_to_json(value)
}

fn native_toml_parse(args: &[Value]) -> Result<Value, String>
{
    let text = str_arg(args, 0, "Toml.parse")?;
    let toml_value: toml::Value = text
        .parse()
        .map_err(|e| format!("Toml.parse failed: {e}"))?;
    let json = serde_json::to_value(toml_value).map_err(|e| format!("Toml.parse failed: {e}"))?;
    Ok(json_to_value(json))
}

fn native_toml_stringify(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Toml.stringify expects a value".to_string())?;
    let json = value_to_json(value);
    let toml_val: toml::Value = serde_json::from_value(json)
        .map_err(|e| format!("Toml.stringify failed: {e}"))?;
    let out = toml::to_string(&toml_val).map_err(|e| format!("Toml.stringify failed: {e}"))?;
    Ok(Value::String(intern::intern_owned(out)))
}

pub fn build_toml_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_toml_parse));
    map.insert(intern::intern("stringify"), Value::NativeFunction(native_toml_stringify));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
