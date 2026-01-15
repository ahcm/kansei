use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use serde_json::Value as JsonValue;
use std::cell::RefCell;
use std::rc::Rc;

pub(crate) fn json_to_value(json: JsonValue) -> Value
{
    match json
    {
        JsonValue::Null => Value::Nil,
        JsonValue::Bool(b) => Value::Boolean(b),
        JsonValue::Number(n) =>
        {
            if let Some(i) = n.as_i64()
            {
                Value::Integer {
                    value: i as i128,
                    kind: crate::ast::IntKind::I64,
                }
            }
            else if let Some(u) = n.as_u64()
            {
                Value::Unsigned {
                    value: u as u128,
                    kind: crate::ast::IntKind::U64,
                }
            }
            else
            {
                Value::Float {
                    value: n.as_f64().unwrap_or(0.0),
                    kind: crate::ast::FloatKind::F64,
                }
            }
        }
        JsonValue::String(s) => Value::String(intern::intern_owned(s)),
        JsonValue::Array(arr) =>
        {
            let vals = arr.into_iter().map(json_to_value).collect::<Vec<_>>();
            Value::Array(Rc::new(RefCell::new(vals)))
        }
        JsonValue::Object(map) =>
        {
            let mut out = FxHashMap::default();
            for (k, v) in map
            {
                out.insert(intern::intern_owned(k), json_to_value(v));
            }
            Value::Map(Rc::new(RefCell::new(MapValue::new(out))))
        }
    }
}

pub(crate) fn value_to_json(value: &Value) -> JsonValue
{
    match value
    {
        Value::Nil => JsonValue::Null,
        Value::Boolean(b) => JsonValue::Bool(*b),
        Value::Integer { value, .. } => JsonValue::Number(serde_json::Number::from(*value as i64)),
        Value::Unsigned { value, .. } => JsonValue::Number(serde_json::Number::from(*value as u64)),
        Value::Float { value, .. } => serde_json::Number::from_f64(*value)
            .map(JsonValue::Number)
            .unwrap_or(JsonValue::Null),
        Value::String(s) => JsonValue::String(s.as_str().to_string()),
        Value::Array(arr) =>
        {
            let vals = arr.borrow().iter().map(value_to_json).collect::<Vec<_>>();
            JsonValue::Array(vals)
        }
        Value::F64Array(arr) =>
        {
            let vals = arr
                .borrow()
                .iter()
                .map(|v| {
                    JsonValue::Number(
                        serde_json::Number::from_f64(*v)
                            .unwrap_or_else(|| serde_json::Number::from(0)),
                    )
                })
                .collect();
            JsonValue::Array(vals)
        }
        Value::Map(map) =>
        {
            let mut out = serde_json::Map::new();
            for (k, v) in map.borrow().data.iter()
            {
                out.insert(k.as_str().to_string(), value_to_json(v));
            }
            JsonValue::Object(out)
        }
        _ => JsonValue::Null,
    }
}

fn native_serde_parse(args: &[Value]) -> Result<Value, String>
{
    match args.get(0)
    {
        Some(Value::String(s)) =>
        {
            let json: JsonValue =
                serde_json::from_str(s.as_str()).map_err(|e| format!("Serde.parse failed: {e}"))?;
            Ok(json_to_value(json))
        }
        _ => Err("Serde.parse expects a JSON string".to_string()),
    }
}

fn native_serde_stringify(args: &[Value]) -> Result<Value, String>
{
    let value = args
        .get(0)
        .ok_or_else(|| "Serde.stringify expects a value".to_string())?;
    let json = value_to_json(value);
    let out = serde_json::to_string(&json).map_err(|e| format!("Serde.stringify failed: {e}"))?;
    Ok(Value::String(intern::intern_owned(out)))
}

pub fn build_serde_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_serde_parse));
    map.insert(intern::intern("stringify"), Value::NativeFunction(native_serde_stringify));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
