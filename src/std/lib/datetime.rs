use super::LibMap;
use crate::intern;
use crate::value::{MapValue, Value};
use chrono::{DateTime, TimeZone, Utc};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

fn int_arg(args: &[Value], idx: usize, name: &str) -> Result<i64, String>
{
    match args.get(idx)
    {
        Some(Value::Integer { value, .. }) => Ok(*value as i64),
        Some(Value::Unsigned { value, .. }) => Ok(*value as i64),
        _ => Err(format!("{name} expects an integer")),
    }
}

fn str_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn native_datetime_now_ms(_args: &[Value]) -> Result<Value, String>
{
    let ms = Utc::now().timestamp_millis();
    Ok(Value::Integer {
        value: ms as i128,
        kind: crate::ast::IntKind::I64,
    })
}

fn native_datetime_format(args: &[Value]) -> Result<Value, String>
{
    let ts = int_arg(args, 0, "DateTime.format")?;
    let fmt = str_arg(args, 1, "DateTime.format")?;
    let dt = Utc
        .timestamp_millis_opt(ts)
        .single()
        .ok_or_else(|| "DateTime.format invalid timestamp".to_string())?;
    Ok(Value::String(intern::intern_owned(dt.format(&fmt).to_string())))
}

fn native_datetime_parse(args: &[Value]) -> Result<Value, String>
{
    let fmt = str_arg(args, 0, "DateTime.parse")?;
    let text = str_arg(args, 1, "DateTime.parse")?;
    let naive = chrono::NaiveDateTime::parse_from_str(&text, &fmt)
        .map_err(|e| format!("DateTime.parse failed: {e}"))?;
    let dt: DateTime<Utc> = DateTime::<Utc>::from_naive_utc_and_offset(naive, Utc);
    Ok(Value::Integer {
        value: dt.timestamp_millis() as i128,
        kind: crate::ast::IntKind::I64,
    })
}

fn native_datetime_rfc3339(_args: &[Value]) -> Result<Value, String>
{
    Ok(Value::String(intern::intern_owned(Utc::now().to_rfc3339())))
}

pub fn build_datetime_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("now_ms"), Value::NativeFunction(native_datetime_now_ms));
    map.insert(intern::intern("format"), Value::NativeFunction(native_datetime_format));
    map.insert(intern::intern("parse"), Value::NativeFunction(native_datetime_parse));
    map.insert(intern::intern("rfc3339"), Value::NativeFunction(native_datetime_rfc3339));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("DateTime"), build_datetime_module());
}
