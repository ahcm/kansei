use crate::intern;
use super::LibMap;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
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

fn response_map(status: i64, body: String) -> Value
{
    let mut map = FxHashMap::default();
    map.insert(
        intern::intern("status"),
        Value::Integer {
            value: status as i128,
            kind: crate::ast::IntKind::I64,
        },
    );
    map.insert(intern::intern("body"), Value::String(intern::intern_owned(body)));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn native_http_get(args: &[Value]) -> Result<Value, String>
{
    let url = str_arg(args, 0, "Http.get")?;
    let resp = ureq::get(&url)
        .call()
        .map_err(|e| format!("Http.get failed: {e}"))?;
    let status = resp.status() as i64;
    let body = resp.into_string().unwrap_or_default();
    Ok(response_map(status, body))
}

fn native_http_post(args: &[Value]) -> Result<Value, String>
{
    let url = str_arg(args, 0, "Http.post")?;
    let body = str_arg(args, 1, "Http.post")?;
    let resp = ureq::post(&url)
        .send_string(&body)
        .map_err(|e| format!("Http.post failed: {e}"))?;
    let status = resp.status() as i64;
    let body = resp.into_string().unwrap_or_default();
    Ok(response_map(status, body))
}

pub fn build_http_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("get"), Value::NativeFunction(native_http_get));
    map.insert(intern::intern("post"), Value::NativeFunction(native_http_post));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("Http"), build_http_module());
}
