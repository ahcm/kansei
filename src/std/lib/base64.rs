use crate::intern;
use crate::value::{MapValue, Value};
use base64::engine::general_purpose::STANDARD as BASE64_STD;
use base64::Engine;
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

fn bytes_from_array(value: &Value, name: &str) -> Result<Vec<u8>, String>
{
    match value
    {
        Value::Array(arr) =>
        {
            let mut out = Vec::new();
            for v in arr.borrow().iter()
            {
                let num = match v
                {
                    Value::Integer { value, .. } => *value as i64,
                    Value::Unsigned { value, .. } => *value as i64,
                    _ => return Err(format!("{name} expects array of integers")),
                };
                if num < 0 || num > 255
                {
                    return Err(format!("{name} expects bytes (0-255)"));
                }
                out.push(num as u8);
            }
            Ok(out)
        }
        _ => Err(format!("{name} expects array of integers")),
    }
}

fn native_base64_encode(args: &[Value]) -> Result<Value, String>
{
    let text = str_arg(args, 0, "Base64.encode")?;
    Ok(Value::String(intern::intern_owned(BASE64_STD.encode(text.as_bytes()))))
}

fn native_base64_encode_bytes(args: &[Value]) -> Result<Value, String>
{
    let bytes = bytes_from_array(args.get(0).ok_or_else(|| "Base64.encode_bytes expects bytes".to_string())?, "Base64.encode_bytes")?;
    Ok(Value::String(intern::intern_owned(BASE64_STD.encode(bytes))))
}

fn native_base64_decode(args: &[Value]) -> Result<Value, String>
{
    let text = str_arg(args, 0, "Base64.decode")?;
    let bytes = BASE64_STD
        .decode(text.as_bytes())
        .map_err(|e| format!("Base64.decode failed: {e}"))?;
    match String::from_utf8(bytes)
    {
        Ok(s) => Ok(Value::String(intern::intern_owned(s))),
        Err(_) => Ok(Value::Nil),
    }
}

fn native_base64_decode_bytes(args: &[Value]) -> Result<Value, String>
{
    let text = str_arg(args, 0, "Base64.decode_bytes")?;
    let bytes = BASE64_STD
        .decode(text.as_bytes())
        .map_err(|e| format!("Base64.decode_bytes failed: {e}"))?;
    let vals = bytes
        .into_iter()
        .map(|b| Value::Integer { value: b as i128, kind: crate::ast::IntKind::I64 })
        .collect::<Vec<_>>();
    Ok(Value::Array(Rc::new(RefCell::new(vals))))
}

pub fn build_base64_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("encode"), Value::NativeFunction(native_base64_encode));
    map.insert(intern::intern("encode_bytes"), Value::NativeFunction(native_base64_encode_bytes));
    map.insert(intern::intern("decode"), Value::NativeFunction(native_base64_decode));
    map.insert(intern::intern("decode_bytes"), Value::NativeFunction(native_base64_decode_bytes));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
