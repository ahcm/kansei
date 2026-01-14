use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

fn int_arg(args: &[Value], idx: usize, name: &str) -> Result<i128, String>
{
    match args.get(idx)
    {
        Some(Value::Integer { value, .. }) => Ok(*value),
        Some(Value::Unsigned { value, .. }) => Ok(*value as i128),
        _ => Err(format!("{name} expects an integer")),
    }
}

fn byte_value(value: &Value, name: &str) -> Result<u8, String>
{
    let num = match value
    {
        Value::Integer { value, .. } => *value,
        Value::Unsigned { value, .. } => *value as i128,
        _ => return Err(format!("{name} expects a byte (0-255)")),
    };
    if num < 0 || num > 255
    {
        return Err(format!("{name} expects a byte (0-255)"));
    }
    Ok(num as u8)
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
                out.push(byte_value(v, name)?);
            }
            Ok(out)
        }
        _ => Err(format!("{name} expects an array of bytes")),
    }
}

fn bytes_len(value: &Value) -> Option<usize>
{
    match value
    {
        Value::Bytes(bytes) => Some(bytes.len()),
        Value::ByteBuf(buf) => Some(buf.borrow().len()),
        Value::Mmap(mmap) => Some(mmap.len()),
        Value::MmapMut(mmap) => Some(mmap.borrow().len()),
        _ => None,
    }
}

fn bytes_to_vec(value: &Value, name: &str) -> Result<Vec<u8>, String>
{
    match value
    {
        Value::Bytes(bytes) => Ok(bytes.as_ref().clone()),
        Value::ByteBuf(buf) => Ok(buf.borrow().clone()),
        Value::Mmap(mmap) => Ok(mmap.as_ref().to_vec()),
        Value::MmapMut(mmap) => Ok(mmap.borrow().as_ref().to_vec()),
        _ => Err(format!("{name} expects Bytes, ByteBuf, or Mmap")),
    }
}

fn bytes_slice(value: &Value, start: usize, len: usize) -> Result<Vec<u8>, String>
{
    let end = start.saturating_add(len);
    match value
    {
        Value::Bytes(bytes) =>
        {
            if end > bytes.len()
            {
                return Err("Bytes.slice out of bounds".to_string());
            }
            Ok(bytes[start..end].to_vec())
        }
        Value::ByteBuf(buf) =>
        {
            let data = buf.borrow();
            if end > data.len()
            {
                return Err("Bytes.slice out of bounds".to_string());
            }
            Ok(data[start..end].to_vec())
        }
        Value::Mmap(mmap) =>
        {
            if end > mmap.len()
            {
                return Err("Bytes.slice out of bounds".to_string());
            }
            Ok(mmap[start..end].to_vec())
        }
        Value::MmapMut(mmap) =>
        {
            let data = mmap.borrow();
            if end > data.len()
            {
                return Err("Bytes.slice out of bounds".to_string());
            }
            Ok(data[start..end].to_vec())
        }
        _ => Err("Bytes.slice expects Bytes, ByteBuf, or Mmap".to_string()),
    }
}

fn native_bytes_from_string(args: &[Value]) -> Result<Value, String>
{
    match args.get(0)
    {
        Some(Value::String(s)) =>
        {
            Ok(Value::Bytes(Rc::new(s.as_bytes().to_vec())))
        }
        _ => Err("Bytes.from_string expects a string".to_string()),
    }
}

fn native_bytes_to_string(args: &[Value]) -> Result<Value, String>
{
    let bytes = bytes_to_vec(args.get(0).ok_or_else(|| "Bytes.to_string expects bytes".to_string())?, "Bytes.to_string")?;
    match String::from_utf8(bytes)
    {
        Ok(s) => Ok(Value::String(intern::intern_owned(s))),
        Err(_) => Ok(Value::Nil),
    }
}

fn native_bytes_from_array(args: &[Value]) -> Result<Value, String>
{
    let arr = args.get(0).ok_or_else(|| "Bytes.from_array expects an array".to_string())?;
    let bytes = bytes_from_array(arr, "Bytes.from_array")?;
    Ok(Value::Bytes(Rc::new(bytes)))
}

fn native_bytes_to_array(args: &[Value]) -> Result<Value, String>
{
    let bytes = bytes_to_vec(args.get(0).ok_or_else(|| "Bytes.to_array expects bytes".to_string())?, "Bytes.to_array")?;
    let vals = bytes
        .into_iter()
        .map(|b| Value::Integer { value: b as i128, kind: crate::ast::IntKind::I64 })
        .collect::<Vec<_>>();
    Ok(Value::Array(Rc::new(RefCell::new(vals))))
}

fn native_bytes_len(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Bytes.len expects bytes".to_string())?;
    match bytes_len(value)
    {
        Some(len) => Ok(Value::Integer { value: len as i128, kind: crate::ast::IntKind::I64 }),
        None => Err("Bytes.len expects Bytes, ByteBuf, or Mmap".to_string()),
    }
}

fn native_bytes_slice(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Bytes.slice expects bytes".to_string())?;
    let start = int_arg(args, 1, "Bytes.slice")?;
    let len = int_arg(args, 2, "Bytes.slice")?;
    if start < 0 || len < 0
    {
        return Err("Bytes.slice expects non-negative start and length".to_string());
    }
    let bytes = bytes_slice(value, start as usize, len as usize)?;
    Ok(Value::Bytes(Rc::new(bytes)))
}

fn native_bytes_buf(args: &[Value]) -> Result<Value, String>
{
    let size = int_arg(args, 0, "Bytes.buf")?;
    if size < 0
    {
        return Err("Bytes.buf expects a non-negative size".to_string());
    }
    let fill = match args.get(1)
    {
        Some(v) => byte_value(v, "Bytes.buf")?,
        None => 0,
    };
    Ok(Value::ByteBuf(Rc::new(RefCell::new(vec![fill; size as usize]))))
}

fn native_bytes_get(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Bytes.get expects bytes".to_string())?;
    let idx = int_arg(args, 1, "Bytes.get")?;
    if idx < 0
    {
        return Err("Bytes.get expects a non-negative index".to_string());
    }
    let idx = idx as usize;
    match value
    {
        Value::Bytes(bytes) =>
        {
            if idx < bytes.len()
            {
                Ok(Value::Integer { value: bytes[idx] as i128, kind: crate::ast::IntKind::I64 })
            }
            else
            {
                Ok(Value::Nil)
            }
        }
        Value::ByteBuf(buf) =>
        {
            let data = buf.borrow();
            if idx < data.len()
            {
                Ok(Value::Integer { value: data[idx] as i128, kind: crate::ast::IntKind::I64 })
            }
            else
            {
                Ok(Value::Nil)
            }
        }
        Value::Mmap(mmap) =>
        {
            if idx < mmap.len()
            {
                Ok(Value::Integer { value: mmap[idx] as i128, kind: crate::ast::IntKind::I64 })
            }
            else
            {
                Ok(Value::Nil)
            }
        }
        Value::MmapMut(mmap) =>
        {
            let data = mmap.borrow();
            if idx < data.len()
            {
                Ok(Value::Integer { value: data[idx] as i128, kind: crate::ast::IntKind::I64 })
            }
            else
            {
                Ok(Value::Nil)
            }
        }
        _ => Err("Bytes.get expects Bytes, ByteBuf, or Mmap".to_string()),
    }
}

fn native_bytes_set(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Bytes.set expects ByteBuf or MmapMut".to_string())?;
    let idx = int_arg(args, 1, "Bytes.set")?;
    if idx < 0
    {
        return Err("Bytes.set expects a non-negative index".to_string());
    }
    let idx = idx as usize;
    let byte = byte_value(args.get(2).ok_or_else(|| "Bytes.set expects a byte".to_string())?, "Bytes.set")?;
    match value
    {
        Value::ByteBuf(buf) =>
        {
            let mut data = buf.borrow_mut();
            if idx < data.len()
            {
                data[idx] = byte;
                Ok(Value::Boolean(true))
            }
            else
            {
                Err("Bytes.set index out of bounds".to_string())
            }
        }
        Value::MmapMut(mmap) =>
        {
            let mut data = mmap.borrow_mut();
            if idx < data.len()
            {
                data[idx] = byte;
                Ok(Value::Boolean(true))
            }
            else
            {
                Err("Bytes.set index out of bounds".to_string())
            }
        }
        _ => Err("Bytes.set expects ByteBuf or MmapMut".to_string()),
    }
}

fn native_bytes_push(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Bytes.push expects ByteBuf".to_string())?;
    let byte = byte_value(args.get(1).ok_or_else(|| "Bytes.push expects a byte".to_string())?, "Bytes.push")?;
    match value
    {
        Value::ByteBuf(buf) =>
        {
            buf.borrow_mut().push(byte);
            Ok(value.clone())
        }
        _ => Err("Bytes.push expects ByteBuf".to_string()),
    }
}

fn native_bytes_freeze(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Bytes.freeze expects bytes".to_string())?;
    let bytes = bytes_to_vec(value, "Bytes.freeze")?;
    Ok(Value::Bytes(Rc::new(bytes)))
}

pub fn build_bytes_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("from_string"), Value::NativeFunction(native_bytes_from_string));
    map.insert(intern::intern("to_string"), Value::NativeFunction(native_bytes_to_string));
    map.insert(intern::intern("from_array"), Value::NativeFunction(native_bytes_from_array));
    map.insert(intern::intern("to_array"), Value::NativeFunction(native_bytes_to_array));
    map.insert(intern::intern("len"), Value::NativeFunction(native_bytes_len));
    map.insert(intern::intern("slice"), Value::NativeFunction(native_bytes_slice));
    map.insert(intern::intern("buf"), Value::NativeFunction(native_bytes_buf));
    map.insert(intern::intern("get"), Value::NativeFunction(native_bytes_get));
    map.insert(intern::intern("set"), Value::NativeFunction(native_bytes_set));
    map.insert(intern::intern("push"), Value::NativeFunction(native_bytes_push));
    map.insert(intern::intern("freeze"), Value::NativeFunction(native_bytes_freeze));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
