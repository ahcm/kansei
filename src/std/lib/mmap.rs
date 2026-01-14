use crate::intern;
use crate::value::{MapValue, Value};
use memmap2::{Mmap, MmapMut};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fs::OpenOptions;
use std::rc::Rc;

fn str_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn int_arg(args: &[Value], idx: usize, name: &str) -> Result<i128, String>
{
    match args.get(idx)
    {
        Some(Value::Integer { value, .. }) => Ok(*value),
        Some(Value::Unsigned { value, .. }) => Ok(*value as i128),
        _ => Err(format!("{name} expects an integer")),
    }
}

fn bytes_from_value(value: &Value, name: &str) -> Result<Vec<u8>, String>
{
    match value
    {
        Value::Bytes(bytes) => Ok(bytes.as_ref().clone()),
        Value::ByteBuf(buf) => Ok(buf.borrow().clone()),
        Value::BytesView(view) =>
        {
            let end = view.offset.saturating_add(view.len);
            match &view.source
            {
                crate::value::BytesViewSource::Mmap(mmap) =>
                {
                    Ok(mmap[view.offset..end].to_vec())
                }
                crate::value::BytesViewSource::MmapMut(mmap) =>
                {
                    let data = mmap.borrow();
                    Ok(data[view.offset..end].to_vec())
                }
            }
        }
        Value::String(s) => Ok(s.as_bytes().to_vec()),
        _ => Err(format!("{name} expects Bytes, ByteBuf, BytesView, or String")),
    }
}

fn native_mmap_open(args: &[Value]) -> Result<Value, String>
{
    let path = str_arg(args, 0, "Mmap.open")?;
    let mode = match args.get(1)
    {
        Some(Value::String(s)) => s.as_str().to_string(),
        None => "r".to_string(),
        _ => return Err("Mmap.open expects mode string".to_string()),
    };
    match mode.as_str()
    {
        "r" =>
        {
            let file = OpenOptions::new()
                .read(true)
                .open(&path)
                .map_err(|e| format!("Mmap.open failed: {e}"))?;
            let mmap = unsafe { Mmap::map(&file) }
                .map_err(|e| format!("Mmap.open failed: {e}"))?;
            Ok(Value::Mmap(Rc::new(mmap)))
        }
        "rw" =>
        {
            let file = OpenOptions::new()
                .read(true)
                .write(true)
                .open(&path)
                .map_err(|e| format!("Mmap.open failed: {e}"))?;
            let mmap = unsafe { MmapMut::map_mut(&file) }
                .map_err(|e| format!("Mmap.open failed: {e}"))?;
            Ok(Value::MmapMut(Rc::new(RefCell::new(mmap))))
        }
        _ => Err("Mmap.open mode must be \"r\" or \"rw\"".to_string()),
    }
}

fn native_mmap_len(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Mmap.len expects a mapping".to_string())?;
    match value
    {
        Value::Mmap(mmap) => Ok(Value::Integer { value: mmap.len() as i128, kind: crate::ast::IntKind::I64 }),
        Value::MmapMut(mmap) => Ok(Value::Integer { value: mmap.borrow().len() as i128, kind: crate::ast::IntKind::I64 }),
        _ => Err("Mmap.len expects a mapping".to_string()),
    }
}

fn native_mmap_read(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Mmap.read expects a mapping".to_string())?;
    let start = int_arg(args, 1, "Mmap.read")?;
    let len = int_arg(args, 2, "Mmap.read")?;
    if start < 0 || len < 0
    {
        return Err("Mmap.read expects non-negative start and length".to_string());
    }
    let start = start as usize;
    let len = len as usize;
    let end = start.saturating_add(len);
    let bytes = match value
    {
        Value::Mmap(mmap) =>
        {
            if end > mmap.len()
            {
                return Err("Mmap.read out of bounds".to_string());
            }
            mmap[start..end].to_vec()
        }
        Value::MmapMut(mmap) =>
        {
            let data = mmap.borrow();
            if end > data.len()
            {
                return Err("Mmap.read out of bounds".to_string());
            }
            data[start..end].to_vec()
        }
        _ => return Err("Mmap.read expects a mapping".to_string()),
    };
    Ok(Value::Bytes(Rc::new(bytes)))
}

fn native_mmap_slice(args: &[Value]) -> Result<Value, String>
{
    native_mmap_read(args)
}

fn native_mmap_write(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Mmap.write expects a mapping".to_string())?;
    let start = int_arg(args, 1, "Mmap.write")?;
    if start < 0
    {
        return Err("Mmap.write expects a non-negative start".to_string());
    }
    let bytes = bytes_from_value(args.get(2).ok_or_else(|| "Mmap.write expects bytes".to_string())?, "Mmap.write")?;
    let start = start as usize;
    match value
    {
        Value::MmapMut(mmap) =>
        {
            let mut data = mmap.borrow_mut();
            let end = start.saturating_add(bytes.len());
            if end > data.len()
            {
                return Err("Mmap.write out of bounds".to_string());
            }
            data[start..end].copy_from_slice(&bytes);
            Ok(Value::Boolean(true))
        }
        _ => Err("Mmap.write expects a writable mapping".to_string()),
    }
}

fn native_mmap_flush(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Mmap.flush expects a writable mapping".to_string())?;
    match value
    {
        Value::MmapMut(mmap) =>
        {
            mmap.borrow().flush().map_err(|e| format!("Mmap.flush failed: {e}"))?;
            Ok(Value::Boolean(true))
        }
        _ => Err("Mmap.flush expects a writable mapping".to_string()),
    }
}

pub fn build_mmap_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("open"), Value::NativeFunction(native_mmap_open));
    map.insert(intern::intern("len"), Value::NativeFunction(native_mmap_len));
    map.insert(intern::intern("read"), Value::NativeFunction(native_mmap_read));
    map.insert(intern::intern("slice"), Value::NativeFunction(native_mmap_slice));
    map.insert(intern::intern("write"), Value::NativeFunction(native_mmap_write));
    map.insert(intern::intern("flush"), Value::NativeFunction(native_mmap_flush));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
