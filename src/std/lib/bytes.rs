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
        Value::String(s) => Some(s.as_bytes().len()),
        Value::Bytes(bytes) => Some(bytes.len()),
        Value::ByteBuf(buf) => Some(buf.borrow().len()),
        Value::BytesView(view) => Some(view.len),
        Value::Mmap(mmap) => Some(mmap.len()),
        Value::MmapMut(mmap) => Some(mmap.borrow().len()),
        _ => None,
    }
}

fn bytes_to_vec(value: &Value, name: &str) -> Result<Vec<u8>, String>
{
    match value
    {
        Value::String(s) => Ok(s.as_bytes().to_vec()),
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
        Value::Mmap(mmap) => Ok(mmap.as_ref().to_vec()),
        Value::MmapMut(mmap) => Ok(mmap.borrow().as_ref().to_vec()),
        _ => Err(format!("{name} expects Bytes, ByteBuf, Mmap, or String")),
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
        Value::BytesView(view) =>
        {
            if start > view.len || end > view.len
            {
                return Err("Bytes.slice out of bounds".to_string());
            }
            let abs_start = view.offset + start;
            let abs_end = view.offset + end;
            match &view.source
            {
                crate::value::BytesViewSource::Mmap(mmap) =>
                {
                    Ok(mmap[abs_start..abs_end].to_vec())
                }
                crate::value::BytesViewSource::MmapMut(mmap) =>
                {
                    let data = mmap.borrow();
                    Ok(data[abs_start..abs_end].to_vec())
                }
            }
        }
        Value::String(s) =>
        {
            let bytes = s.as_bytes();
            if end > bytes.len()
            {
                return Err("Bytes.slice out of bounds".to_string());
            }
            Ok(bytes[start..end].to_vec())
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
        _ => Err("Bytes.slice expects Bytes, ByteBuf, BytesView, Mmap, or String".to_string()),
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
        None => Err("Bytes.len expects Bytes, ByteBuf, BytesView, Mmap, or String".to_string()),
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

fn native_bytes_slice_view(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Bytes.slice_view expects bytes".to_string())?;
    let start = int_arg(args, 1, "Bytes.slice_view")?;
    let len = int_arg(args, 2, "Bytes.slice_view")?;
    if start < 0 || len < 0
    {
        return Err("Bytes.slice_view expects non-negative start and length".to_string());
    }
    let start = start as usize;
    let len = len as usize;
    match value
    {
        Value::Mmap(mmap) =>
        {
            let end = start.saturating_add(len);
            if end > mmap.len()
            {
                return Err("Bytes.slice_view out of bounds".to_string());
            }
            Ok(Value::BytesView(Rc::new(crate::value::BytesView {
                source: crate::value::BytesViewSource::Mmap(mmap.clone()),
                offset: start,
                len,
            })))
        }
        Value::MmapMut(mmap) =>
        {
            let data = mmap.borrow();
            let end = start.saturating_add(len);
            if end > data.len()
            {
                return Err("Bytes.slice_view out of bounds".to_string());
            }
            Ok(Value::BytesView(Rc::new(crate::value::BytesView {
                source: crate::value::BytesViewSource::MmapMut(mmap.clone()),
                offset: start,
                len,
            })))
        }
        _ => native_bytes_slice(args),
    }
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
        Value::BytesView(view) =>
        {
            if idx < view.len
            {
                let offset = view.offset + idx;
                let byte = match &view.source
                {
                    crate::value::BytesViewSource::Mmap(mmap) => mmap[offset],
                    crate::value::BytesViewSource::MmapMut(mmap) =>
                    {
                        let data = mmap.borrow();
                        data[offset]
                    }
                };
                Ok(Value::Integer { value: byte as i128, kind: crate::ast::IntKind::I64 })
            }
            else
            {
                Ok(Value::Nil)
            }
        }
        Value::String(s) =>
        {
            let bytes = s.as_bytes();
            if idx < bytes.len()
            {
                Ok(Value::Integer { value: bytes[idx] as i128, kind: crate::ast::IntKind::I64 })
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
        _ => Err("Bytes.get expects Bytes, ByteBuf, BytesView, Mmap, or String".to_string()),
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

fn native_bytes_fill(args: &[Value]) -> Result<Value, String>
{
    let value = args.get(0).ok_or_else(|| "Bytes.fill expects ByteBuf".to_string())?;
    let byte = byte_value(args.get(1).ok_or_else(|| "Bytes.fill expects a byte".to_string())?, "Bytes.fill")?;
    match value
    {
        Value::ByteBuf(buf) =>
        {
            let mut data = buf.borrow_mut();
            for b in data.iter_mut()
            {
                *b = byte;
            }
            Ok(value.clone())
        }
        Value::MmapMut(mmap) =>
        {
            let mut data = mmap.borrow_mut();
            for b in data.iter_mut()
            {
                *b = byte;
            }
            Ok(value.clone())
        }
        _ => Err("Bytes.fill expects ByteBuf or MmapMut".to_string()),
    }
}

fn native_bytes_copy(args: &[Value]) -> Result<Value, String>
{
    let dst = args.get(0).ok_or_else(|| "Bytes.copy expects a destination".to_string())?;
    let dst_start = int_arg(args, 1, "Bytes.copy")?;
    let src = args.get(2).ok_or_else(|| "Bytes.copy expects a source".to_string())?;
    let src_start = int_arg(args, 3, "Bytes.copy")?;
    let len = int_arg(args, 4, "Bytes.copy")?;
    if dst_start < 0 || src_start < 0 || len < 0
    {
        return Err("Bytes.copy expects non-negative offsets and length".to_string());
    }
    let src_bytes = bytes_to_vec(src, "Bytes.copy")?;
    let src_start = src_start as usize;
    let len = len as usize;
    let src_end = src_start.saturating_add(len);
    if src_end > src_bytes.len()
    {
        return Err("Bytes.copy source out of bounds".to_string());
    }
    let slice = &src_bytes[src_start..src_end];
    let dst_start = dst_start as usize;
    match dst
    {
        Value::ByteBuf(buf) =>
        {
            let mut data = buf.borrow_mut();
            let dst_end = dst_start.saturating_add(slice.len());
            if dst_end > data.len()
            {
                return Err("Bytes.copy destination out of bounds".to_string());
            }
            data[dst_start..dst_end].copy_from_slice(slice);
            Ok(Value::Boolean(true))
        }
        Value::MmapMut(mmap) =>
        {
            let mut data = mmap.borrow_mut();
            let dst_end = dst_start.saturating_add(slice.len());
            if dst_end > data.len()
            {
                return Err("Bytes.copy destination out of bounds".to_string());
            }
            data[dst_start..dst_end].copy_from_slice(slice);
            Ok(Value::Boolean(true))
        }
        _ => Err("Bytes.copy expects ByteBuf or MmapMut".to_string()),
    }
}

fn native_bytes_find(args: &[Value]) -> Result<Value, String>
{
    let haystack = args.get(0).ok_or_else(|| "Bytes.find expects bytes".to_string())?;
    let needle = args.get(1).ok_or_else(|| "Bytes.find expects a needle".to_string())?;
    let start = match args.get(2)
    {
        Some(_) => int_arg(args, 2, "Bytes.find")?,
        None => 0,
    };
    if start < 0
    {
        return Err("Bytes.find expects non-negative start".to_string());
    }
    let hay_bytes = bytes_to_vec(haystack, "Bytes.find")?;
    let needle_bytes = bytes_to_vec(needle, "Bytes.find")?;
    if needle_bytes.is_empty()
    {
        return Ok(Value::Integer { value: start as i128, kind: crate::ast::IntKind::I64 });
    }
    let start = start as usize;
    if start > hay_bytes.len()
    {
        return Ok(Value::Nil);
    }
    for i in start..=hay_bytes.len().saturating_sub(needle_bytes.len())
    {
        if hay_bytes[i..i + needle_bytes.len()] == needle_bytes[..]
        {
            return Ok(Value::Integer { value: i as i128, kind: crate::ast::IntKind::I64 });
        }
    }
    Ok(Value::Nil)
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
    map.insert(intern::intern("slice_view"), Value::NativeFunction(native_bytes_slice_view));
    map.insert(intern::intern("buf"), Value::NativeFunction(native_bytes_buf));
    map.insert(intern::intern("get"), Value::NativeFunction(native_bytes_get));
    map.insert(intern::intern("set"), Value::NativeFunction(native_bytes_set));
    map.insert(intern::intern("push"), Value::NativeFunction(native_bytes_push));
    map.insert(intern::intern("fill"), Value::NativeFunction(native_bytes_fill));
    map.insert(intern::intern("copy"), Value::NativeFunction(native_bytes_copy));
    map.insert(intern::intern("find"), Value::NativeFunction(native_bytes_find));
    map.insert(intern::intern("freeze"), Value::NativeFunction(native_bytes_freeze));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
