use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::fs::OpenOptions;
use std::path::Path;
use std::rc::Rc;

fn io_path_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a string path")),
    }
}

fn io_content_arg(args: &[Value], idx: usize) -> Result<String, String>
{
    Ok(args.get(idx).cloned().unwrap_or(Value::Nil).to_string())
}

fn io_bytes_arg(args: &[Value], idx: usize, name: &str) -> Result<Vec<u8>, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_bytes().to_vec()),
        Some(Value::Bytes(bytes)) => Ok(bytes.as_ref().clone()),
        Some(Value::ByteBuf(buf)) => Ok(buf.borrow().clone()),
        Some(Value::BytesView(view)) =>
        {
            let end = view.offset.saturating_add(view.len);
            match &view.source
            {
                crate::value::BytesViewSource::Mmap(mmap) => Ok(mmap[view.offset..end].to_vec()),
                crate::value::BytesViewSource::MmapMut(mmap) =>
                {
                    let data = mmap.borrow();
                    Ok(data[view.offset..end].to_vec())
                }
            }
        }
        _ => Err(format!("{name} expects bytes")),
    }
}

fn native_io_read(args: &[Value]) -> Result<Value, String>
{
    let path = io_path_arg(args, 0, "IO.read")?;
    match fs::read_to_string(&path)
    {
        Ok(content) => Ok(Value::String(intern::intern_owned(content))),
        Err(_) => Ok(Value::Nil),
    }
}

fn native_io_write(args: &[Value]) -> Result<Value, String>
{
    let path = io_path_arg(args, 0, "IO.write")?;
    let content = io_content_arg(args, 1)?;
    match fs::write(&path, content)
    {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_io_append(args: &[Value]) -> Result<Value, String>
{
    let path = io_path_arg(args, 0, "IO.append")?;
    let content = io_content_arg(args, 1)?;
    let result = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
        .and_then(|mut file| {
            use std::io::Write as _;
            file.write_all(content.as_bytes())
        });
    match result
    {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_io_read_bytes(args: &[Value]) -> Result<Value, String>
{
    let path = io_path_arg(args, 0, "IO.read_bytes")?;
    match fs::read(&path)
    {
        Ok(content) => Ok(Value::Bytes(Rc::new(content))),
        Err(_) => Ok(Value::Nil),
    }
}

fn native_io_write_bytes(args: &[Value]) -> Result<Value, String>
{
    let path = io_path_arg(args, 0, "IO.write_bytes")?;
    let content = io_bytes_arg(args, 1, "IO.write_bytes")?;
    match fs::write(&path, content)
    {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_io_append_bytes(args: &[Value]) -> Result<Value, String>
{
    let path = io_path_arg(args, 0, "IO.append_bytes")?;
    let content = io_bytes_arg(args, 1, "IO.append_bytes")?;
    let result = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
        .and_then(|mut file| {
            use std::io::Write as _;
            file.write_all(&content)
        });
    match result
    {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_io_exists(args: &[Value]) -> Result<Value, String>
{
    let path = io_path_arg(args, 0, "IO.exists")?;
    Ok(Value::Boolean(Path::new(&path).exists()))
}

fn native_io_remove(args: &[Value]) -> Result<Value, String>
{
    let path = io_path_arg(args, 0, "IO.remove")?;
    match fs::remove_file(&path)
    {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_io_mkdirs(args: &[Value]) -> Result<Value, String>
{
    let path = io_path_arg(args, 0, "IO.mkdirs")?;
    match fs::create_dir_all(&path)
    {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_io_cwd(_args: &[Value]) -> Result<Value, String>
{
    match env::current_dir()
    {
        Ok(path) => Ok(Value::String(intern::intern_owned(path.to_string_lossy().to_string()))),
        Err(_) => Ok(Value::Nil),
    }
}

pub fn build_io_module() -> Value
{
    let mut io_map = FxHashMap::default();
    io_map.insert(intern::intern("read"), Value::NativeFunction(native_io_read));
    io_map.insert(intern::intern("write"), Value::NativeFunction(native_io_write));
    io_map.insert(intern::intern("append"), Value::NativeFunction(native_io_append));
    io_map.insert(intern::intern("read_bytes"), Value::NativeFunction(native_io_read_bytes));
    io_map.insert(intern::intern("write_bytes"), Value::NativeFunction(native_io_write_bytes));
    io_map.insert(intern::intern("append_bytes"), Value::NativeFunction(native_io_append_bytes));
    io_map.insert(intern::intern("exists"), Value::NativeFunction(native_io_exists));
    io_map.insert(intern::intern("remove"), Value::NativeFunction(native_io_remove));
    io_map.insert(intern::intern("mkdirs"), Value::NativeFunction(native_io_mkdirs));
    io_map.insert(intern::intern("cwd"), Value::NativeFunction(native_io_cwd));
    Value::Map(Rc::new(RefCell::new(MapValue::new(io_map))))
}
