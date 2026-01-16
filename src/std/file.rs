use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::fs::OpenOptions;
use std::path::Path;
use std::rc::Rc;

fn file_path_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a string path")),
    }
}

fn file_content_arg(args: &[Value], idx: usize) -> Result<String, String>
{
    Ok(args.get(idx).cloned().unwrap_or(Value::Nil).to_string())
}

fn file_bytes_arg(args: &[Value], idx: usize, name: &str) -> Result<Vec<u8>, String>
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

fn native_file_read(args: &[Value]) -> Result<Value, String>
{
    let path = file_path_arg(args, 0, "File.read")?;
    match fs::read_to_string(&path)
    {
        Ok(content) => Ok(Value::String(intern::intern_owned(content))),
        Err(_) => Ok(Value::Nil),
    }
}

fn native_file_write(args: &[Value]) -> Result<Value, String>
{
    let path = file_path_arg(args, 0, "File.write")?;
    let content = file_content_arg(args, 1)?;
    match fs::write(&path, content)
    {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_file_append(args: &[Value]) -> Result<Value, String>
{
    let path = file_path_arg(args, 0, "File.append")?;
    let content = file_content_arg(args, 1)?;
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

fn native_file_read_bytes(args: &[Value]) -> Result<Value, String>
{
    let path = file_path_arg(args, 0, "File.read_bytes")?;
    match fs::read(&path)
    {
        Ok(content) => Ok(Value::Bytes(Rc::new(content))),
        Err(_) => Ok(Value::Nil),
    }
}

fn native_file_write_bytes(args: &[Value]) -> Result<Value, String>
{
    let path = file_path_arg(args, 0, "File.write_bytes")?;
    let content = file_bytes_arg(args, 1, "File.write_bytes")?;
    match fs::write(&path, content)
    {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_file_append_bytes(args: &[Value]) -> Result<Value, String>
{
    let path = file_path_arg(args, 0, "File.append_bytes")?;
    let content = file_bytes_arg(args, 1, "File.append_bytes")?;
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

fn native_file_exists(args: &[Value]) -> Result<Value, String>
{
    let path = file_path_arg(args, 0, "File.exists")?;
    Ok(Value::Boolean(Path::new(&path).exists()))
}

fn native_file_remove(args: &[Value]) -> Result<Value, String>
{
    let path = file_path_arg(args, 0, "File.remove")?;
    match fs::remove_file(&path)
    {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_file_mkdirs(args: &[Value]) -> Result<Value, String>
{
    let path = file_path_arg(args, 0, "File.mkdirs")?;
    match fs::create_dir_all(&path)
    {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_file_copy(args: &[Value]) -> Result<Value, String>
{
    let src = file_path_arg(args, 0, "File.copy")?;
    let dst = file_path_arg(args, 1, "File.copy")?;
    match fs::copy(&src, &dst)
    {
        Ok(_) => Ok(Value::Boolean(true)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_file_cwd(_args: &[Value]) -> Result<Value, String>
{
    match env::current_dir()
    {
        Ok(path) => Ok(Value::String(intern::intern_owned(path.to_string_lossy().to_string()))),
        Err(_) => Ok(Value::Nil),
    }
}

pub fn build_file_module() -> Value
{
    let mut file_map = FxHashMap::default();
    file_map.insert(intern::intern("read"), Value::NativeFunction(native_file_read));
    file_map.insert(intern::intern("write"), Value::NativeFunction(native_file_write));
    file_map.insert(intern::intern("append"), Value::NativeFunction(native_file_append));
    file_map.insert(intern::intern("read_bytes"), Value::NativeFunction(native_file_read_bytes));
    file_map.insert(intern::intern("write_bytes"), Value::NativeFunction(native_file_write_bytes));
    file_map.insert(intern::intern("append_bytes"), Value::NativeFunction(native_file_append_bytes));
    file_map.insert(intern::intern("exists"), Value::NativeFunction(native_file_exists));
    file_map.insert(intern::intern("remove"), Value::NativeFunction(native_file_remove));
    file_map.insert(intern::intern("mkdirs"), Value::NativeFunction(native_file_mkdirs));
    file_map.insert(intern::intern("copy"), Value::NativeFunction(native_file_copy));
    file_map.insert(intern::intern("cwd"), Value::NativeFunction(native_file_cwd));
    Value::Map(Rc::new(RefCell::new(MapValue::new(file_map))))
}
