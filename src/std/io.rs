use crate::intern;
use crate::value::{MapValue, Value};
use glob::glob;
use notify::{RecursiveMode, Watcher};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::fs::OpenOptions;
use std::path::Path;
use std::rc::Rc;
use std::sync::mpsc;
use std::time::{Duration, Instant};

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

fn native_io_read_lines(args: &[Value]) -> Result<Value, String>
{
    let path = io_path_arg(args, 0, "IO.read_lines")?;
    match fs::read_to_string(&path)
    {
        Ok(content) =>
        {
            let lines = content
                .lines()
                .map(|line| Value::String(intern::intern_owned(line.to_string())))
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(lines))))
        }
        Err(_) => Ok(Value::Nil),
    }
}

fn native_io_write_lines(args: &[Value]) -> Result<Value, String>
{
    let path = io_path_arg(args, 0, "IO.write_lines")?;
    let lines_val = args.get(1).cloned().unwrap_or(Value::Nil);
    let joined = match lines_val
    {
        Value::Array(arr) =>
        {
            arr.borrow()
                .iter()
                .map(|val| val.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        }
        _ => return Err("IO.write_lines expects an array".to_string()),
    };
    match fs::write(&path, joined)
    {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_io_glob(args: &[Value]) -> Result<Value, String>
{
    let pattern = io_path_arg(args, 0, "IO.glob")?;
    let mut results = Vec::new();
    let entries = glob(&pattern).map_err(|e| format!("IO.glob invalid pattern: {e}"))?;
    for entry in entries
    {
        if let Ok(path) = entry
        {
            results.push(Value::String(intern::intern_owned(
                path.to_string_lossy().to_string(),
            )));
        }
    }
    Ok(Value::Array(Rc::new(RefCell::new(results))))
}

fn native_io_walk(args: &[Value]) -> Result<Value, String>
{
    let root = io_path_arg(args, 0, "IO.walk")?;
    let mut results = Vec::new();
    fn walk_dir(path: &Path, out: &mut Vec<Value>)
    {
        if let Ok(entries) = fs::read_dir(path)
        {
            for entry in entries.flatten()
            {
                let entry_path = entry.path();
                out.push(Value::String(intern::intern_owned(
                    entry_path.to_string_lossy().to_string(),
                )));
                if entry_path.is_dir()
                {
                    walk_dir(&entry_path, out);
                }
            }
        }
    }
    let root_path = Path::new(&root);
    if root_path.exists()
    {
        walk_dir(root_path, &mut results);
    }
    Ok(Value::Array(Rc::new(RefCell::new(results))))
}

fn native_io_watch(args: &[Value]) -> Result<Value, String>
{
    let path = io_path_arg(args, 0, "IO.watch")?;
    let timeout_ms = match args.get(1)
    {
        None | Some(Value::Nil) => 1000u64,
        Some(Value::Integer { value, .. }) => *value as u64,
        Some(Value::Unsigned { value, .. }) => *value as u64,
        _ => return Err("IO.watch expects timeout_ms as integer".to_string()),
    };
    let recursive = match args.get(2)
    {
        None | Some(Value::Nil) => true,
        Some(Value::Boolean(val)) => *val,
        _ => return Err("IO.watch expects recursive as boolean".to_string()),
    };

    let (tx, rx) = mpsc::channel();
    let mut watcher = notify::recommended_watcher(move |res| {
        let _ = tx.send(res);
    })
    .map_err(|e| format!("IO.watch failed: {e}"))?;

    let mode = if recursive
    {
        RecursiveMode::Recursive
    }
    else
    {
        RecursiveMode::NonRecursive
    };
    watcher
        .watch(Path::new(&path), mode)
        .map_err(|e| format!("IO.watch failed: {e}"))?;

    let deadline = Instant::now() + Duration::from_millis(timeout_ms);
    let mut results = Vec::new();
    loop
    {
        let now = Instant::now();
        if now >= deadline
        {
            break;
        }
        let remaining = deadline.saturating_duration_since(now);
        match rx.recv_timeout(remaining)
        {
            Ok(Ok(event)) =>
            {
                for path in event.paths
                {
                    results.push(Value::String(intern::intern_owned(
                        path.to_string_lossy().to_string(),
                    )));
                }
            }
            Ok(Err(_)) => {}
            Err(mpsc::RecvTimeoutError::Timeout) => break,
            Err(_) => break,
        }
    }

    Ok(Value::Array(Rc::new(RefCell::new(results))))
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
    io_map.insert(intern::intern("read_lines"), Value::NativeFunction(native_io_read_lines));
    io_map.insert(intern::intern("write_lines"), Value::NativeFunction(native_io_write_lines));
    io_map.insert(intern::intern("glob"), Value::NativeFunction(native_io_glob));
    io_map.insert(intern::intern("walk"), Value::NativeFunction(native_io_walk));
    io_map.insert(intern::intern("watch"), Value::NativeFunction(native_io_watch));
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
