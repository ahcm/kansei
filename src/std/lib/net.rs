use super::LibMap;
use crate::ast::TypeRef;
use crate::intern;
use crate::value::{MapValue, StructField, StructInstance, StructType, Value, NetStream};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::io::{Read, Write};
use std::net::{Shutdown, TcpStream, ToSocketAddrs};
use std::rc::Rc;
use std::sync::Arc;
use std::time::Duration;

use rustls::{ClientConfig, ClientConnection, StreamOwned};
use rustls::pki_types::ServerName;
use webpki_roots::TLS_SERVER_ROOTS;

thread_local! {
    static NET_STREAM_TYPE: RefCell<Option<Rc<StructType>>> = RefCell::new(None);
}

fn any_type_ref() -> TypeRef
{
    TypeRef {
        path: vec![intern::intern_symbol("Any")],
    }
}

fn net_stream_type() -> Result<Rc<StructType>, String>
{
    NET_STREAM_TYPE.with(|cell| {
        cell.borrow()
            .clone()
            .ok_or_else(|| "NetStream type not initialized".to_string())
    })
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

fn string_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        Some(v) => Ok(v.to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn bytes_arg(args: &[Value], idx: usize, name: &str) -> Result<Vec<u8>, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_bytes().to_vec()),
        Some(Value::Bytes(b)) => Ok(b.as_ref().clone()),
        Some(Value::ByteBuf(b)) => Ok(b.borrow().clone()),
        Some(Value::BytesView(view)) =>
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
        _ => Err(format!("{name} expects bytes or string")),
    }
}

fn stream_from_value(value: &Value, name: &str) -> Result<Rc<RefCell<NetStream>>, String>
{
    match value
    {
        Value::StructInstance(inst) =>
        {
            let fields = inst.fields.borrow();
            match fields.get(0)
            {
                Some(Value::NetStream(stream)) => Ok(stream.clone()),
                _ => Err(format!("{name} missing NetStream handle")),
            }
        }
        _ => Err(format!("{name} expects a NetStream instance")),
    }
}

trait ReadWrite: Read + Write {}
impl<T: Read + Write> ReadWrite for T {}

fn with_stream_mut<T, F>(stream: &Rc<RefCell<NetStream>>, f: F) -> Result<T, String>
where
    F: FnOnce(&mut dyn ReadWrite) -> Result<T, String>,
{
    let mut stream_ref = stream.borrow_mut();
    match &mut *stream_ref
    {
        NetStream::Tcp(stream) => f(stream),
        NetStream::Tls(stream) => f(stream),
    }
}

fn with_tcp_mut<T, F>(stream: &Rc<RefCell<NetStream>>, f: F) -> Result<T, String>
where
    F: FnOnce(&mut TcpStream) -> Result<T, String>,
{
    let mut stream_ref = stream.borrow_mut();
    match &mut *stream_ref
    {
        NetStream::Tcp(stream) => f(stream),
        NetStream::Tls(stream) => f(stream.get_mut()),
    }
}

fn net_connect(args: &[Value]) -> Result<Value, String>
{
    let host = string_arg(args, 0, "Net.connect")?;
    let port = int_arg(args, 1, "Net.connect")?;
    let use_tls = match args.get(2)
    {
        Some(Value::Boolean(b)) => *b,
        Some(Value::Nil) | None => false,
        _ => return Err("Net.connect expects tls to be a boolean".to_string()),
    };

    let addr = (host.as_str(), port as u16)
        .to_socket_addrs()
        .map_err(|e| format!("Net.connect resolve failed: {e}"))?
        .next()
        .ok_or_else(|| "Net.connect failed to resolve address".to_string())?;

    let tcp = TcpStream::connect(addr).map_err(|e| format!("Net.connect failed: {e}"))?;
    tcp.set_nodelay(true).ok();

    let stream = if use_tls
    {
        let server_name = ServerName::try_from(host.clone())
            .map_err(|_| "Net.connect invalid TLS host".to_string())?;
        let mut root_store = rustls::RootCertStore::empty();
        root_store.extend(TLS_SERVER_ROOTS.iter().cloned());
        let config = ClientConfig::builder()
            .with_root_certificates(root_store)
            .with_no_client_auth();
        let conn = ClientConnection::new(Arc::new(config), server_name)
            .map_err(|e| format!("Net.connect TLS failed: {e}"))?;
        NetStream::Tls(StreamOwned::new(conn, tcp))
    }
    else
    {
        NetStream::Tcp(tcp)
    };

    let ty = net_stream_type()?;
    let inst = StructInstance {
        ty,
        fields: RefCell::new(vec![Value::NetStream(Rc::new(RefCell::new(stream)))]),
    };
    Ok(Value::StructInstance(Rc::new(inst)))
}

fn net_read(args: &[Value]) -> Result<Value, String>
{
    let stream = stream_from_value(
        args.get(0).ok_or_else(|| "NetStream.read expects a receiver".to_string())?,
        "NetStream.read",
    )?;
    let n = int_arg(args, 1, "NetStream.read")?;
    if n < 0
    {
        return Err("NetStream.read expects n >= 0".to_string());
    }
    let mut buf = vec![0u8; n as usize];
    let read = with_stream_mut(&stream, |s| {
        s.read(&mut buf)
            .map_err(|e| format!("NetStream.read failed: {e}"))
    })?;
    buf.truncate(read);
    Ok(Value::Bytes(Rc::new(buf)))
}

fn net_read_line(args: &[Value]) -> Result<Value, String>
{
    let stream = stream_from_value(
        args.get(0).ok_or_else(|| "NetStream.read_line expects a receiver".to_string())?,
        "NetStream.read_line",
    )?;
    let max = match args.get(1)
    {
        Some(Value::Nil) | None => 8192,
        Some(_) => int_arg(args, 1, "NetStream.read_line")? as usize,
    };
    let mut buf = Vec::new();
    let mut byte = [0u8; 1];
    while buf.len() < max
    {
        let read = with_stream_mut(&stream, |s| {
            s.read(&mut byte)
                .map_err(|e| format!("NetStream.read_line failed: {e}"))
        })?;
        if read == 0
        {
            break;
        }
        buf.push(byte[0]);
        if byte[0] == b'\n'
        {
            break;
        }
    }
    Ok(Value::Bytes(Rc::new(buf)))
}

fn net_write(args: &[Value]) -> Result<Value, String>
{
    let stream = stream_from_value(
        args.get(0).ok_or_else(|| "NetStream.write expects a receiver".to_string())?,
        "NetStream.write",
    )?;
    let data = bytes_arg(args, 1, "NetStream.write")?;
    let written = with_stream_mut(&stream, |s| {
        s.write(&data)
            .map_err(|e| format!("NetStream.write failed: {e}"))
    })?;
    Ok(Value::Integer {
        value: written as i128,
        kind: crate::ast::IntKind::I64,
    })
}

fn net_flush(args: &[Value]) -> Result<Value, String>
{
    let stream = stream_from_value(
        args.get(0).ok_or_else(|| "NetStream.flush expects a receiver".to_string())?,
        "NetStream.flush",
    )?;
    with_stream_mut(&stream, |s| {
        s.flush()
            .map_err(|e| format!("NetStream.flush failed: {e}"))
    })?;
    Ok(Value::Nil)
}

fn net_close(args: &[Value]) -> Result<Value, String>
{
    let stream = stream_from_value(
        args.get(0).ok_or_else(|| "NetStream.close expects a receiver".to_string())?,
        "NetStream.close",
    )?;
    with_tcp_mut(&stream, |s| {
        s.shutdown(Shutdown::Both)
            .map_err(|e| format!("NetStream.close failed: {e}"))
    })?;
    Ok(Value::Nil)
}

fn net_set_timeout(args: &[Value]) -> Result<Value, String>
{
    let stream = stream_from_value(
        args.get(0).ok_or_else(|| "NetStream.set_timeout expects a receiver".to_string())?,
        "NetStream.set_timeout",
    )?;
    let read_ms = match args.get(1)
    {
        Some(Value::Nil) => None,
        Some(_) => Some(int_arg(args, 1, "NetStream.set_timeout")?),
        None => None,
    };
    let write_ms = match args.get(2)
    {
        Some(Value::Nil) => None,
        Some(_) => Some(int_arg(args, 2, "NetStream.set_timeout")?),
        None => None,
    };
    let read_timeout = read_ms.map(|ms| Duration::from_millis(ms as u64));
    let write_timeout = write_ms.map(|ms| Duration::from_millis(ms as u64));
    with_tcp_mut(&stream, |s| {
        s.set_read_timeout(read_timeout)
            .map_err(|e| format!("NetStream.set_timeout failed: {e}"))?;
        s.set_write_timeout(write_timeout)
            .map_err(|e| format!("NetStream.set_timeout failed: {e}"))?;
        Ok(())
    })?;
    Ok(Value::Nil)
}

pub fn build_net_module() -> Value
{
    let mut methods = FxHashMap::default();
    methods.insert(intern::intern("read"), Value::NativeFunction(net_read));
    methods.insert(intern::intern("read_line"), Value::NativeFunction(net_read_line));
    methods.insert(intern::intern("write"), Value::NativeFunction(net_write));
    methods.insert(intern::intern("flush"), Value::NativeFunction(net_flush));
    methods.insert(intern::intern("close"), Value::NativeFunction(net_close));
    methods.insert(
        intern::intern("set_timeout"),
        Value::NativeFunction(net_set_timeout),
    );

    let ty = Rc::new(StructType {
        name: intern::intern("NetStream"),
        fields: vec![StructField {
            name: intern::intern("_handle"),
            type_ref: any_type_ref(),
        }],
        field_map: {
            let mut map = FxHashMap::default();
            map.insert(intern::intern("_handle"), 0);
            map
        },
        methods: RefCell::new(methods),
    });
    NET_STREAM_TYPE.with(|cell| {
        *cell.borrow_mut() = Some(ty.clone());
    });

    let mut map = FxHashMap::default();
    map.insert(intern::intern("NetStream"), Value::StructType(ty));
    map.insert(intern::intern("connect"), Value::NativeFunction(net_connect));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("Net"), build_net_module());
}
