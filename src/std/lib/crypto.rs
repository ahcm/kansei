use crate::intern;
use super::LibMap;
use crate::value::{MapValue, Value};
use hmac::{Hmac, Mac};
use rand::RngCore;
use rustc_hash::FxHashMap;
use sha2::{Digest, Sha256};
use std::cell::RefCell;
use std::rc::Rc;

fn bytes_arg(args: &[Value], idx: usize, name: &str) -> Result<Vec<u8>, String>
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

fn int_arg(args: &[Value], idx: usize, name: &str) -> Result<usize, String>
{
    match args.get(idx)
    {
        Some(Value::Integer { value, .. }) =>
        {
            if *value < 0
            {
                Err(format!("{name} expects non-negative integer"))
            }
            else
            {
                usize::try_from(*value).map_err(|_| format!("{name} expects non-negative integer"))
            }
        }
        Some(Value::Unsigned { value, .. }) =>
        {
            usize::try_from(*value).map_err(|_| format!("{name} expects non-negative integer"))
        }
        _ => Err(format!("{name} expects an integer")),
    }
}

fn hex_encode(bytes: &[u8]) -> String
{
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut out = String::with_capacity(bytes.len() * 2);
    for b in bytes
    {
        out.push(HEX[(b >> 4) as usize] as char);
        out.push(HEX[(b & 0xF) as usize] as char);
    }
    out
}

fn native_crypto_sha256(args: &[Value]) -> Result<Value, String>
{
    let input = bytes_arg(args, 0, "Crypto.sha256")?;
    let mut hasher = Sha256::new();
    hasher.update(&input);
    let out = hasher.finalize();
    Ok(Value::String(intern::intern_owned(hex_encode(&out))))
}

fn native_crypto_blake3(args: &[Value]) -> Result<Value, String>
{
    let input = bytes_arg(args, 0, "Crypto.blake3")?;
    let out = blake3::hash(&input);
    Ok(Value::String(intern::intern_owned(out.to_hex().to_string())))
}

fn native_crypto_hmac_sha256(args: &[Value]) -> Result<Value, String>
{
    let key = bytes_arg(args, 0, "Crypto.hmac_sha256")?;
    let msg = bytes_arg(args, 1, "Crypto.hmac_sha256")?;
    let mut mac = Hmac::<Sha256>::new_from_slice(&key)
        .map_err(|_| "Crypto.hmac_sha256 invalid key".to_string())?;
    mac.update(&msg);
    let out = mac.finalize().into_bytes();
    Ok(Value::String(intern::intern_owned(hex_encode(&out))))
}

fn native_crypto_random_bytes(args: &[Value]) -> Result<Value, String>
{
    let n = int_arg(args, 0, "Crypto.random_bytes")?;
    let mut buf = vec![0u8; n];
    rand::thread_rng().fill_bytes(&mut buf);
    Ok(Value::String(intern::intern_owned(hex_encode(&buf))))
}

fn native_crypto_random_bytes_buf(args: &[Value]) -> Result<Value, String>
{
    let n = int_arg(args, 0, "Crypto.random_bytes_buf")?;
    let mut buf = vec![0u8; n];
    rand::thread_rng().fill_bytes(&mut buf);
    Ok(Value::Bytes(Rc::new(buf)))
}

pub fn build_crypto_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("sha256"), Value::NativeFunction(native_crypto_sha256));
    map.insert(intern::intern("blake3"), Value::NativeFunction(native_crypto_blake3));
    map.insert(intern::intern("hmac_sha256"), Value::NativeFunction(native_crypto_hmac_sha256));
    map.insert(intern::intern("random_bytes"), Value::NativeFunction(native_crypto_random_bytes));
    map.insert(
        intern::intern("random_bytes_buf"),
        Value::NativeFunction(native_crypto_random_bytes_buf),
    );
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("Crypto"), build_crypto_module());
}
