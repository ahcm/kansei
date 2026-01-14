use crate::intern;
use crate::value::{MapValue, Value};
use base64::engine::general_purpose::STANDARD as BASE64_STD;
use base64::Engine;
use flate2::read::{ZlibDecoder, ZlibEncoder};
use flate2::Compression;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::io::Read;
use std::rc::Rc;

fn str_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn native_flate2_compress(args: &[Value]) -> Result<Value, String>
{
    let text = str_arg(args, 0, "Flate2.compress")?;
    let mut encoder = ZlibEncoder::new(text.as_bytes(), Compression::default());
    let mut out = Vec::new();
    encoder.read_to_end(&mut out).map_err(|e| format!("Flate2.compress failed: {e}"))?;
    Ok(Value::String(intern::intern_owned(BASE64_STD.encode(out))))
}

fn native_flate2_decompress(args: &[Value]) -> Result<Value, String>
{
    let text = str_arg(args, 0, "Flate2.decompress")?;
    let bytes = BASE64_STD.decode(text.as_bytes()).map_err(|e| format!("Flate2.decompress failed: {e}"))?;
    let mut decoder = ZlibDecoder::new(bytes.as_slice());
    let mut out = Vec::new();
    decoder.read_to_end(&mut out).map_err(|e| format!("Flate2.decompress failed: {e}"))?;
    let s = String::from_utf8(out).map_err(|e| format!("Flate2.decompress failed: {e}"))?;
    Ok(Value::String(intern::intern_owned(s)))
}

pub fn build_flate2_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("compress"), Value::NativeFunction(native_flate2_compress));
    map.insert(intern::intern("decompress"), Value::NativeFunction(native_flate2_decompress));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
