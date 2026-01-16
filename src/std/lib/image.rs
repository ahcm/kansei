use crate::intern;
use super::LibMap;
use crate::value::{MapValue, Value};
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64_STD;
use image::{ImageBuffer, Rgba};
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

fn int_arg(args: &[Value], idx: usize, name: &str) -> Result<u32, String>
{
    match args.get(idx)
    {
        Some(Value::Integer { value, .. }) =>
        {
            u32::try_from(*value).map_err(|_| format!("{name} expects a non-negative integer"))
        }
        Some(Value::Unsigned { value, .. }) =>
        {
            u32::try_from(*value).map_err(|_| format!("{name} expects a non-negative integer"))
        }
        _ => Err(format!("{name} expects an integer")),
    }
}

fn native_image_load_png(args: &[Value]) -> Result<Value, String>
{
    let path = str_arg(args, 0, "Image.load_png")?;
    let img = image::open(&path).map_err(|e| format!("Image.load_png failed: {e}"))?;
    let rgba = img.to_rgba8();
    let width = rgba.width();
    let height = rgba.height();
    let data = BASE64_STD.encode(rgba.as_raw());
    let mut map = FxHashMap::default();
    map.insert(
        intern::intern("width"),
        Value::Integer {
            value: width as i128,
            kind: crate::ast::IntKind::I64,
        },
    );
    map.insert(
        intern::intern("height"),
        Value::Integer {
            value: height as i128,
            kind: crate::ast::IntKind::I64,
        },
    );
    map.insert(intern::intern("rgba"), Value::String(intern::intern_owned(data)));
    Ok(Value::Map(Rc::new(RefCell::new(MapValue::new(map)))))
}

fn native_image_load_png_bytes(args: &[Value]) -> Result<Value, String>
{
    let path = str_arg(args, 0, "Image.load_png_bytes")?;
    let img = image::open(&path).map_err(|e| format!("Image.load_png_bytes failed: {e}"))?;
    let rgba = img.to_rgba8();
    let width = rgba.width();
    let height = rgba.height();
    let mut map = FxHashMap::default();
    map.insert(
        intern::intern("width"),
        Value::Integer {
            value: width as i128,
            kind: crate::ast::IntKind::I64,
        },
    );
    map.insert(
        intern::intern("height"),
        Value::Integer {
            value: height as i128,
            kind: crate::ast::IntKind::I64,
        },
    );
    map.insert(intern::intern("rgba"), Value::Bytes(Rc::new(rgba.into_raw())));
    Ok(Value::Map(Rc::new(RefCell::new(MapValue::new(map)))))
}

fn native_image_save_png(args: &[Value]) -> Result<Value, String>
{
    let path = str_arg(args, 0, "Image.save_png")?;
    let width = int_arg(args, 1, "Image.save_png")?;
    let height = int_arg(args, 2, "Image.save_png")?;
    let data = str_arg(args, 3, "Image.save_png")?;
    let bytes = BASE64_STD
        .decode(data.as_bytes())
        .map_err(|e| format!("Image.save_png failed: {e}"))?;
    let expected = width as usize * height as usize * 4;
    if bytes.len() != expected
    {
        return Err("Image.save_png rgba length mismatch".to_string());
    }
    let img: ImageBuffer<Rgba<u8>, _> = ImageBuffer::from_raw(width, height, bytes)
        .ok_or_else(|| "Image.save_png invalid image buffer".to_string())?;
    img.save(&path)
        .map_err(|e| format!("Image.save_png failed: {e}"))?;
    Ok(Value::Boolean(true))
}

fn native_image_save_png_bytes(args: &[Value]) -> Result<Value, String>
{
    let path = str_arg(args, 0, "Image.save_png_bytes")?;
    let width = int_arg(args, 1, "Image.save_png_bytes")?;
    let height = int_arg(args, 2, "Image.save_png_bytes")?;
    let bytes = match args.get(3)
    {
        Some(Value::Bytes(b)) => b.as_ref().clone(),
        Some(Value::ByteBuf(b)) => b.borrow().clone(),
        Some(Value::BytesView(view)) =>
        {
            let end = view.offset.saturating_add(view.len);
            match &view.source
            {
                crate::value::BytesViewSource::Mmap(mmap) => mmap[view.offset..end].to_vec(),
                crate::value::BytesViewSource::MmapMut(mmap) =>
                {
                    let data = mmap.borrow();
                    data[view.offset..end].to_vec()
                }
            }
        }
        _ => return Err("Image.save_png_bytes expects bytes".to_string()),
    };
    let expected = width as usize * height as usize * 4;
    if bytes.len() != expected
    {
        return Err("Image.save_png_bytes rgba length mismatch".to_string());
    }
    let img: ImageBuffer<Rgba<u8>, _> = ImageBuffer::from_raw(width, height, bytes)
        .ok_or_else(|| "Image.save_png_bytes invalid image buffer".to_string())?;
    img.save(&path)
        .map_err(|e| format!("Image.save_png_bytes failed: {e}"))?;
    Ok(Value::Boolean(true))
}

pub fn build_image_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("load_png"), Value::NativeFunction(native_image_load_png));
    map.insert(intern::intern("save_png"), Value::NativeFunction(native_image_save_png));
    map.insert(
        intern::intern("load_png_bytes"),
        Value::NativeFunction(native_image_load_png_bytes),
    );
    map.insert(
        intern::intern("save_png_bytes"),
        Value::NativeFunction(native_image_save_png_bytes),
    );
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("Image"), build_image_module());
}
