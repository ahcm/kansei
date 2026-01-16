use crate::intern;
use super::LibMap;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

fn num_arg(args: &[Value], idx: usize, name: &str) -> Result<f64, String>
{
    match args.get(idx)
    {
        Some(Value::Float { value, .. }) => Ok(*value),
        Some(Value::Integer { value, .. }) => Ok(*value as f64),
        Some(Value::Unsigned { value, .. }) => Ok(*value as f64),
        _ => Err(format!("{name} expects a number")),
    }
}

fn native_math_sin(args: &[Value]) -> Result<Value, String>
{
    Ok(Value::Float {
        value: num_arg(args, 0, "Math.sin")?.sin(),
        kind: crate::ast::FloatKind::F64,
    })
}

fn native_math_cos(args: &[Value]) -> Result<Value, String>
{
    Ok(Value::Float {
        value: num_arg(args, 0, "Math.cos")?.cos(),
        kind: crate::ast::FloatKind::F64,
    })
}

fn native_math_tan(args: &[Value]) -> Result<Value, String>
{
    Ok(Value::Float {
        value: num_arg(args, 0, "Math.tan")?.tan(),
        kind: crate::ast::FloatKind::F64,
    })
}

fn native_math_asin(args: &[Value]) -> Result<Value, String>
{
    Ok(Value::Float {
        value: num_arg(args, 0, "Math.asin")?.asin(),
        kind: crate::ast::FloatKind::F64,
    })
}

fn native_math_acos(args: &[Value]) -> Result<Value, String>
{
    Ok(Value::Float {
        value: num_arg(args, 0, "Math.acos")?.acos(),
        kind: crate::ast::FloatKind::F64,
    })
}

fn native_math_atan(args: &[Value]) -> Result<Value, String>
{
    Ok(Value::Float {
        value: num_arg(args, 0, "Math.atan")?.atan(),
        kind: crate::ast::FloatKind::F64,
    })
}

fn native_math_log(args: &[Value]) -> Result<Value, String>
{
    Ok(Value::Float {
        value: num_arg(args, 0, "Math.log")?.ln(),
        kind: crate::ast::FloatKind::F64,
    })
}

fn native_math_exp(args: &[Value]) -> Result<Value, String>
{
    Ok(Value::Float {
        value: num_arg(args, 0, "Math.exp")?.exp(),
        kind: crate::ast::FloatKind::F64,
    })
}

fn native_math_pow(args: &[Value]) -> Result<Value, String>
{
    let base = num_arg(args, 0, "Math.pow")?;
    let exp = num_arg(args, 1, "Math.pow")?;
    Ok(Value::Float {
        value: base.powf(exp),
        kind: crate::ast::FloatKind::F64,
    })
}

fn native_math_sqrt(args: &[Value]) -> Result<Value, String>
{
    Ok(Value::Float {
        value: num_arg(args, 0, "Math.sqrt")?.sqrt(),
        kind: crate::ast::FloatKind::F64,
    })
}

pub fn build_math_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("sin"), Value::NativeFunction(native_math_sin));
    map.insert(intern::intern("cos"), Value::NativeFunction(native_math_cos));
    map.insert(intern::intern("tan"), Value::NativeFunction(native_math_tan));
    map.insert(intern::intern("asin"), Value::NativeFunction(native_math_asin));
    map.insert(intern::intern("acos"), Value::NativeFunction(native_math_acos));
    map.insert(intern::intern("atan"), Value::NativeFunction(native_math_atan));
    map.insert(intern::intern("log"), Value::NativeFunction(native_math_log));
    map.insert(intern::intern("exp"), Value::NativeFunction(native_math_exp));
    map.insert(intern::intern("pow"), Value::NativeFunction(native_math_pow));
    map.insert(intern::intern("sqrt"), Value::NativeFunction(native_math_sqrt));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("Math"), build_math_module());
}
