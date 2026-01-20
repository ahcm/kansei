use super::LibMap;
use crate::intern;
use crate::value::{MapValue, Value};
use regex::Regex;
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

fn native_regex_is_match(args: &[Value]) -> Result<Value, String>
{
    let pattern = str_arg(args, 0, "Regex.is_match")?;
    let text = str_arg(args, 1, "Regex.is_match")?;
    let re = Regex::new(&pattern).map_err(|e| format!("Regex.is_match failed: {e}"))?;
    Ok(Value::Boolean(re.is_match(&text)))
}

fn native_regex_find(args: &[Value]) -> Result<Value, String>
{
    let pattern = str_arg(args, 0, "Regex.find")?;
    let text = str_arg(args, 1, "Regex.find")?;
    let re = Regex::new(&pattern).map_err(|e| format!("Regex.find failed: {e}"))?;
    if let Some(mat) = re.find(&text)
    {
        let mut map = FxHashMap::default();
        map.insert(
            intern::intern("start"),
            Value::Integer {
                value: mat.start() as i128,
                kind: crate::ast::IntKind::I64,
            },
        );
        map.insert(
            intern::intern("end"),
            Value::Integer {
                value: mat.end() as i128,
                kind: crate::ast::IntKind::I64,
            },
        );
        map.insert(
            intern::intern("match"),
            Value::String(intern::intern_owned(mat.as_str().to_string())),
        );
        Ok(Value::Map(Rc::new(RefCell::new(MapValue::new(map)))))
    }
    else
    {
        Ok(Value::Nil)
    }
}

fn native_regex_replace(args: &[Value]) -> Result<Value, String>
{
    let pattern = str_arg(args, 0, "Regex.replace")?;
    let text = str_arg(args, 1, "Regex.replace")?;
    let replacement = str_arg(args, 2, "Regex.replace")?;
    let re = Regex::new(&pattern).map_err(|e| format!("Regex.replace failed: {e}"))?;
    let out = re.replace_all(&text, replacement.as_str()).to_string();
    Ok(Value::String(intern::intern_owned(out)))
}

fn native_regex_split(args: &[Value]) -> Result<Value, String>
{
    let pattern = str_arg(args, 0, "Regex.split")?;
    let text = str_arg(args, 1, "Regex.split")?;
    let re = Regex::new(&pattern).map_err(|e| format!("Regex.split failed: {e}"))?;
    let parts = re
        .split(&text)
        .map(|s| Value::String(intern::intern_owned(s.to_string())))
        .collect::<Vec<_>>();
    Ok(Value::Array(Rc::new(RefCell::new(parts))))
}

pub fn build_regex_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("is_match"), Value::NativeFunction(native_regex_is_match));
    map.insert(intern::intern("find"), Value::NativeFunction(native_regex_find));
    map.insert(intern::intern("replace"), Value::NativeFunction(native_regex_replace));
    map.insert(intern::intern("split"), Value::NativeFunction(native_regex_split));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("Regex"), build_regex_module());
}
