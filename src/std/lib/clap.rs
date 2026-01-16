use crate::intern;
use super::LibMap;
use crate::value::{MapValue, Value};
use clap::{Arg, ArgAction, Command};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

fn arg_name(raw: &str) -> String
{
    raw.trim_start_matches('-').to_string()
}

fn leak_name(name: String) -> &'static str
{
    Box::leak(name.into_boxed_str())
}

fn array_strings(val: Option<&Value>, name: &str) -> Result<Vec<String>, String>
{
    match val
    {
        None => Ok(Vec::new()),
        Some(Value::Array(arr)) => arr
            .borrow()
            .iter()
            .map(|v| match v
            {
                Value::String(s) => Ok(s.as_str().to_string()),
                _ => Err(format!("{name} expects an array of strings")),
            })
            .collect(),
        _ => Err(format!("{name} expects an array of strings")),
    }
}

fn native_clap_parse(args: &[Value]) -> Result<Value, String>
{
    let argv = array_strings(args.get(0), "clap.parse args")?;
    let flags = array_strings(args.get(1), "clap.parse flags")?;
    let options = array_strings(args.get(2), "clap.parse options")?;

    let mut cmd = Command::new("kansei").disable_help_flag(true);
    for flag in &flags
    {
        let name = leak_name(arg_name(flag));
        cmd = cmd.arg(Arg::new(name).long(name).action(ArgAction::SetTrue));
    }
    for opt in &options
    {
        let name = leak_name(arg_name(opt));
        cmd = cmd.arg(Arg::new(name).long(name).action(ArgAction::Set));
    }
    cmd = cmd.arg(
        Arg::new("args")
            .action(ArgAction::Append)
            .num_args(0..)
            .allow_hyphen_values(true),
    );

    let mut argv_with_bin = Vec::with_capacity(argv.len() + 1);
    argv_with_bin.push("kansei".to_string());
    argv_with_bin.extend(argv);

    let matches = cmd
        .try_get_matches_from(argv_with_bin)
        .map_err(|e| format!("clap.parse failed: {e}"))?;

    let mut map = FxHashMap::default();
    for flag in flags
    {
        let name = arg_name(&flag);
        let value = matches.get_flag(&name);
        map.insert(intern::intern(&name), Value::Boolean(value));
    }
    for opt in options
    {
        let name = arg_name(&opt);
        let value = matches
            .get_one::<String>(&name)
            .map(|s| Value::String(intern::intern_owned(s.clone())))
            .unwrap_or(Value::Nil);
        map.insert(intern::intern(&name), value);
    }
    if let Some(rest) = matches.get_many::<String>("args")
    {
        let vals = rest
            .map(|s| Value::String(intern::intern_owned(s.to_string())))
            .collect::<Vec<_>>();
        map.insert(intern::intern("args"), Value::Array(Rc::new(RefCell::new(vals))));
    }
    else
    {
        map.insert(intern::intern("args"), Value::Array(Rc::new(RefCell::new(Vec::new()))));
    }

    Ok(Value::Map(Rc::new(RefCell::new(MapValue::new(map)))))
}

pub fn build_clap_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_clap_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("clap"), build_clap_module());
}
