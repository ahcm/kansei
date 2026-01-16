use crate::intern;
use super::LibMap;
use crate::value::{MapValue, Value};
use csv::{ReaderBuilder, WriterBuilder};
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

fn native_csv_parse(args: &[Value]) -> Result<Value, String>
{
    let text = str_arg(args, 0, "Csv.parse")?;
    let mut rdr = ReaderBuilder::new()
        .has_headers(false)
        .from_reader(text.as_bytes());
    let mut rows = Vec::new();
    for result in rdr.records()
    {
        let record = result.map_err(|e| format!("Csv.parse failed: {e}"))?;
        let row = record
            .iter()
            .map(|s| Value::String(intern::intern_owned(s.to_string())))
            .collect::<Vec<_>>();
        rows.push(Value::Array(Rc::new(RefCell::new(row))));
    }
    Ok(Value::Array(Rc::new(RefCell::new(rows))))
}

fn native_csv_stringify(args: &[Value]) -> Result<Value, String>
{
    let rows_val = args
        .get(0)
        .ok_or_else(|| "Csv.stringify expects rows".to_string())?;
    let rows = match rows_val
    {
        Value::Array(arr) => arr.borrow(),
        _ => return Err("Csv.stringify expects array of rows".to_string()),
    };
    let mut wtr = WriterBuilder::new().from_writer(vec![]);
    for row_val in rows.iter()
    {
        let row = match row_val
        {
            Value::Array(arr) => arr.borrow(),
            _ => return Err("Csv.stringify expects array rows".to_string()),
        };
        let mut fields = Vec::with_capacity(row.len());
        for field in row.iter()
        {
            fields.push(field.to_string());
        }
        wtr.write_record(&fields)
            .map_err(|e| format!("Csv.stringify failed: {e}"))?;
    }
    let data = wtr
        .into_inner()
        .map_err(|e| format!("Csv.stringify failed: {e}"))?;
    let out = String::from_utf8(data).map_err(|e| format!("Csv.stringify failed: {e}"))?;
    Ok(Value::String(intern::intern_owned(out)))
}

pub fn build_csv_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_csv_parse));
    map.insert(intern::intern("stringify"), Value::NativeFunction(native_csv_stringify));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("Csv"), build_csv_module());
}
