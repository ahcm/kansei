use crate::ast::IntKind;
use crate::intern;
use super::LibMap;
use crate::value::{MapValue, Value};
use polars::prelude::*;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fs::File;
use std::rc::Rc;

fn polars_df_arg(args: &[Value], idx: usize, name: &str) -> Result<Rc<RefCell<DataFrame>>, String>
{
    match args.get(idx)
    {
        Some(Value::DataFrame(df)) => Ok(df.clone()),
        _ => Err(format!("{name} expects a DataFrame")),
    }
}

fn polars_str_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn polars_usize_arg(args: &[Value], idx: usize, default: usize, name: &str)
-> Result<usize, String>
{
    match args.get(idx)
    {
        None => Ok(default),
        Some(Value::Integer { value, .. }) =>
        {
            if *value < 0
            {
                Err(format!("{name} expects a non-negative integer"))
            }
            else
            {
                usize::try_from(*value)
                    .map_err(|_| format!("{name} expects a non-negative integer"))
            }
        }
        Some(Value::Unsigned { value, .. }) =>
        {
            usize::try_from(*value).map_err(|_| format!("{name} expects a non-negative integer"))
        }
        _ => Err(format!("{name} expects an integer")),
    }
}

fn native_polars_read_csv(args: &[Value]) -> Result<Value, String>
{
    let path = polars_str_arg(args, 0, "Polars.read_csv")?;
    let df = CsvReader::from_path(&path)
        .map_err(|e| format!("Polars.read_csv failed: {e}"))?
        .finish()
        .map_err(|e| format!("Polars.read_csv failed: {e}"))?;
    Ok(Value::DataFrame(Rc::new(RefCell::new(df))))
}

fn native_polars_write_csv(args: &[Value]) -> Result<Value, String>
{
    let df = polars_df_arg(args, 0, "Polars.write_csv")?;
    let path = polars_str_arg(args, 1, "Polars.write_csv")?;
    let mut file = File::create(&path).map_err(|e| format!("Polars.write_csv failed: {e}"))?;
    CsvWriter::new(&mut file)
        .finish(&mut df.borrow().clone())
        .map_err(|e| format!("Polars.write_csv failed: {e}"))?;
    Ok(Value::Boolean(true))
}

fn native_polars_shape(args: &[Value]) -> Result<Value, String>
{
    let df = polars_df_arg(args, 0, "Polars.shape")?;
    let df_ref = df.borrow();
    let rows = df_ref.height() as i128;
    let cols = df_ref.width() as i128;
    let arr = vec![
        Value::Integer {
            value: rows,
            kind: IntKind::I64,
        },
        Value::Integer {
            value: cols,
            kind: IntKind::I64,
        },
    ];
    Ok(Value::Array(Rc::new(RefCell::new(arr))))
}

fn native_polars_columns(args: &[Value]) -> Result<Value, String>
{
    let df = polars_df_arg(args, 0, "Polars.columns")?;
    let df_ref = df.borrow();
    let cols = df_ref.get_column_names();
    let vals = cols
        .iter()
        .map(|name| Value::String(intern::intern_owned(name.to_string())))
        .collect::<Vec<_>>();
    Ok(Value::Array(Rc::new(RefCell::new(vals))))
}

fn native_polars_head(args: &[Value]) -> Result<Value, String>
{
    let df = polars_df_arg(args, 0, "Polars.head")?;
    let n = polars_usize_arg(args, 1, 5, "Polars.head")?;
    let head = df.borrow().head(Some(n));
    Ok(Value::DataFrame(Rc::new(RefCell::new(head))))
}

fn native_polars_select(args: &[Value]) -> Result<Value, String>
{
    let df = polars_df_arg(args, 0, "Polars.select")?;
    let cols_val = args
        .get(1)
        .ok_or_else(|| "Polars.select expects columns array".to_string())?;
    let cols = match cols_val
    {
        Value::Array(arr) => arr
            .borrow()
            .iter()
            .map(|v| match v
            {
                Value::String(s) => Ok(s.as_str().to_string()),
                _ => Err("Polars.select expects string column names".to_string()),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => return Err("Polars.select expects columns array".to_string()),
    };
    let cols_ref = cols.iter().map(|s| s.as_str()).collect::<Vec<_>>();
    let selected = df
        .borrow()
        .select(&cols_ref)
        .map_err(|e| format!("Polars.select failed: {e}"))?;
    Ok(Value::DataFrame(Rc::new(RefCell::new(selected))))
}

fn native_polars_to_csv(args: &[Value]) -> Result<Value, String>
{
    let df = polars_df_arg(args, 0, "Polars.to_csv")?;
    let mut buf = Vec::new();
    CsvWriter::new(&mut buf)
        .finish(&mut df.borrow().clone())
        .map_err(|e| format!("Polars.to_csv failed: {e}"))?;
    let out = String::from_utf8(buf).map_err(|e| format!("Polars.to_csv failed: {e}"))?;
    Ok(Value::String(intern::intern_owned(out)))
}

pub fn build_polars_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("read_csv"), Value::NativeFunction(native_polars_read_csv));
    map.insert(intern::intern("write_csv"), Value::NativeFunction(native_polars_write_csv));
    map.insert(intern::intern("shape"), Value::NativeFunction(native_polars_shape));
    map.insert(intern::intern("columns"), Value::NativeFunction(native_polars_columns));
    map.insert(intern::intern("head"), Value::NativeFunction(native_polars_head));
    map.insert(intern::intern("select"), Value::NativeFunction(native_polars_select));
    map.insert(intern::intern("to_csv"), Value::NativeFunction(native_polars_to_csv));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("Polars"), build_polars_module());
}
