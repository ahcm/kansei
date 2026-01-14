use crate::intern;
use crate::value::{MapValue, Value};
use base64::engine::general_purpose::STANDARD as BASE64_STD;
use base64::Engine;
use rusqlite::types::ValueRef;
use rusqlite::Connection;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

fn conn_arg(args: &[Value], idx: usize, name: &str) -> Result<Rc<RefCell<Connection>>, String>
{
    match args.get(idx)
    {
        Some(Value::Sqlite(conn)) => Ok(conn.clone()),
        _ => Err(format!("{name} expects a Sqlite connection")),
    }
}

fn str_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn row_to_value(row: &rusqlite::Row<'_>, blob_as_bytes: bool) -> rusqlite::Result<Value>
{
    let mut map = FxHashMap::default();
    let stmt = row.as_ref();
    let col_count = stmt.column_count();
    for i in 0..col_count
    {
        let name = stmt.column_name(i).unwrap_or("");
        let cell = row.get_ref(i)?;
        let value = match cell
        {
            ValueRef::Null => Value::Nil,
            ValueRef::Integer(v) =>
            {
                Value::Integer { value: v as i128, kind: crate::ast::IntKind::I64 }
            }
            ValueRef::Real(v) =>
            {
                Value::Float { value: v, kind: crate::ast::FloatKind::F64 }
            }
            ValueRef::Text(v) =>
            {
                Value::String(intern::intern_owned(String::from_utf8_lossy(v).to_string()))
            }
            ValueRef::Blob(v) =>
            {
                if blob_as_bytes
                {
                    Value::Bytes(Rc::new(v.to_vec()))
                }
                else
                {
                    Value::String(intern::intern_owned(BASE64_STD.encode(v)))
                }
            }
        };
        map.insert(intern::intern_owned(name.to_string()), value);
    }
    Ok(Value::Map(Rc::new(RefCell::new(MapValue::new(map)))))
}

fn native_sqlite_open(args: &[Value]) -> Result<Value, String>
{
    let path = str_arg(args, 0, "Sqlite.open")?;
    let conn = Connection::open(path).map_err(|e| format!("Sqlite.open failed: {e}"))?;
    Ok(Value::Sqlite(Rc::new(RefCell::new(conn))))
}

fn native_sqlite_exec(args: &[Value]) -> Result<Value, String>
{
    let conn = conn_arg(args, 0, "Sqlite.exec")?;
    let sql = str_arg(args, 1, "Sqlite.exec")?;
    conn.borrow()
        .execute_batch(&sql)
        .map_err(|e| format!("Sqlite.exec failed: {e}"))?;
    Ok(Value::Boolean(true))
}

fn native_sqlite_query(args: &[Value]) -> Result<Value, String>
{
    let conn = conn_arg(args, 0, "Sqlite.query")?;
    let sql = str_arg(args, 1, "Sqlite.query")?;
    let conn_ref = conn.borrow();
    let mut stmt = conn_ref
        .prepare(&sql)
        .map_err(|e| format!("Sqlite.query failed: {e}"))?;
    let rows = stmt
        .query_map([], |row| row_to_value(row, false))
        .map_err(|e| format!("Sqlite.query failed: {e}"))?;
    let mut out = Vec::new();
    for row in rows
    {
        let value = row.map_err(|e| format!("Sqlite.query failed: {e}"))?;
        out.push(value);
    }
    Ok(Value::Array(Rc::new(RefCell::new(out))))
}

fn native_sqlite_query_bytes(args: &[Value]) -> Result<Value, String>
{
    let conn = conn_arg(args, 0, "Sqlite.query_bytes")?;
    let sql = str_arg(args, 1, "Sqlite.query_bytes")?;
    let conn_ref = conn.borrow();
    let mut stmt = conn_ref
        .prepare(&sql)
        .map_err(|e| format!("Sqlite.query_bytes failed: {e}"))?;
    let rows = stmt
        .query_map([], |row| row_to_value(row, true))
        .map_err(|e| format!("Sqlite.query_bytes failed: {e}"))?;
    let mut out = Vec::new();
    for row in rows
    {
        let value = row.map_err(|e| format!("Sqlite.query_bytes failed: {e}"))?;
        out.push(value);
    }
    Ok(Value::Array(Rc::new(RefCell::new(out))))
}

pub fn build_sqlite_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("open"), Value::NativeFunction(native_sqlite_open));
    map.insert(intern::intern("exec"), Value::NativeFunction(native_sqlite_exec));
    map.insert(intern::intern("query"), Value::NativeFunction(native_sqlite_query));
    map.insert(intern::intern("query_bytes"), Value::NativeFunction(native_sqlite_query_bytes));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
