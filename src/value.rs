use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value
{
    Integer(i64),
    String(String),
    Boolean(bool),
    Array(Vec<Value>),
    Map(HashMap<String, Value>),
    Nil,
}

impl Value
{
    pub fn inspect(&self) -> String
    {
        match self
        {
            Value::Integer(i) => i.to_string(),
            Value::String(s) => format!("\"{}\"", s), // Inspect strings with quotes
            Value::Boolean(b) => b.to_string(),
            Value::Array(arr) =>
            {
                let elems: Vec<String> = arr.iter().map(|v| v.inspect()).collect();
                format!("[{}]", elems.join(", "))
            }
            Value::Map(map) =>
            {
                let entries: Vec<String> = map
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}", k, v.inspect()))
                    .collect();
                format!("{{{}}}", entries.join(", "))
            }
            Value::Nil => "nil".to_string(),
        }
    }
}

impl fmt::Display for Value
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            Value::Integer(i) => write!(f, "{}", i),
            Value::String(s) => write!(f, "{}", s), // Display strings without quotes
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Array(arr) =>
            {
                let elems: Vec<String> = arr.iter().map(|v| v.inspect()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            Value::Map(map) =>
            {
                let entries: Vec<String> = map
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}", k, v.inspect()))
                    .collect();
                write!(f, "{{{}}}", entries.join(", "))
            }
            Value::Nil => write!(f, "nil"),
        }
    }
}
