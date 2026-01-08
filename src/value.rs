use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value
{
    Integer(i64),
    String(String),
    Boolean(bool),
    Nil,
}

impl fmt::Display for Value
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            Value::Integer(i) => write!(f, "{}", i),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
        }
    }
}
