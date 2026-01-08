use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value
{
    Integer(i64),
    String(String),
}

impl fmt::Display for Value
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            Value::Integer(i) => write!(f, "{}", i),
            Value::String(s) => write!(f, "{}", s),
        }
    }
}
