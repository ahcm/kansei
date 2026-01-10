use crate::ast::Expr;
use std::collections::HashMap;
use std::fmt;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub values: HashMap<String, Value>,
    pub parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self { values: HashMap::new(), parent }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.values.get(name) {
             match v {
                 Value::Reference(r) => Some(r.borrow().clone()),
                 _ => Some(v.clone()),
             }
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    pub fn define(&mut self, name: String, val: Value) {
        self.values.insert(name, val);
    }

    // Set variable in current scope. If it's a reference, update referee.
    pub fn set(&mut self, name: String, val: Value) {
         if let Some(Value::Reference(r)) = self.values.get(&name) {
             *r.borrow_mut() = val;
         } else {
             self.values.insert(name, val);
         }
    }

    pub fn assign(&mut self, name: String, val: Value) {
        if self.values.contains_key(&name) {
            self.set(name, val);
            return;
        }

        if let Some(parent) = &self.parent {
            if parent.borrow_mut().update_existing(&name, &val) {
                return;
            }
        }

        // Not found anywhere, define local
        self.define(name, val);
    }

    fn update_existing(&mut self, name: &str, val: &Value) -> bool {
        if self.values.contains_key(name) {
            // Self::set logic inline because of borrowing issues?
            if let Some(Value::Reference(r)) = self.values.get(name) {
                *r.borrow_mut() = val.clone();
            } else {
                self.values.insert(name.to_string(), val.clone());
            }
            return true;
        }
        if let Some(parent) = &self.parent {
            return parent.borrow_mut().update_existing(name, val);
        }
        false
    }

    pub fn promote(&mut self, name: &str) -> Option<Value> {
         if self.values.contains_key(name) {
             let val = self.values.get(name).unwrap().clone();
             if let Value::Reference(_) = val {
                 return Some(val);
             }
             // Promote
             let r = Rc::new(RefCell::new(val));
             let new_ref = Value::Reference(r);
             self.values.insert(name.to_string(), new_ref.clone());
             return Some(new_ref);
         }
         
         if let Some(parent) = &self.parent {
             return parent.borrow_mut().promote(name);
         }
         
         None
    }
}

#[derive(Clone)]
pub enum Value
{
    Integer(i64),
    String(String),
    Boolean(bool),
    Array(Vec<Value>),
    Map(HashMap<String, Value>),
    Nil,
    Function {
        params: Vec<(String, bool)>,
        body: Box<Expr>,
        env: Rc<RefCell<Environment>>,
    },
    Reference(Rc<RefCell<Value>>),
    Uninitialized,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        let left = match self {
            Value::Reference(r) => &*r.borrow(),
            _ => self,
        };
        let right = match other {
            Value::Reference(r) => &*r.borrow(),
            _ => other,
        };
        
        // Handle double reference unwrapping if needed? 
        // We assume Reference only points to non-Reference.
        // But if left is Reference pointing to Reference... 
        // We'll trust the invariant.

        match (left, right) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => {
                if a.len() != b.len() { return false; }
                for (k, v) in a {
                    if let Some(other_v) = b.get(k) {
                        if v != other_v { return false; }
                    } else {
                        return false;
                    }
                }
                true
            }
            (Value::Nil, Value::Nil) => true,
            (Value::Function { .. }, Value::Function { .. }) => false,
            (Value::Uninitialized, Value::Uninitialized) => true,
            _ => false,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "Integer({})", i),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Array(a) => write!(f, "Array({:?})", a),
            Value::Map(m) => write!(f, "Map({:?})", m),
            Value::Nil => write!(f, "Nil"),
            Value::Function { .. } => write!(f, "Function(...)"),
            Value::Reference(r) => write!(f, "Reference({:?})", r.borrow()),
            Value::Uninitialized => write!(f, "Uninitialized"),
        }
    }
}

impl Value
{
    pub fn inspect(&self) -> String
    {
        match self
        {
            Value::Integer(i) => i.to_string(),
            Value::String(s) => format!("\"{}\"", s),
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
                    .map(|(k, v)| format!("\"{}\": {}\"", k, v.inspect()))
                    .collect();
                format!("{{{}}}", entries.join(", "))
            }
            Value::Nil => "nil".to_string(),
            Value::Function { params, .. } => {
                let p_str: Vec<String> = params.iter().map(|(n, r)| if *r { format!("&{}", n) } else { n.clone() }).collect();
                format!("<function({})>", p_str.join(", "))
            },
            Value::Reference(r) => r.borrow().inspect(),
            Value::Uninitialized => "<uninitialized>".to_string(),
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
            Value::String(s) => write!(f, "{}", s),
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
                    .map(|(k, v)| format!("\"{}\": {}\"", k, v.inspect()))
                    .collect();
                write!(f, "{{{}}}", entries.join(", "))
            }
            Value::Nil => write!(f, "nil"),
            Value::Function { params, .. } => {
                let p_str: Vec<String> = params.iter().map(|(n, r)| if *r { format!("&{}", n) } else { n.clone() }).collect();
                write!(f, "<function({})>", p_str.join(", "))
            },
            Value::Reference(r) => write!(f, "{}", r.borrow()),
            Value::Uninitialized => write!(f, "<uninitialized>"),
        }
    }
}
