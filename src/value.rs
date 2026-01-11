use crate::ast::Expr;
use rustc_hash::FxHashMap;
use std::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use smallvec::SmallVec;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub values: FxHashMap<Rc<String>, Value>,
    pub slots: SmallVec<[Value; 8]>,
    pub parent: Option<Rc<RefCell<Environment>>>,
    pub is_partial: bool, // If true, allow full recursive lookup (used for currying/params)
}

impl Environment {
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self { values: FxHashMap::default(), slots: SmallVec::new(), parent, is_partial: false }
    }

    pub fn new_partial(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self { values: FxHashMap::default(), slots: SmallVec::new(), parent, is_partial: true }
    }

    pub fn get_slot(&self, index: usize) -> Option<Value> {
        self.slots.get(index).cloned()
    }

    pub fn set_slot(&mut self, index: usize, val: Value) {
        if index < self.slots.len() {
            self.slots[index] = val;
        }
    }

    pub fn reset(&mut self, parent: Option<Rc<RefCell<Environment>>>, is_partial: bool) {
        self.values.clear();
        self.slots.clear();
        self.parent = parent;
        self.is_partial = is_partial;
    }

    pub fn get(&self, name: &Rc<String>) -> Option<Value> {
        if let Some(v) = self.values.get(name) {
             match v {
                 Value::Reference(r) => Some(r.borrow().clone()),
                 _ => Some(v.clone()),
             }
        } else if let Some(parent) = &self.parent {
            if self.is_partial {
                parent.borrow().get(name)
            } else {
                // Strictly controlled recursion for functions/references
                parent.borrow().get_recursive(name)
            }
        } else {
            None
        }
    }

    pub fn get_recursive(&self, name: &Rc<String>) -> Option<Value> {
        if let Some(v) = self.values.get(name) {
             match v {
                 Value::Function(_) | Value::Reference(_) => {
                     // Dereference if it's a reference
                     if let Value::Reference(r) = v {
                         return Some(r.borrow().clone());
                     }
                     return Some(v.clone());
                 }
                 _ => {
                     if self.is_partial {
                         return Some(v.clone());
                     }
                     return None;
                 }
             }
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_recursive(name)
        } else {
            None
        }
    }

    pub fn get_raw_no_deref(&self, name: &Rc<String>) -> Option<Value> {
        if let Some(v) = self.values.get(name) {
            Some(v.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_raw_no_deref(name)
        } else {
            None
        }
    }

    pub fn define(&mut self, name: Rc<String>, val: Value) {
        self.values.insert(name, val);
    }

    // Set variable in current scope. If it's a reference, update referee.
    pub fn set(&mut self, name: Rc<String>, val: Value) {
         if let Some(Value::Reference(r)) = self.values.get(&name) {
             *r.borrow_mut() = val;
         } else {
             self.values.insert(name, val);
         }
    }

    pub fn assign(&mut self, name: Rc<String>, val: Value) {
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

    fn update_existing(&mut self, name: &Rc<String>, val: &Value) -> bool {
        if self.values.contains_key(name) {
            // Self::set logic inline because of borrowing issues?
            if let Some(Value::Reference(r)) = self.values.get(name) {
                *r.borrow_mut() = val.clone();
            } else {
                self.values.insert(name.clone(), val.clone());
            }
            return true;
        }
        if let Some(parent) = &self.parent {
            return parent.borrow_mut().update_existing(name, val);
        }
        false
    }

    pub fn promote(&mut self, name: &Rc<String>) -> Option<Value> {
         if self.values.contains_key(name) {
             let val = self.values.get(name).unwrap().clone();
             if let Value::Reference(_) = val {
                 return Some(val);
             }
             // Promote
             let r = Rc::new(RefCell::new(val));
             let new_ref = Value::Reference(r);
             self.values.insert(name.clone(), new_ref.clone());
             return Some(new_ref);
         }
         
         if let Some(parent) = &self.parent {
             return parent.borrow_mut().promote(name);
         }
         
         None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    LoadSlot(usize),
    StoreSlot(usize),
    LoadConst(Value),
    Pop,
    JumpIfFalse(usize),
    Jump(usize),
    CallBuiltin(Builtin, usize),
    CallValue(usize),
    ForEach { var_slot: usize, body: Rc<Vec<Instruction>> },
    MakeArray(usize),
    MakeMap(usize),
    Index,
    IndexAssign,
    ArrayGen,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Gt,
    Lt,
    // Add more if needed
}

#[derive(Debug, Clone, PartialEq)]
pub enum Builtin {
    Puts,
    Print,
    Len,
    ReadFile,
    WriteFile,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionData {
    pub params: Vec<(Rc<String>, bool)>,
    pub body: Expr,
    pub declarations: Rc<Vec<Rc<String>>>,
    pub param_offset: usize,
    pub is_simple: bool,
    pub uses_env: bool,
    pub code: Option<Rc<Vec<Instruction>>>,
    pub env: Rc<RefCell<Environment>>,
}

#[derive(Clone)]
pub enum Value
{
    Integer(i64),
    Float(f64),
    String(Rc<String>),
    Boolean(bool),
    Array(Rc<RefCell<Vec<Value>>>),
    Map(Rc<RefCell<FxHashMap<Rc<String>, Value>>>),
    Nil,
    Function(Rc<FunctionData>),
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
        
        match (left, right) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b, // Note: NaN != NaN
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b, // RefCell PartialEq compares inner values
            (Value::Map(a), Value::Map(b)) => {
                let map_a = a.borrow();
                let map_b = b.borrow();
                if map_a.len() != map_b.len() { return false; }
                for (k, v) in map_a.iter() {
                    if let Some(other_v) = map_b.get(k) {
                        if v != other_v { return false; }
                    } else {
                        return false;
                    }
                }
                true
            }
            (Value::Nil, Value::Nil) => true,
            (Value::Function(a), Value::Function(b)) => Rc::ptr_eq(a, b),
            (Value::Uninitialized, Value::Uninitialized) => true,
            _ => false,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "Integer({})", i),
            Value::Float(n) => write!(f, "Float({})", n),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Array(a) => write!(f, "Array({:?})", a.borrow()),
            Value::Map(m) => write!(f, "Map({:?})", m.borrow()),
            Value::Nil => write!(f, "Nil"),
            Value::Function(_) => write!(f, "Function(...)"),
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
            Value::Float(n) => n.to_string(),
            Value::String(s) => format!("\"{}\"", s),
            Value::Boolean(b) => b.to_string(),
            Value::Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.inspect()).collect();
                format!("[{}]", elems.join(", "))
            }
            Value::Map(map) =>
            {
                let entries: Vec<String> = map.borrow()
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}\"", k, v.inspect()))
                    .collect();
                format!("{{{}}}", entries.join(", "))
            }
            Value::Nil => "nil".to_string(),
            Value::Function(data) => {
                let p_str: Vec<String> = data.params.iter()
                    .map(|(n, r)| if *r { format!("&{}", n.as_str()) } else { n.as_str().to_string() })
                    .collect();
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
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.inspect()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            Value::Map(map) =>
            {
                let entries: Vec<String> = map.borrow()
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}\"", k, v.inspect()))
                    .collect();
                write!(f, "{{{}}}", entries.join(", "))
            }
            Value::Nil => write!(f, "nil"),
            Value::Function(data) => {
                let p_str: Vec<String> = data.params.iter()
                    .map(|(n, r)| if *r { format!("&{}", n.as_str()) } else { n.as_str().to_string() })
                    .collect();
                write!(f, "<function({})>", p_str.join(", "))
            },
            Value::Reference(r) => write!(f, "{}", r.borrow()),
            Value::Uninitialized => write!(f, "<uninitialized>"),
        }
    }
}
