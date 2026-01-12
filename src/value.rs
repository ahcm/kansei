use crate::ast::{Expr, FloatKind, IntKind};
use crate::wasm::WasmFunction;
use crate::intern::SymbolId;
use rustc_hash::FxHashMap;
use std::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use smallvec::SmallVec;

pub type NativeFunction = fn(&[Value]) -> Result<Value, String>;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub values: Vec<Value>,
    pub slots: SmallVec<[Value; 8]>,
    pub parent: Option<Rc<RefCell<Environment>>>,
    pub is_partial: bool, // If true, allow full recursive lookup (used for currying/params)
}

impl Environment {
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self { values: Vec::new(), slots: SmallVec::new(), parent, is_partial: false }
    }

    pub fn reset(&mut self, parent: Option<Rc<RefCell<Environment>>>, is_partial: bool) {
        self.values.clear();
        self.slots.clear();
        self.parent = parent;
        self.is_partial = is_partial;
    }

    pub fn get(&self, name: SymbolId) -> Option<Value> {
        let idx = name as usize;
        if idx < self.values.len() {
            let v = &self.values[idx];
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

    pub fn get_recursive(&self, name: SymbolId) -> Option<Value> {
        let idx = name as usize;
        if idx < self.values.len() {
            let v = &self.values[idx];
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

    pub fn define(&mut self, name: SymbolId, val: Value) {
        let idx = name as usize;
        if idx >= self.values.len() {
            self.values.resize(idx + 1, Value::Uninitialized);
        }
        self.values[idx] = val;
    }

    // Set variable in current scope. If it's a reference, update referee.
    pub fn set(&mut self, name: SymbolId, val: Value) {
         let idx = name as usize;
         if idx >= self.values.len() {
             self.values.resize(idx + 1, Value::Uninitialized);
         }
         if let Some(Value::Reference(r)) = self.values.get(idx) {
             *r.borrow_mut() = val;
         } else {
             self.values[idx] = val;
         }
    }

    pub fn assign(&mut self, name: SymbolId, val: Value) {
        let idx = name as usize;
        if idx < self.values.len() {
            self.set(name, val);
            return;
        }

        if let Some(parent) = &self.parent {
            if parent.borrow_mut().update_existing(name, &val) {
                return;
            }
        }

        // Not found anywhere, define local
        self.define(name, val);
    }

    fn update_existing(&mut self, name: SymbolId, val: &Value) -> bool {
        let idx = name as usize;
        if idx < self.values.len() {
            // Self::set logic inline because of borrowing issues?
            if let Some(Value::Reference(r)) = self.values.get(idx) {
                *r.borrow_mut() = val.clone();
            } else {
                self.values[idx] = val.clone();
            }
            return true;
        }
        if let Some(parent) = &self.parent {
            return parent.borrow_mut().update_existing(name, val);
        }
        false
    }

    pub fn promote(&mut self, name: SymbolId) -> Option<Value> {
         let idx = name as usize;
         if idx < self.values.len() {
             let val = self.values[idx].clone();
             if let Value::Reference(_) = val {
                 return Some(val);
             }
             // Promote
             let r = Rc::new(RefCell::new(val));
             let new_ref = Value::Reference(r);
             self.values[idx] = new_ref.clone();
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
    LoadGlobal(SymbolId),
    StoreGlobal(SymbolId),
    LoadConstIdx(usize),
    Pop,
    JumpIfFalse(usize),
    Jump(usize),
    CallBuiltin(Builtin, usize),
    CallValue(usize),
    ForEach { var_slot: usize, body: Rc<Vec<Instruction>> },
    ForRange { index_slot: usize, end: RangeEnd, body: Rc<Vec<Instruction>> },
    ForRangeInt { index_slot: usize, end: RangeEnd, step: i64, body: Rc<Vec<Instruction>> },
    ForRangeFloat { index_slot: usize, end: RangeEnd, step: f64, kind: FloatKind, body: Rc<Vec<Instruction>> },
    MakeArray(usize),
    MakeMap(usize),
    F64ArrayGen { count: Option<usize> },
    Index,
    IndexCached(Rc<RefCell<IndexCache>>),
    F64IndexCached(Rc<RefCell<IndexCache>>),
    IndexAssign,
    F64IndexAssignCached(Rc<RefCell<IndexCache>>),
    CloneValue,
    AddCached(Rc<RefCell<BinaryOpCache>>),
    SubCached(Rc<RefCell<BinaryOpCache>>),
    MulCached(Rc<RefCell<BinaryOpCache>>),
    DivCached(Rc<RefCell<BinaryOpCache>>),
    CallValueCached(Rc<RefCell<CallSiteCache>>, usize),
    ArrayGen,
    Dup,
    F64Axpy { dst_slot: usize, dst_index_slot: usize, src_slot: usize, src_index_slot: usize },
    F64DotRange { acc_slot: usize, a_slot: usize, b_slot: usize, index_slot: usize, end: RangeEnd },
    F64Dot2Range {
        acc1_slot: usize,
        a1_slot: usize,
        b1_slot: usize,
        acc2_slot: usize,
        a2_slot: usize,
        b2_slot: usize,
        index_slot: usize,
        end: RangeEnd,
    },
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
pub enum BinaryOpCacheKind {
    Float,
    Int,
    Uint,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOpCache {
    pub kind: Option<BinaryOpCacheKind>,
    pub hits: u64,
    pub misses: u64,
}

impl Default for BinaryOpCache {
    fn default() -> Self {
        Self {
            kind: None,
            hits: 0,
            misses: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexCache {
    pub map_ptr: Option<usize>,
    pub key: Option<Rc<String>>,
    pub value: Option<Value>,
    pub version: u64,
    pub array_ptr: Option<usize>,
    pub index_usize: Option<usize>,
    pub hits: u64,
    pub misses: u64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallSiteCache {
    pub func_ptr: Option<usize>,
    pub native_ptr: Option<usize>,
    pub hits: u64,
    pub misses: u64,
}

impl Default for CallSiteCache {
    fn default() -> Self {
        Self {
            func_ptr: None,
            native_ptr: None,
            hits: 0,
            misses: 0,
        }
    }
}

impl Default for IndexCache {
    fn default() -> Self {
        Self {
            map_ptr: None,
            key: None,
            value: None,
            version: 0,
            array_ptr: None,
            index_usize: None,
            hits: 0,
            misses: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RangeEnd {
    Slot(usize),
    Const(usize),
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
    pub params: Vec<(SymbolId, bool)>,
    pub body: Expr,
    pub declarations: Rc<Vec<Rc<String>>>,
    pub param_offset: usize,
    pub is_simple: bool,
    pub uses_env: bool,
    pub code: Option<Rc<Vec<Instruction>>>,
    pub const_pool: Rc<Vec<Value>>,
    pub env: Rc<RefCell<Environment>>,
}

#[derive(Clone)]
pub enum Value
{
    Integer { value: i128, kind: IntKind },
    Unsigned { value: u128, kind: IntKind },
    Float { value: f64, kind: FloatKind },
    String(Rc<String>),
    Boolean(bool),
    Array(Rc<RefCell<Vec<Value>>>),
    F64Array(Rc<RefCell<Vec<f64>>>),
    Map(Rc<RefCell<MapValue>>),
    Nil,
    Function(Rc<FunctionData>),
    NativeFunction(NativeFunction),
    WasmFunction(Rc<WasmFunction>),
    Reference(Rc<RefCell<Value>>),
    Uninitialized,
}

#[derive(Debug, Clone)]
pub struct MapValue {
    pub data: FxHashMap<Rc<String>, Value>,
    pub version: u64,
}

impl MapValue {
    pub fn new(data: FxHashMap<Rc<String>, Value>) -> Self {
        Self { data, version: 0 }
    }
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
            (Value::Integer { value: a, kind: ka }, Value::Integer { value: b, kind: kb }) => a == b && ka == kb,
            (Value::Unsigned { value: a, kind: ka }, Value::Unsigned { value: b, kind: kb }) => a == b && ka == kb,
            (Value::Float { value: a, kind: ka }, Value::Float { value: b, kind: kb }) => a == b && ka == kb, // Note: NaN != NaN
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b, // RefCell PartialEq compares inner values
            (Value::F64Array(a), Value::F64Array(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => {
                let map_a = a.borrow();
                let map_b = b.borrow();
                if map_a.data.len() != map_b.data.len() { return false; }
                for (k, v) in map_a.data.iter() {
                    if let Some(other_v) = map_b.data.get(k) {
                        if v != other_v { return false; }
                    } else {
                        return false;
                    }
                }
                true
            }
            (Value::Nil, Value::Nil) => true,
            (Value::Function(a), Value::Function(b)) => Rc::ptr_eq(a, b),
            (Value::NativeFunction(a), Value::NativeFunction(b)) => std::ptr::fn_addr_eq(*a, *b),
            (Value::WasmFunction(a), Value::WasmFunction(b)) => Rc::ptr_eq(a, b),
            (Value::Uninitialized, Value::Uninitialized) => true,
            _ => false,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Integer { value, kind } => write!(f, "Integer({:?}, {})", kind, value),
            Value::Unsigned { value, kind } => write!(f, "Unsigned({:?}, {})", kind, value),
            Value::Float { value, kind } => write!(f, "Float({:?}, {})", kind, value),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Array(a) => write!(f, "Array({:?})", a.borrow()),
            Value::F64Array(a) => write!(f, "F64Array({:?})", a.borrow()),
            Value::Map(m) => write!(f, "Map({:?})", m.borrow().data),
            Value::Nil => write!(f, "Nil"),
            Value::Function(_) => write!(f, "Function(...)"),
            Value::NativeFunction(_) => write!(f, "NativeFunction(...)"),
            Value::WasmFunction(fnc) => write!(f, "WasmFunction({})", fnc.name),
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
            Value::Integer { value, .. } => value.to_string(),
            Value::Unsigned { value, .. } => value.to_string(),
            Value::Float { value, .. } => value.to_string(),
            Value::String(s) => format!("\"{}\"", s),
            Value::Boolean(b) => b.to_string(),
            Value::Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.inspect()).collect();
                format!("[{}]", elems.join(", "))
            }
            Value::F64Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.to_string()).collect();
                format!("[{}]", elems.join(", "))
            }
            Value::Map(map) =>
            {
                let entries: Vec<String> = map.borrow().data
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}\"", k, v.inspect()))
                    .collect();
                format!("{{{}}}", entries.join(", "))
            }
            Value::Nil => "nil".to_string(),
            Value::Function(data) => {
                let p_str: Vec<String> = data.params.iter()
                    .map(|(n, r)| {
                        let name = crate::intern::symbol_name(*n);
                        if *r { format!("&{}", name.as_str()) } else { name.as_str().to_string() }
                    })
                    .collect();
                format!("<function({})>", p_str.join(", "))
            },
            Value::NativeFunction(_) => "<native function>".to_string(),
            Value::WasmFunction(fnc) => format!("<wasm function {}>", fnc.name),
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
            Value::Integer { value, .. } => write!(f, "{}", value),
            Value::Unsigned { value, .. } => write!(f, "{}", value),
            Value::Float { value, .. } => write!(f, "{}", value),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.inspect()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            Value::F64Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.to_string()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            Value::Map(map) =>
            {
                let entries: Vec<String> = map.borrow().data
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}\"", k, v.inspect()))
                    .collect();
                write!(f, "{{{}}}", entries.join(", "))
            }
            Value::Nil => write!(f, "nil"),
            Value::Function(data) => {
                let p_str: Vec<String> = data.params.iter()
                    .map(|(n, r)| {
                        let name = crate::intern::symbol_name(*n);
                        if *r { format!("&{}", name.as_str()) } else { name.as_str().to_string() }
                    })
                    .collect();
                write!(f, "<function({})>", p_str.join(", "))
            },
            Value::NativeFunction(_) => write!(f, "<native function>"),
            Value::WasmFunction(fnc) => write!(f, "<wasm function {}>", fnc.name),
            Value::Reference(r) => write!(f, "{}", r.borrow()),
            Value::Uninitialized => write!(f, "<uninitialized>"),
        }
    }
}
