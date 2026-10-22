use crate::ast::{Closure, Expr, FloatKind, IntKind, Param, ParamType, TypeRef};
use crate::intern::SymbolId;
use crate::wasm::WasmFunction;
use memmap2::{Mmap, MmapMut};
use rusqlite::Connection;
use rustc_hash::{FxHashMap, FxHashSet};
#[cfg(feature = "lib-net")]
use rustls::{ClientConnection, StreamOwned};
use std::cell::RefCell;
use std::fmt;
#[cfg(feature = "lib-net")]
use std::net::TcpStream;
use std::rc::Rc;

use smallvec::SmallVec;

pub type NativeFunction = fn(&[Value]) -> Result<Value, String>;
pub type HostFunction = fn(&mut crate::eval::Interpreter, &[Value]) -> Result<Value, String>;

fn type_ref_label(type_ref: &TypeRef) -> String
{
    let mut out = String::new();
    for (idx, segment) in type_ref.path.iter().enumerate()
    {
        if idx > 0
        {
            out.push_str("::");
        }
        out.push_str(crate::intern::symbol_name(*segment).as_str());
    }
    out
}

fn param_label(param: &Param) -> String
{
    let mut out = String::new();
    if param.is_ref
    {
        out.push('&');
    }
    out.push_str(crate::intern::symbol_name(param.name).as_str());
    if let Some(ParamType::Struct(fields)) = &param.type_ann
    {
        out.push_str(" { ");
        let mut first = true;
        for (field_name, type_ref) in fields
        {
            if !first
            {
                out.push_str(", ");
            }
            first = false;
            out.push_str(crate::intern::symbol_name(*field_name).as_str());
            out.push_str(": ");
            out.push_str(&type_ref_label(type_ref));
        }
        out.push_str(" }");
    }
    out
}

#[derive(Clone)]
pub enum BytesViewSource
{
    Mmap(Rc<Mmap>),
    MmapMut(Rc<RefCell<MmapMut>>),
}

#[cfg(feature = "lib-net")]
pub enum NetStream
{
    Tcp(TcpStream),
    Tls(StreamOwned<ClientConnection, TcpStream>),
}

#[derive(Clone)]
pub struct BytesView
{
    pub source: BytesViewSource,
    pub offset: usize,
    pub len: usize,
}

#[derive(Clone)]
pub struct StructField
{
    pub name: Rc<String>,
    pub type_ref: TypeRef,
}

#[derive(Clone)]
pub struct StructType
{
    pub name: Rc<String>,
    pub fields: Vec<StructField>,
    pub field_map: FxHashMap<Rc<String>, usize>,
    pub methods: RefCell<FxHashMap<Rc<String>, Value>>,
}

#[derive(Clone)]
pub struct StructInstance
{
    pub ty: Rc<StructType>,
    pub fields: RefCell<Vec<Value>>,
}

#[derive(Clone)]
pub struct BoundMethod
{
    pub receiver: Value,
    pub func: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment
{
    pub values: Vec<Value>,
    pub slots: SmallVec<[Value; 8]>,
    pub parent: Option<Rc<RefCell<Environment>>>,
    pub is_partial: bool, // If true, allow full recursive lookup (used for currying/params)
    pub version: u64,
    pub file_public: FxHashSet<SymbolId>,
    pub function_public: FxHashSet<SymbolId>,
}

impl Environment
{
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self
    {
        Self {
            values: Vec::new(),
            slots: SmallVec::new(),
            parent,
            is_partial: false,
            version: 0,
            file_public: FxHashSet::default(),
            function_public: FxHashSet::default(),
        }
    }

    pub fn reset(&mut self, parent: Option<Rc<RefCell<Environment>>>, is_partial: bool)
    {
        self.values.clear();
        self.slots.clear();
        self.parent = parent;
        self.is_partial = is_partial;
        self.version = 0;
        self.file_public.clear();
        self.function_public.clear();
    }

    pub fn get(&self, name: SymbolId) -> Option<Value>
    {
        let idx = name as usize;
        if idx < self.values.len()
        {
            let v = &self.values[idx];
            if !matches!(v, Value::Uninitialized)
            {
                return match v
                {
                    Value::Reference(r) => Some(r.borrow().clone()),
                    _ => Some(v.clone()),
                };
            }
        }
        if let Some(parent) = &self.parent
        {
            if self.is_partial
            {
                parent.borrow().get(name)
            }
            else
            {
                // Strictly controlled recursion for functions/references
                parent.borrow().get_recursive(name)
            }
        }
        else
        {
            None
        }
    }

    pub fn get_recursive(&self, name: SymbolId) -> Option<Value>
    {
        let idx = name as usize;
        if idx < self.values.len()
        {
            let v = &self.values[idx];
            if !matches!(v, Value::Uninitialized)
            {
                match v
                {
                    Value::Reference(_) =>
                    {
                        // Dereference if it's a reference
                        if let Value::Reference(r) = v
                        {
                            return Some(r.borrow().clone());
                        }
                        return Some(v.clone());
                    }
                    _ =>
                    {
                        if self.file_public.contains(&name) || self.function_public.contains(&name)
                        {
                            return Some(v.clone());
                        }
                        if self.is_partial
                        {
                            return Some(v.clone());
                        }
                        return None;
                    }
                }
            }
        }
        if let Some(parent) = &self.parent
        {
            parent.borrow().get_recursive(name)
        }
        else
        {
            None
        }
    }

    pub fn define(&mut self, name: SymbolId, val: Value)
    {
        let idx = name as usize;
        if idx >= self.values.len()
        {
            self.values.resize(idx + 1, Value::Uninitialized);
        }
        self.values[idx] = val;
        self.version = self.version.wrapping_add(1);
    }

    // Set variable in current scope. If it's a reference, update referee.
    pub fn set(&mut self, name: SymbolId, val: Value)
    {
        let idx = name as usize;
        if idx >= self.values.len()
        {
            self.values.resize(idx + 1, Value::Uninitialized);
        }
        if let Some(Value::Reference(r)) = self.values.get(idx)
        {
            *r.borrow_mut() = val;
        }
        else
        {
            self.values[idx] = val;
        }
        self.version = self.version.wrapping_add(1);
    }

    pub fn mark_public(&mut self, name: SymbolId)
    {
        self.file_public.insert(name);
    }

    pub fn mark_function_public(&mut self, name: SymbolId)
    {
        self.function_public.insert(name);
    }

    pub fn assign(&mut self, name: SymbolId, val: Value)
    {
        let idx = name as usize;
        if idx < self.values.len()
        {
            self.set(name, val);
            return;
        }

        if let Some(parent) = &self.parent
        {
            if parent.borrow_mut().update_existing(name, &val)
            {
                return;
            }
        }

        // Not found anywhere, define local
        self.define(name, val);
    }

    fn update_existing(&mut self, name: SymbolId, val: &Value) -> bool
    {
        let idx = name as usize;
        if idx < self.values.len()
        {
            // Self::set logic inline because of borrowing issues?
            if let Some(Value::Reference(r)) = self.values.get(idx)
            {
                *r.borrow_mut() = val.clone();
            }
            else
            {
                self.values[idx] = val.clone();
            }
            self.version = self.version.wrapping_add(1);
            return true;
        }
        if let Some(parent) = &self.parent
        {
            return parent.borrow_mut().update_existing(name, val);
        }
        false
    }

    pub fn promote(&mut self, name: SymbolId) -> Option<Value>
    {
        let idx = name as usize;
        if idx < self.values.len()
        {
            let val = self.values[idx].clone();
            if let Value::Reference(_) = val
            {
                return Some(val);
            }
            // Promote
            let r = Rc::new(RefCell::new(val));
            let new_ref = Value::Reference(r);
            self.values[idx] = new_ref.clone();
            self.version = self.version.wrapping_add(1);
            return Some(new_ref);
        }

        if let Some(parent) = &self.parent
        {
            return parent.borrow_mut().promote(name);
        }

        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction
{
    LoadSlot(usize),
    StoreSlot(usize),
    LoadGlobal(SymbolId),
    LoadGlobalCached(SymbolId, Rc<RefCell<GlobalCache>>),
    StoreGlobal(SymbolId),
    LoadConstIdx(usize),
    Pop,
    JumpIfFalse(usize),
    Jump(usize),
    CallBuiltin(Builtin, usize),
    CallValue(usize),
    CallValueWithBlock(Rc<Closure>, usize),
    CallValueWithBlockCached(Rc<RefCell<CallSiteCache>>, Rc<Closure>, usize),
    CallValueWithBlockCached0(Rc<RefCell<CallSiteCache>>, Rc<Closure>),
    CallValueWithBlockCached1(Rc<RefCell<CallSiteCache>>, Rc<Closure>),
    CallValueCached0(Rc<RefCell<CallSiteCache>>),
    CallValueCached1(Rc<RefCell<CallSiteCache>>),
    ForEach
    {
        var_slot: usize,
        body: Rc<Vec<Instruction>>,
    },
    ForEachArray
    {
        var_slot: usize,
        body: Rc<Vec<Instruction>>,
    },
    ForEachF64Array
    {
        var_slot: usize,
        body: Rc<Vec<Instruction>>,
    },
    ForRange
    {
        index_slot: usize,
        end: RangeEnd,
        body: Rc<Vec<Instruction>>,
    },
    ForRangeInt
    {
        index_slot: usize,
        end: RangeEnd,
        step: i64,
        body: Rc<Vec<Instruction>>,
    },
    ForRangeFloat
    {
        index_slot: usize,
        end: RangeEnd,
        step: f64,
        kind: FloatKind,
        body: Rc<Vec<Instruction>>,
    },
    Len,
    MapKeys,
    MapValues,
    MakeArray(usize),
    MakeMap(usize),
    F64ArrayGen
    {
        count: Option<usize>,
    },
    Index,
    Slice,
    IndexCached(Rc<RefCell<IndexCache>>),
    MapIndexCached(Rc<RefCell<MapAccessCache>>),
    F64IndexCached(Rc<RefCell<IndexCache>>),
    IndexAssign,
    F64IndexAssignCached(Rc<RefCell<IndexCache>>),
    CloneValue,
    Not,
    CheckBool,
    AddCached(Rc<RefCell<BinaryOpCache>>),
    SubCached(Rc<RefCell<BinaryOpCache>>),
    MulCached(Rc<RefCell<BinaryOpCache>>),
    DivCached(Rc<RefCell<BinaryOpCache>>),
    CallValueCached(Rc<RefCell<CallSiteCache>>, usize),
    CallGlobalCached(SymbolId, Rc<RefCell<GlobalCache>>, Rc<RefCell<CallSiteCache>>, usize),
    CallGlobalCached0(SymbolId, Rc<RefCell<GlobalCache>>, Rc<RefCell<CallSiteCache>>),
    CallGlobalCached1(SymbolId, Rc<RefCell<GlobalCache>>, Rc<RefCell<CallSiteCache>>),
    CallMethodCached(Rc<String>, Rc<RefCell<MapAccessCache>>, Rc<RefCell<CallSiteCache>>, usize),
    CallMethodCached0(Rc<String>, Rc<RefCell<MapAccessCache>>, Rc<RefCell<CallSiteCache>>),
    CallMethodCached1(Rc<String>, Rc<RefCell<MapAccessCache>>, Rc<RefCell<CallSiteCache>>),
    CallMethodWithBlockCached(
        Rc<String>,
        Rc<RefCell<MapAccessCache>>,
        Rc<RefCell<CallSiteCache>>,
        Rc<Closure>,
        usize,
    ),
    CallMethodWithBlockCached0(
        Rc<String>,
        Rc<RefCell<MapAccessCache>>,
        Rc<RefCell<CallSiteCache>>,
        Rc<Closure>,
    ),
    CallMethodWithBlockCached1(
        Rc<String>,
        Rc<RefCell<MapAccessCache>>,
        Rc<RefCell<CallSiteCache>>,
        Rc<Closure>,
    ),
    ArrayGen,
    Dup,
    F64Axpy
    {
        dst_slot: usize,
        dst_index_slot: usize,
        src_slot: usize,
        src_index_slot: usize,
    },
    F64DotRange
    {
        acc_slot: usize,
        a_slot: usize,
        b_slot: usize,
        index_slot: usize,
        end: RangeEnd,
    },
    F64Dot2Range
    {
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RegBinOp
{
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Gt,
    Lt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RegInstruction
{
    LoadConst
    {
        dst: usize, idx: usize
    },
    LoadSlot
    {
        dst: usize, slot: usize
    },
    StoreSlot
    {
        slot: usize, src: usize
    },
    CloneValue
    {
        dst: usize, src: usize
    },
    BinOpCached
    {
        dst: usize,
        op: RegBinOp,
        left: usize,
        right: usize,
        cache: Rc<RefCell<BinaryOpCache>>,
    },
    F64IndexCached
    {
        dst: usize,
        target: usize,
        index: usize,
        cache: Rc<RefCell<IndexCache>>,
    },
    MapIndexCached
    {
        dst: usize,
        target: usize,
        index: usize,
        cache: Rc<RefCell<MapAccessCache>>,
    },
    F64IndexAssignCached
    {
        dst: usize,
        target: usize,
        index: usize,
        value: usize,
        cache: Rc<RefCell<IndexCache>>,
    },
    CallValueCached0
    {
        dst: usize,
        func: usize,
        cache: Rc<RefCell<CallSiteCache>>,
    },
    CallValueCached1
    {
        dst: usize,
        func: usize,
        arg0: usize,
        cache: Rc<RefCell<CallSiteCache>>,
    },
    CallValueCached2
    {
        dst: usize,
        func: usize,
        arg0: usize,
        arg1: usize,
        cache: Rc<RefCell<CallSiteCache>>,
    },
    CallValueCached3
    {
        dst: usize,
        func: usize,
        arg0: usize,
        arg1: usize,
        arg2: usize,
        cache: Rc<RefCell<CallSiteCache>>,
    },
    CallValueCached
    {
        dst: usize,
        func: usize,
        args: Vec<usize>,
        cache: Rc<RefCell<CallSiteCache>>,
    },
    Len
    {
        dst: usize, src: usize
    },
    MapKeys
    {
        dst: usize, src: usize
    },
    MapValues
    {
        dst: usize, src: usize
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum FastRegInstruction
{
    LoadConst
    {
        dst: usize, value: f64
    },
    LoadSlot
    {
        dst: usize, slot: usize
    },
    BinOp
    {
        dst: usize,
        op: RegBinOp,
        left: usize,
        right: usize,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FastRegFunction
{
    pub code: Vec<FastRegInstruction>,
    pub reg_count: usize,
    pub ret_reg: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RegFunction
{
    pub code: Vec<RegInstruction>,
    pub reg_count: usize,
    pub ret_reg: usize,
    pub const_pool: Rc<Vec<Value>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOpCacheKind
{
    Float,
    Int,
    Uint,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOpCache
{
    pub kind: Option<BinaryOpCacheKind>,
    pub hits: u64,
    pub misses: u64,
}

impl Default for BinaryOpCache
{
    fn default() -> Self
    {
        Self {
            kind: None,
            hits: 0,
            misses: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexCache
{
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
pub struct CallSiteCache
{
    pub func_ptr: Option<usize>,
    pub native_ptr: Option<usize>,
    pub hits: u64,
    pub misses: u64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalCache
{
    pub env_ptr: Option<usize>,
    pub version: u64,
    pub value: Option<Value>,
    pub hits: u64,
    pub misses: u64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapAccessCacheEntry
{
    pub map_ptr: usize,
    pub version: u64,
    pub key: Rc<String>,
    pub value: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapAccessCache
{
    pub entries: [Option<MapAccessCacheEntry>; 2],
    pub hits: u64,
    pub misses: u64,
}

impl Default for CallSiteCache
{
    fn default() -> Self
    {
        Self {
            func_ptr: None,
            native_ptr: None,
            hits: 0,
            misses: 0,
        }
    }
}

impl Default for GlobalCache
{
    fn default() -> Self
    {
        Self {
            env_ptr: None,
            version: 0,
            value: None,
            hits: 0,
            misses: 0,
        }
    }
}

impl Default for MapAccessCache
{
    fn default() -> Self
    {
        Self {
            entries: std::array::from_fn(|_| None),
            hits: 0,
            misses: 0,
        }
    }
}

impl Default for IndexCache
{
    fn default() -> Self
    {
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
pub enum RangeEnd
{
    Slot(usize),
    Const(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Builtin
{
    Puts,
    Print,
    Len,
    ReadFile,
    WriteFile,
    Typeof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionData
{
    pub params: Vec<Param>,
    pub body: Expr,
    pub declarations: Rc<Vec<Rc<String>>>,
    pub param_offset: usize,
    pub is_simple: bool,
    pub uses_env: bool,
    pub code: Option<Rc<Vec<Instruction>>>,
    pub reg_code: Option<Rc<RegFunction>>,
    pub fast_reg_code: Option<Rc<FastRegFunction>>,
    pub const_pool: Rc<Vec<Value>>,
    pub bound_args: Rc<Vec<(usize, Value)>>,
    pub env: Rc<RefCell<Environment>>,
}

#[derive(Clone)]
pub enum Value
{
    Integer
    {
        value: i128,
        kind: IntKind,
    },
    Unsigned
    {
        value: u128,
        kind: IntKind,
    },
    Float
    {
        value: f64,
        kind: FloatKind,
    },
    String(Rc<String>),
    Boolean(bool),
    Array(Rc<RefCell<Vec<Value>>>),
    F32Array(Rc<RefCell<Vec<f32>>>),
    F64Array(Rc<RefCell<Vec<f64>>>),
    I32Array(Rc<RefCell<Vec<i32>>>),
    I64Array(Rc<RefCell<Vec<i64>>>),
    Bytes(Rc<Vec<u8>>),
    ByteBuf(Rc<RefCell<Vec<u8>>>),
    BytesView(Rc<BytesView>),
    StructType(Rc<StructType>),
    StructInstance(Rc<StructInstance>),
    BoundMethod(Rc<BoundMethod>),
    Map(Rc<RefCell<MapValue>>),
    Ast(Rc<Expr>),
    DataFrame(Rc<RefCell<polars::prelude::DataFrame>>),
    Sqlite(Rc<RefCell<Connection>>),
    Mmap(Rc<Mmap>),
    MmapMut(Rc<RefCell<MmapMut>>),
    #[cfg(feature = "lib-net")]
    NetStream(Rc<RefCell<NetStream>>),
    Nil,
    Function(Rc<FunctionData>),
    NativeFunction(NativeFunction),
    HostFunction(HostFunction),
    WasmFunction(Rc<WasmFunction>),
    Reference(Rc<RefCell<Value>>),
    Uninitialized,
}

#[derive(Debug, Clone)]
pub struct MapValue
{
    pub data: FxHashMap<Rc<String>, Value>,
    pub version: u64,
}

impl MapValue
{
    pub fn new(data: FxHashMap<Rc<String>, Value>) -> Self
    {
        Self { data, version: 0 }
    }
}

fn bytes_view_to_vec(view: &BytesView) -> Vec<u8>
{
    match &view.source
    {
        BytesViewSource::Mmap(mmap) =>
        {
            let end = view.offset.saturating_add(view.len);
            mmap[view.offset..end].to_vec()
        }
        BytesViewSource::MmapMut(mmap) =>
        {
            let data = mmap.borrow();
            let end = view.offset.saturating_add(view.len);
            data[view.offset..end].to_vec()
        }
    }
}

impl PartialEq for Value
{
    fn eq(&self, other: &Self) -> bool
    {
        let left = match self
        {
            Value::Reference(r) => &*r.borrow(),
            _ => self,
        };
        let right = match other
        {
            Value::Reference(r) => &*r.borrow(),
            _ => other,
        };

        match (left, right)
        {
            (Value::Integer { value: a, kind: ka }, Value::Integer { value: b, kind: kb }) =>
            {
                a == b && ka == kb
            }
            (Value::Unsigned { value: a, kind: ka }, Value::Unsigned { value: b, kind: kb }) =>
            {
                a == b && ka == kb
            }
            (Value::Float { value: a, kind: ka }, Value::Float { value: b, kind: kb }) =>
            {
                a == b && ka == kb
            } // Note: NaN != NaN
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b, // RefCell PartialEq compares inner values
            (Value::F32Array(a), Value::F32Array(b)) => a == b,
            (Value::F64Array(a), Value::F64Array(b)) => a == b,
            (Value::I32Array(a), Value::I32Array(b)) => a == b,
            (Value::I64Array(a), Value::I64Array(b)) => a == b,
            (Value::Bytes(a), Value::Bytes(b)) => a == b,
            (Value::ByteBuf(a), Value::ByteBuf(b)) =>
            {
                a.borrow().as_slice() == b.borrow().as_slice()
            }
            (Value::Bytes(a), Value::ByteBuf(b)) | (Value::ByteBuf(b), Value::Bytes(a)) =>
            {
                a.as_slice() == b.borrow().as_slice()
            }
            (Value::BytesView(a), Value::BytesView(b)) =>
            {
                a.offset == b.offset
                    && a.len == b.len
                    && bytes_view_to_vec(a) == bytes_view_to_vec(b)
            }
            (Value::BytesView(a), Value::Bytes(b)) | (Value::Bytes(b), Value::BytesView(a)) =>
            {
                bytes_view_to_vec(a) == b.as_ref().as_slice()
            }
            (Value::BytesView(a), Value::ByteBuf(b)) | (Value::ByteBuf(b), Value::BytesView(a)) =>
            {
                bytes_view_to_vec(a) == b.borrow().as_slice()
            }
            (Value::StructType(a), Value::StructType(b)) => Rc::ptr_eq(a, b),
            (Value::StructInstance(a), Value::StructInstance(b)) =>
            {
                Rc::ptr_eq(&a.ty, &b.ty)
                    && a.fields.borrow().as_slice() == b.fields.borrow().as_slice()
            }
            (Value::BoundMethod(a), Value::BoundMethod(b)) =>
            {
                a.receiver == b.receiver && a.func == b.func
            }
            (Value::Map(a), Value::Map(b)) =>
            {
                let map_a = a.borrow();
                let map_b = b.borrow();
                if map_a.data.len() != map_b.data.len()
                {
                    return false;
                }
                for (k, v) in map_a.data.iter()
                {
                    if let Some(other_v) = map_b.data.get(k)
                    {
                        if v != other_v
                        {
                            return false;
                        }
                    }
                    else
                    {
                        return false;
                    }
                }
                true
            }
            (Value::Ast(a), Value::Ast(b)) => a == b,
            (Value::DataFrame(a), Value::DataFrame(b)) => Rc::ptr_eq(a, b),
            (Value::Sqlite(a), Value::Sqlite(b)) => Rc::ptr_eq(a, b),
            (Value::Mmap(a), Value::Mmap(b)) => Rc::ptr_eq(a, b),
            (Value::MmapMut(a), Value::MmapMut(b)) => Rc::ptr_eq(a, b),
            #[cfg(feature = "lib-net")]
            (Value::NetStream(a), Value::NetStream(b)) => Rc::ptr_eq(a, b),
            (Value::Nil, Value::Nil) => true,
            (Value::Function(a), Value::Function(b)) => Rc::ptr_eq(a, b),
            (Value::NativeFunction(a), Value::NativeFunction(b)) => std::ptr::fn_addr_eq(*a, *b),
            (Value::HostFunction(a), Value::HostFunction(b)) => std::ptr::fn_addr_eq(*a, *b),
            (Value::WasmFunction(a), Value::WasmFunction(b)) => Rc::ptr_eq(a, b),
            (Value::Uninitialized, Value::Uninitialized) => true,
            _ => false,
        }
    }
}

impl fmt::Debug for Value
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            Value::Integer { value, kind } => write!(f, "Integer({:?}, {})", kind, value),
            Value::Unsigned { value, kind } => write!(f, "Unsigned({:?}, {})", kind, value),
            Value::Float { value, kind } => write!(f, "Float({:?}, {})", kind, value),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Array(a) => write!(f, "Array({:?})", a.borrow()),
            Value::F32Array(a) => write!(f, "F32Array({:?})", a.borrow()),
            Value::F64Array(a) => write!(f, "F64Array({:?})", a.borrow()),
            Value::I32Array(a) => write!(f, "I32Array({:?})", a.borrow()),
            Value::I64Array(a) => write!(f, "I64Array({:?})", a.borrow()),
            Value::Bytes(b) => write!(f, "Bytes({} bytes)", b.len()),
            Value::ByteBuf(b) => write!(f, "ByteBuf({} bytes)", b.borrow().len()),
            Value::BytesView(b) => write!(f, "BytesView({} bytes)", b.len),
            Value::StructType(ty) => write!(f, "StructType({})", ty.name),
            Value::StructInstance(inst) => write!(f, "StructInstance({})", inst.ty.name),
            Value::BoundMethod(_) => write!(f, "BoundMethod(...)"),
            Value::Map(m) => write!(f, "Map({:?})", m.borrow().data),
            Value::Ast(_) => write!(f, "Ast(...)"),
            Value::DataFrame(df) =>
            {
                write!(f, "DataFrame({}x{})", df.borrow().height(), df.borrow().width())
            }
            Value::Sqlite(_) => write!(f, "Sqlite(<connection>)"),
            Value::Mmap(m) => write!(f, "Mmap({} bytes)", m.len()),
            Value::MmapMut(m) => write!(f, "MmapMut({} bytes)", m.borrow().len()),
            #[cfg(feature = "lib-net")]
            Value::NetStream(_) => write!(f, "NetStream(<connection>)"),
            Value::Nil => write!(f, "Nil"),
            Value::Function(_) => write!(f, "Function(...)"),
            Value::NativeFunction(_) => write!(f, "NativeFunction(...)"),
            Value::HostFunction(_) => write!(f, "HostFunction(...)"),
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
            Value::F32Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.to_string()).collect();
                format!("[{}]", elems.join(", "))
            }
            Value::F64Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.to_string()).collect();
                format!("[{}]", elems.join(", "))
            }
            Value::I32Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.to_string()).collect();
                format!("[{}]", elems.join(", "))
            }
            Value::I64Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.to_string()).collect();
                format!("[{}]", elems.join(", "))
            }
            Value::Bytes(b) => format!("<Bytes {}>", b.len()),
            Value::ByteBuf(b) => format!("<ByteBuf {}>", b.borrow().len()),
            Value::BytesView(b) => format!("<BytesView {}>", b.len),
            Value::StructType(ty) => format!("<Struct {}>", ty.name),
            Value::StructInstance(inst) => format!("<{}>", inst.ty.name),
            Value::BoundMethod(_) => "<bound method>".to_string(),
            Value::Map(map) =>
            {
                let entries: Vec<String> = map
                    .borrow()
                    .data
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}\"", k, v.inspect()))
                    .collect();
                format!("{{{}}}", entries.join(", "))
            }
            Value::Ast(_) => "<ast>".to_string(),
            Value::DataFrame(df) =>
            {
                let df_ref = df.borrow();
                format!("<DataFrame {}x{}>", df_ref.height(), df_ref.width())
            }
            Value::Sqlite(_) => "<Sqlite>".to_string(),
            Value::Mmap(m) => format!("<Mmap {}>", m.len()),
            Value::MmapMut(m) => format!("<MmapMut {}>", m.borrow().len()),
            #[cfg(feature = "lib-net")]
            Value::NetStream(_) => "<NetStream>".to_string(),
            Value::Nil => "nil".to_string(),
            Value::Function(data) =>
            {
                let p_str: Vec<String> = data.params.iter().map(param_label).collect();
                format!("<function({})>", p_str.join(", "))
            }
            Value::NativeFunction(_) => "<native function>".to_string(),
            Value::HostFunction(_) => "<host function>".to_string(),
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
            Value::F32Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.to_string()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            Value::F64Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.to_string()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            Value::I32Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.to_string()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            Value::I64Array(arr) =>
            {
                let elems: Vec<String> = arr.borrow().iter().map(|v| v.to_string()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            Value::Bytes(b) => write!(f, "<Bytes {}>", b.len()),
            Value::ByteBuf(b) => write!(f, "<ByteBuf {}>", b.borrow().len()),
            Value::BytesView(b) => write!(f, "<BytesView {}>", b.len),
            Value::StructType(ty) => write!(f, "<Struct {}>", ty.name),
            Value::StructInstance(inst) => write!(f, "<{}>", inst.ty.name),
            Value::BoundMethod(_) => write!(f, "<bound method>"),
            Value::Map(map) =>
            {
                let entries: Vec<String> = map
                    .borrow()
                    .data
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}\"", k, v.inspect()))
                    .collect();
                write!(f, "{{{}}}", entries.join(", "))
            }
            Value::Ast(_) => write!(f, "<ast>"),
            Value::DataFrame(df) =>
            {
                let df_ref = df.borrow();
                write!(f, "<DataFrame {}x{}>", df_ref.height(), df_ref.width())
            }
            Value::Sqlite(_) => write!(f, "<Sqlite>"),
            Value::Mmap(m) => write!(f, "<Mmap {}>", m.len()),
            Value::MmapMut(m) => write!(f, "<MmapMut {}>", m.borrow().len()),
            #[cfg(feature = "lib-net")]
            Value::NetStream(_) => write!(f, "<NetStream>"),
            Value::Nil => write!(f, "nil"),
            Value::Function(data) =>
            {
                let p_str: Vec<String> = data.params.iter().map(param_label).collect();
                write!(f, "<function({})>", p_str.join(", "))
            }
            Value::NativeFunction(_) => write!(f, "<native function>"),
            Value::HostFunction(_) => write!(f, "<host function>"),
            Value::WasmFunction(fnc) => write!(f, "<wasm function {}>", fnc.name),
            Value::Reference(r) => write!(f, "{}", r.borrow()),
            Value::Uninitialized => write!(f, "<uninitialized>"),
        }
    }
}
