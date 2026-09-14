// Execution backends share runtime values and caches through this module.
mod numeric;
mod compiler;
mod vm;
mod builtins;
mod ast_eval;
mod wat;
pub use wat::dump_wat;
use numeric::*;
use compiler::*;
use vm::*;
use builtins::*;
pub use compiler::{dump_bytecode, resolve_slots};

use crate::ast::{
    Closure, Expr, ExprKind, FloatKind, FormatPart, FormatSpec, IntKind, Op, Param, ParamType,
    TypeRef,
};
use crate::intern;
use crate::intern::{SymbolId, symbol_name};
use crate::kansei_std::{
    build_file_module, build_io_module, build_kansei_module, build_lib_module,
    build_log_module, build_os_module, build_parallel_module, build_simd_module, build_wasm_module,
};
use crate::value::{
    BinaryOpCache, BinaryOpCacheKind, BoundMethod, Builtin, CallSiteCache, EnvValue, Environment,
    FastRegFunction, FastRegInstruction, GlobalCache, IndexCache, Instruction, MapAccessCache,
    MapAccessCacheEntry, MapValue, RangeEnd, RegBinOp, RegFunction, RegInstruction, StructType,
    Value, clone_frozen_value, deep_clone_value, freeze_to_env,
};
use crate::wasm::{
    WasmBackend, WasmFunction, WasmModule, WasmValue, WasmValueType, parse_wasm_backend,
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::{Cell, RefCell};
use std::collections::HashSet;
use std::env;
use std::fs;
use std::fs::OpenOptions;
use std::io::{self, Write};
use std::io::BufWriter;
use std::path::PathBuf;
use std::process::Command;
use std::rc::Rc;
use std::simd::Simd;
use std::simd::num::SimdFloat;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, Clone)]
pub struct RuntimeTraceFrame
{
    pub line: usize,
    pub column: usize,
    pub source: Rc<String>,
}

#[derive(Debug, Clone)]
pub struct RuntimeError
{
    pub message: String,
    pub line: usize,
    pub column: usize,
    pub source: Rc<String>,
    pub trace: Vec<RuntimeTraceFrame>,
}

impl RuntimeError
{
    pub fn new(message: String, line: usize, column: usize, source: Rc<String>) -> Self
    {
        let frame = RuntimeTraceFrame {
            line,
            column,
            source: source.clone(),
        };
        Self {
            message,
            line,
            column,
            source,
            trace: vec![frame],
        }
    }

    pub fn simple(message: String, line: usize) -> Self
    {
        let mut column = 0;
        let mut source = Rc::new(String::new());
        CURRENT_SPAN.with(|span| {
            if let Some((span_line, span_col, span_src)) = span.borrow().as_ref()
            {
                if line == 0 || *span_line == line
                {
                    column = *span_col;
                    source = span_src.clone();
                }
            }
        });
        let trace = if line > 0
        {
            vec![RuntimeTraceFrame {
                line,
                column,
                source: source.clone(),
            }]
        }
        else
        {
            Vec::new()
        };
        Self {
            message,
            line,
            column,
            source,
            trace,
        }
    }

    pub fn from_expr(message: String, expr: &Expr) -> Self
    {
        Self::new(message, expr.line, expr.column, expr.source.clone())
    }

    pub fn wrap(err: RuntimeError, line: usize, column: usize, source: Rc<String>) -> Self
    {
        let mut trace = err.trace;
        trace.push(RuntimeTraceFrame {
            line,
            column,
            source: source.clone(),
        });
        Self {
            message: err.message,
            line,
            column,
            source,
            trace,
        }
    }
}

fn runtime_error_to_value(err: &RuntimeError) -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("message"), Value::String(Rc::new(err.message.clone())));
    map.insert(intern::intern("line"), default_int(err.line as i128));
    map.insert(intern::intern("column"), default_int(err.column as i128));
    map.insert(
        intern::intern("source"),
        Value::String(err.source.clone()),
    );

    let trace_vals = err
        .trace
        .iter()
        .map(|frame| {
            let mut frame_map = FxHashMap::default();
            frame_map.insert(intern::intern("line"), default_int(frame.line as i128));
            frame_map.insert(intern::intern("column"), default_int(frame.column as i128));
            frame_map.insert(
                intern::intern("source"),
                Value::String(frame.source.clone()),
            );
            Value::Map(Rc::new(RefCell::new(MapValue::new(frame_map))))
        })
        .collect::<Vec<_>>();
    map.insert(
        intern::intern("trace"),
        Value::Array(Rc::new(RefCell::new(trace_vals))),
    );
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn runtime_error_from_value(value: &Value) -> Option<RuntimeError>
{
    let map = match value
    {
        Value::Map(map) => map.borrow(),
        _ => return None,
    };

    let message = match map.data.get(&intern::intern("message"))
    {
        Some(Value::String(s)) => s.as_str().to_string(),
        _ => return None,
    };
    let line = map
        .data
        .get(&intern::intern("line"))
        .and_then(int_value_as_i64)
        .unwrap_or(0) as usize;
    let column = map
        .data
        .get(&intern::intern("column"))
        .and_then(int_value_as_i64)
        .unwrap_or(0) as usize;
    let source = map
        .data
        .get(&intern::intern("source"))
        .and_then(|val| match val
        {
            Value::String(s) => Some(s.clone()),
            _ => None,
        })
        .unwrap_or_else(|| Rc::new(String::new()));

    let trace = map
        .data
        .get(&intern::intern("trace"))
        .and_then(|val| match val
        {
            Value::Array(arr) => Some(arr.borrow()),
            _ => None,
        })
        .map(|frames| {
            frames
                .iter()
                .filter_map(|frame_val| {
                    let frame_map = match frame_val
                    {
                        Value::Map(map) => map.borrow(),
                        _ => return None,
                    };
                    let line = frame_map
                        .data
                        .get(&intern::intern("line"))
                        .and_then(int_value_as_i64)
                        .unwrap_or(0) as usize;
                    let column = frame_map
                        .data
                        .get(&intern::intern("column"))
                        .and_then(int_value_as_i64)
                        .unwrap_or(0) as usize;
                    let source = frame_map
                        .data
                        .get(&intern::intern("source"))
                        .and_then(|val| match val
                        {
                            Value::String(s) => Some(s.clone()),
                            _ => None,
                        })
                        .unwrap_or_else(|| Rc::new(String::new()));
                    Some(RuntimeTraceFrame {
                        line,
                        column,
                        source,
                    })
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_else(Vec::new);

    let trace = if trace.is_empty() && line > 0
    {
        vec![RuntimeTraceFrame {
            line,
            column,
            source: source.clone(),
        }]
    }
    else
    {
        trace
    };

    Some(RuntimeError {
        message,
        line,
        column,
        source,
        trace,
    })
}

// Special marker for early return - stored in thread local
thread_local! {
    static EARLY_RETURN: std::cell::RefCell<Option<Value>> = const { std::cell::RefCell::new(None) };
    static CURRENT_SPAN: std::cell::RefCell<Option<(usize, usize, Rc<String>)>> =
        const { std::cell::RefCell::new(None) };
}

const EARLY_RETURN_MARKER: &str = "\x00EARLY_RETURN\x00";

fn set_early_return(value: Value)
{
    EARLY_RETURN.with(|r| *r.borrow_mut() = Some(value));
}

fn take_early_return() -> Option<Value>
{
    EARLY_RETURN.with(|r| r.borrow_mut().take())
}

fn is_early_return(err: &RuntimeError) -> bool
{
    err.message == EARLY_RETURN_MARKER
}

// Helper to handle eval result with early return support
fn handle_eval_result(result: EvalResult) -> EvalResult
{
    match result
    {
        Ok(v) => Ok(v),
        Err(err) if is_early_return(&err) => Ok(take_early_return().unwrap_or(Value::Nil)),
        Err(err) => Err(err),
    }
}

fn make_early_return_error(value: Value) -> RuntimeError
{
    set_early_return(value);
    RuntimeError {
        message: EARLY_RETURN_MARKER.to_string(),
        line: 0,
        column: 0,
        source: Rc::new(String::new()),
        trace: Vec::new(),
    }
}

pub type EvalResult = Result<Value, RuntimeError>;

enum BlockCollectionTarget
{
    Array(Rc<RefCell<Vec<Value>>>),
    F32Array(Rc<RefCell<Vec<f32>>>),
    F64Array(Rc<RefCell<Vec<f64>>>),
    I32Array(Rc<RefCell<Vec<i32>>>),
    I64Array(Rc<RefCell<Vec<i64>>>),
    Map(Rc<RefCell<MapValue>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BytecodeMode
{
    Off,
    Simple,
    Advanced,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WasiTarget
{
    Wasip1,
    Wasip2,
}

impl WasiTarget
{
    fn as_str(self) -> &'static str
    {
        match self
        {
            WasiTarget::Wasip1 => "wasip1",
            WasiTarget::Wasip2 => "wasip2",
        }
    }
}

fn is_collect_into_array(value: &Value) -> bool
{
    matches!(
        value,
        Value::Array(_)
            | Value::F64Array(_)
            | Value::F32Array(_)
            | Value::I64Array(_)
            | Value::I32Array(_)
    )
}

fn resolve_collect_args(
    args: &[Value],
) -> Result<(&Value, Option<&Value>, Option<&Value>, bool), String>
{
    if args.len() < 2 || args.len() > 4
    {
        return Err("std.collect expects count, function, optional context, and optional into"
            .to_string());
    }
    let mut func_idx = None;
    for (idx, arg) in args.iter().enumerate().skip(1)
    {
        if matches!(arg, Value::Function(_) | Value::NativeFunction(_))
        {
            func_idx = Some(idx);
            break;
        }
    }

    let func_idx = match func_idx
    {
        Some(idx) => idx,
        None =>
        {
            return Err("std.collect expects a function as 2nd or 3rd argument".to_string());
        }
    };

    let mut context_arg = None;
    let mut into_arg = None;
    let mut new_order = false;

    if func_idx == 1
    {
        if args.len() == 3
        {
            if is_collect_into_array(&args[2])
            {
                into_arg = Some(&args[2]);
            }
            else
            {
                context_arg = Some(&args[2]);
            }
        }
        else if args.len() == 4
        {
            context_arg = Some(&args[2]);
            into_arg = Some(&args[3]);
        }
    }
    else if func_idx == 2
    {
        context_arg = Some(&args[1]);
        new_order = true;
        if args.len() == 4
        {
            into_arg = Some(&args[3]);
        }
    }
    else
    {
        return Err("std.collect expects function as 2nd or 3rd argument".to_string());
    }

    Ok((&args[func_idx], context_arg, into_arg, new_order))
}

fn native_collect(args: &[Value]) -> Result<Value, String>
{
    let (func_arg, context_arg, into_arg, new_order) = resolve_collect_args(args)?;

    let n = match &args[0]
    {
        Value::Integer { value, .. } if *value >= 0 => *value as usize,
        Value::Unsigned { value, .. } => *value as usize,
        _ => return Err("std.collect expects non-negative integer count".to_string()),
    };

    if let Value::NativeFunction(func) = func_arg
    {
        let func = *func;
        if let Some(into) = into_arg
        {
            match into
            {
                Value::Array(arr) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err("std.collect into array length mismatch".to_string());
                    }
                    for idx in 0..n
                    {
                        let arg = Value::Integer {
                            value: idx as i128,
                            kind: IntKind::I64,
                        };
                        let val = if let Some(ctx) = context_arg
                        {
                            let ctx_val = ctx.clone();
                            if new_order
                            {
                                func(&[ctx_val, arg])?
                            }
                            else
                            {
                                func(&[arg, ctx_val])?
                            }
                        }
                        else
                        {
                            func(&[arg])?
                        };
                        arr.borrow_mut()[idx] = val;
                    }
                    return Ok(Value::Array(arr.clone()));
                }
                Value::F64Array(arr) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err("std.collect into array length mismatch".to_string());
                    }
                    for idx in 0..n
                    {
                        let arg = Value::Integer {
                            value: idx as i128,
                            kind: IntKind::I64,
                        };
                        let val = if let Some(ctx) = context_arg
                        {
                            let ctx_val = ctx.clone();
                            if new_order
                            {
                                func(&[ctx_val, arg])?
                            }
                            else
                            {
                                func(&[arg, ctx_val])?
                            }
                        }
                        else
                        {
                            func(&[arg])?
                        };
                        let num = int_value_as_f64(&val)
                            .ok_or_else(|| "std.collect into F64Array expects numeric results".to_string())?;
                        arr.borrow_mut()[idx] = num;
                    }
                    return Ok(Value::F64Array(arr.clone()));
                }
                Value::F32Array(arr) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err("std.collect into array length mismatch".to_string());
                    }
                    for idx in 0..n
                    {
                        let arg = Value::Integer {
                            value: idx as i128,
                            kind: IntKind::I64,
                        };
                        let val = if let Some(ctx) = context_arg
                        {
                            let ctx_val = ctx.clone();
                            if new_order
                            {
                                func(&[ctx_val, arg])?
                            }
                            else
                            {
                                func(&[arg, ctx_val])?
                            }
                        }
                        else
                        {
                            func(&[arg])?
                        };
                        let num = int_value_as_f64(&val)
                            .ok_or_else(|| "std.collect into F32Array expects numeric results".to_string())?;
                        arr.borrow_mut()[idx] = num as f32;
                    }
                    return Ok(Value::F32Array(arr.clone()));
                }
                Value::I64Array(arr) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err("std.collect into array length mismatch".to_string());
                    }
                    for idx in 0..n
                    {
                        let arg = Value::Integer {
                            value: idx as i128,
                            kind: IntKind::I64,
                        };
                        let val = if let Some(ctx) = context_arg
                        {
                            let ctx_val = ctx.clone();
                            if new_order
                            {
                                func(&[ctx_val, arg])?
                            }
                            else
                            {
                                func(&[arg, ctx_val])?
                            }
                        }
                        else
                        {
                            func(&[arg])?
                        };
                        let num = int_value_as_i64(&val)
                            .ok_or_else(|| "std.collect into I64Array expects integer results".to_string())?;
                        arr.borrow_mut()[idx] = num;
                    }
                    return Ok(Value::I64Array(arr.clone()));
                }
                Value::I32Array(arr) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err("std.collect into array length mismatch".to_string());
                    }
                    for idx in 0..n
                    {
                        let arg = Value::Integer {
                            value: idx as i128,
                            kind: IntKind::I64,
                        };
                        let val = if let Some(ctx) = context_arg
                        {
                            let ctx_val = ctx.clone();
                            if new_order
                            {
                                func(&[ctx_val, arg])?
                            }
                            else
                            {
                                func(&[arg, ctx_val])?
                            }
                        }
                        else
                        {
                            func(&[arg])?
                        };
                        let num = int_value_as_i64(&val)
                            .ok_or_else(|| "std.collect into I32Array expects integer results".to_string())?;
                        if num < i32::MIN as i64 || num > i32::MAX as i64
                        {
                            return Err("std.collect into I32Array result out of range".to_string());
                        }
                        arr.borrow_mut()[idx] = num as i32;
                    }
                    return Ok(Value::I32Array(arr.clone()));
                }
                _ => return Err("std.collect into expects an array".to_string()),
            }
        }

        let mut out = Vec::with_capacity(n);
        for idx in 0..n
        {
            let arg = Value::Integer {
                value: idx as i128,
                kind: IntKind::I64,
            };
            let val = if let Some(ctx) = context_arg
            {
                let ctx_val = ctx.clone();
                if new_order
                {
                    func(&[ctx_val, arg])?
                }
                else
                {
                    func(&[arg, ctx_val])?
                }
            }
            else
            {
                func(&[arg])?
            };
            out.push(val);
        }
        return Ok(Value::Array(Rc::new(RefCell::new(out))));
    }

    if let Value::Function(_) = func_arg
    {
        let func_val = deep_clone_value(func_arg);
        let ctx_val = context_arg.cloned();

        if let (Some(ctx_val), Value::Function(data)) = (&ctx_val, &func_val)
        {
            if let Value::Env(env_val) = ctx_val
            {
                let mut env = data.env.borrow_mut();
                env.is_partial = true;
                for (key, val) in &env_val.data
                {
                    let sym = intern::intern_symbol(key.as_str());
                    env.define(sym, clone_frozen_value(val));
                }
            }
        }

        let mut interpreter = Interpreter::new();
        interpreter.set_autoload_std(false);

        if let Some(into) = into_arg
        {
            match into
            {
                Value::Array(arr) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err("std.collect into array length mismatch".to_string());
                    }
                    for idx in 0..n
                    {
                        let arg = Value::Integer {
                            value: idx as i128,
                            kind: IntKind::I64,
                        };
                        let args = if let (Some(ctx_val), Value::Function(data)) = (&ctx_val, &func_val)
                        {
                            if data.params.len() == 1
                            {
                                vec![arg]
                            }
                            else if new_order
                            {
                                vec![ctx_val.clone(), arg]
                            }
                            else
                            {
                                vec![arg, ctx_val.clone()]
                            }
                        }
                        else
                        {
                            vec![arg]
                        };
                        let val = interpreter.call_value_from_host(func_val.clone(), args)?;
                        arr.borrow_mut()[idx] = val;
                    }
                    return Ok(Value::Array(arr.clone()));
                }
                Value::F64Array(arr) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err("std.collect into array length mismatch".to_string());
                    }
                    for idx in 0..n
                    {
                        let arg = Value::Integer {
                            value: idx as i128,
                            kind: IntKind::I64,
                        };
                        let args = if let (Some(ctx_val), Value::Function(data)) = (&ctx_val, &func_val)
                        {
                            if data.params.len() == 1
                            {
                                vec![arg]
                            }
                            else if new_order
                            {
                                vec![ctx_val.clone(), arg]
                            }
                            else
                            {
                                vec![arg, ctx_val.clone()]
                            }
                        }
                        else
                        {
                            vec![arg]
                        };
                        let val = interpreter.call_value_from_host(func_val.clone(), args)?;
                        let num = int_value_as_f64(&val)
                            .ok_or_else(|| "std.collect into F64Array expects numeric results".to_string())?;
                        arr.borrow_mut()[idx] = num;
                    }
                    return Ok(Value::F64Array(arr.clone()));
                }
                Value::F32Array(arr) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err("std.collect into array length mismatch".to_string());
                    }
                    for idx in 0..n
                    {
                        let arg = Value::Integer {
                            value: idx as i128,
                            kind: IntKind::I64,
                        };
                        let args = if let (Some(ctx_val), Value::Function(data)) = (&ctx_val, &func_val)
                        {
                            if data.params.len() == 1
                            {
                                vec![arg]
                            }
                            else if new_order
                            {
                                vec![ctx_val.clone(), arg]
                            }
                            else
                            {
                                vec![arg, ctx_val.clone()]
                            }
                        }
                        else
                        {
                            vec![arg]
                        };
                        let val = interpreter.call_value_from_host(func_val.clone(), args)?;
                        let num = int_value_as_f64(&val)
                            .ok_or_else(|| "std.collect into F32Array expects numeric results".to_string())?;
                        arr.borrow_mut()[idx] = num as f32;
                    }
                    return Ok(Value::F32Array(arr.clone()));
                }
                Value::I64Array(arr) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err("std.collect into array length mismatch".to_string());
                    }
                    for idx in 0..n
                    {
                        let arg = Value::Integer {
                            value: idx as i128,
                            kind: IntKind::I64,
                        };
                        let args = if let (Some(ctx_val), Value::Function(data)) = (&ctx_val, &func_val)
                        {
                            if data.params.len() == 1
                            {
                                vec![arg]
                            }
                            else if new_order
                            {
                                vec![ctx_val.clone(), arg]
                            }
                            else
                            {
                                vec![arg, ctx_val.clone()]
                            }
                        }
                        else
                        {
                            vec![arg]
                        };
                        let val = interpreter.call_value_from_host(func_val.clone(), args)?;
                        let num = int_value_as_i64(&val)
                            .ok_or_else(|| "std.collect into I64Array expects integer results".to_string())?;
                        arr.borrow_mut()[idx] = num;
                    }
                    return Ok(Value::I64Array(arr.clone()));
                }
                Value::I32Array(arr) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err("std.collect into array length mismatch".to_string());
                    }
                    for idx in 0..n
                    {
                        let arg = Value::Integer {
                            value: idx as i128,
                            kind: IntKind::I64,
                        };
                        let args = if let (Some(ctx_val), Value::Function(data)) = (&ctx_val, &func_val)
                        {
                            if data.params.len() == 1
                            {
                                vec![arg]
                            }
                            else if new_order
                            {
                                vec![ctx_val.clone(), arg]
                            }
                            else
                            {
                                vec![arg, ctx_val.clone()]
                            }
                        }
                        else
                        {
                            vec![arg]
                        };
                        let val = interpreter.call_value_from_host(func_val.clone(), args)?;
                        let num = int_value_as_i64(&val)
                            .ok_or_else(|| "std.collect into I32Array expects integer results".to_string())?;
                        if num < i32::MIN as i64 || num > i32::MAX as i64
                        {
                            return Err("std.collect into I32Array result out of range".to_string());
                        }
                        arr.borrow_mut()[idx] = num as i32;
                    }
                    return Ok(Value::I32Array(arr.clone()));
                }
                _ => return Err("std.collect into expects an array".to_string()),
            }
        }

        let mut out = Vec::with_capacity(n);
        for idx in 0..n
        {
            let arg = Value::Integer {
                value: idx as i128,
                kind: IntKind::I64,
            };
            let args = if let (Some(ctx_val), Value::Function(data)) = (&ctx_val, &func_val)
            {
                if data.params.len() == 1
                {
                    vec![arg]
                }
                else if new_order
                {
                    vec![ctx_val.clone(), arg]
                }
                else
                {
                    vec![arg, ctx_val.clone()]
                }
            }
            else
            {
                vec![arg]
            };
            let val = interpreter.call_value_from_host(func_val.clone(), args)?;
            out.push(val);
        }
        return Ok(Value::Array(Rc::new(RefCell::new(out))));
    }

    Err("std.collect expects a native function or a user function".to_string())
}

fn signed_int_min(kind: IntKind) -> i128
{
    match kind
    {
        IntKind::I8 => i8::MIN as i128,
        IntKind::I16 => i16::MIN as i128,
        IntKind::I32 => i32::MIN as i128,
        IntKind::I64 => i64::MIN as i128,
        IntKind::I128 => i128::MIN,
        _ => panic!("Expected signed int kind, got {:?}", kind),
    }
}

fn signed_int_max(kind: IntKind) -> i128
{
    match kind
    {
        IntKind::I8 => i8::MAX as i128,
        IntKind::I16 => i16::MAX as i128,
        IntKind::I32 => i32::MAX as i128,
        IntKind::I64 => i64::MAX as i128,
        IntKind::I128 => i128::MAX,
        _ => panic!("Expected signed int kind, got {:?}", kind),
    }
}

fn unsigned_int_max(kind: IntKind) -> u128
{
    match kind
    {
        IntKind::U8 => u8::MAX as u128,
        IntKind::U16 => u16::MAX as u128,
        IntKind::U32 => u32::MAX as u128,
        IntKind::U64 => u64::MAX as u128,
        IntKind::U128 => u128::MAX,
        _ => panic!("Expected unsigned int kind, got {:?}", kind),
    }
}

enum ResolvedType
{
    Int(IntKind),
    Uint(IntKind),
    Float(FloatKind),
    Bool,
    String,
    Bytes,
    ByteBuf,
    Array,
    Map,
    F32Array,
    F64Array,
    I32Array,
    I64Array,
    Struct(Rc<StructType>),
    Any,
}

impl PartialEq for ResolvedType
{
    fn eq(&self, other: &Self) -> bool
    {
        match (self, other)
        {
            (ResolvedType::Int(a), ResolvedType::Int(b)) => a == b,
            (ResolvedType::Uint(a), ResolvedType::Uint(b)) => a == b,
            (ResolvedType::Float(a), ResolvedType::Float(b)) => a == b,
            (ResolvedType::Bool, ResolvedType::Bool) => true,
            (ResolvedType::String, ResolvedType::String) => true,
            (ResolvedType::Bytes, ResolvedType::Bytes) => true,
            (ResolvedType::ByteBuf, ResolvedType::ByteBuf) => true,
            (ResolvedType::Array, ResolvedType::Array) => true,
            (ResolvedType::Map, ResolvedType::Map) => true,
            (ResolvedType::F32Array, ResolvedType::F32Array) => true,
            (ResolvedType::F64Array, ResolvedType::F64Array) => true,
            (ResolvedType::I32Array, ResolvedType::I32Array) => true,
            (ResolvedType::I64Array, ResolvedType::I64Array) => true,
            (ResolvedType::Struct(a), ResolvedType::Struct(b)) => Rc::ptr_eq(a, b),
            (ResolvedType::Any, ResolvedType::Any) => true,
            _ => false,
        }
    }
}

fn lookup_value_path(env: &Rc<RefCell<Environment>>, path: &[SymbolId]) -> Option<Value>
{
    if path.is_empty()
    {
        return None;
    }
    let mut value = env.borrow().get(path[0])?;
    for segment in &path[1..]
    {
        let key = symbol_name(*segment);
        match value
        {
            Value::Map(map) =>
            {
                value = map.borrow().data.get(&key).cloned().unwrap_or(Value::Nil);
            }
            Value::Env(env) =>
            {
                value = env
                    .data
                    .get(&key)
                    .map(env_clone_value)
                    .unwrap_or(Value::Nil);
            }
            _ => return None,
        }
    }
    Some(value)
}

fn resolve_type_ref(
    env: &Rc<RefCell<Environment>>,
    type_ref: &TypeRef,
    line: usize,
) -> Result<ResolvedType, RuntimeError>
{
    if type_ref.path.is_empty()
    {
        return Err(RuntimeError::simple("Empty type reference".to_string(), line));
    }
    if type_ref.path.len() == 1
    {
        let name = symbol_name(type_ref.path[0]);
        let kind = match name.as_str()
        {
            "Int8" => Some(ResolvedType::Int(IntKind::I8)),
            "Int16" => Some(ResolvedType::Int(IntKind::I16)),
            "Int32" => Some(ResolvedType::Int(IntKind::I32)),
            "Int64" => Some(ResolvedType::Int(IntKind::I64)),
            "Int128" => Some(ResolvedType::Int(IntKind::I128)),
            "Uint8" => Some(ResolvedType::Uint(IntKind::U8)),
            "Uint16" => Some(ResolvedType::Uint(IntKind::U16)),
            "Uint32" => Some(ResolvedType::Uint(IntKind::U32)),
            "Uint64" => Some(ResolvedType::Uint(IntKind::U64)),
            "Uint128" => Some(ResolvedType::Uint(IntKind::U128)),
            "Float32" => Some(ResolvedType::Float(FloatKind::F32)),
            "Float64" => Some(ResolvedType::Float(FloatKind::F64)),
            "Float128" => Some(ResolvedType::Float(FloatKind::F128)),
            "Bool" => Some(ResolvedType::Bool),
            "String" => Some(ResolvedType::String),
            "Bytes" => Some(ResolvedType::Bytes),
            "ByteBuf" => Some(ResolvedType::ByteBuf),
            "Array" => Some(ResolvedType::Array),
            "Map" => Some(ResolvedType::Map),
            "F32Array" => Some(ResolvedType::F32Array),
            "F64Array" => Some(ResolvedType::F64Array),
            "I32Array" => Some(ResolvedType::I32Array),
            "I64Array" => Some(ResolvedType::I64Array),
            "Any" => Some(ResolvedType::Any),
            _ => None,
        };
        if let Some(kind) = kind
        {
            return Ok(kind);
        }
    }

    if let Some(value) = lookup_value_path(env, &type_ref.path)
    {
        if let Value::StructType(ty) = value
        {
            return Ok(ResolvedType::Struct(ty));
        }
    }
    Err(RuntimeError::simple(format!("Unknown type '{}'", symbol_name(*type_ref.path.last().unwrap()).as_str()), line))
}

fn int_kind_label(kind: IntKind) -> &'static str
{
    match kind
    {
        IntKind::I8 => "Int8",
        IntKind::I16 => "Int16",
        IntKind::I32 => "Int32",
        IntKind::I64 => "Int64",
        IntKind::I128 => "Int128",
        IntKind::U8 => "Uint8",
        IntKind::U16 => "Uint16",
        IntKind::U32 => "Uint32",
        IntKind::U64 => "Uint64",
        IntKind::U128 => "Uint128",
    }
}

fn float_kind_label(kind: FloatKind) -> &'static str
{
    match kind
    {
        FloatKind::F32 => "Float32",
        FloatKind::F64 => "Float64",
        FloatKind::F128 => "Float128",
    }
}

fn resolved_type_name(resolved: &ResolvedType) -> String
{
    match resolved
    {
        ResolvedType::Int(kind) | ResolvedType::Uint(kind) => int_kind_label(*kind).to_string(),
        ResolvedType::Float(kind) => float_kind_label(*kind).to_string(),
        ResolvedType::Bool => "Bool".to_string(),
        ResolvedType::String => "String".to_string(),
        ResolvedType::Bytes => "Bytes".to_string(),
        ResolvedType::ByteBuf => "ByteBuf".to_string(),
        ResolvedType::Array => "Array".to_string(),
        ResolvedType::Map => "Map".to_string(),
        ResolvedType::F32Array => "F32Array".to_string(),
        ResolvedType::F64Array => "F64Array".to_string(),
        ResolvedType::I32Array => "I32Array".to_string(),
        ResolvedType::I64Array => "I64Array".to_string(),
        ResolvedType::Struct(ty) => ty.name.as_str().to_string(),
        ResolvedType::Any => "Any".to_string(),
    }
}

fn value_to_bytes(value: &Value, line: usize, label: &str) -> Result<Vec<u8>, RuntimeError>
{
    match value
    {
        Value::String(s) => Ok(s.as_bytes().to_vec()),
        Value::Bytes(bytes) => Ok(bytes.as_ref().clone()),
        Value::ByteBuf(buf) => Ok(buf.borrow().clone()),
        #[cfg(feature = "lib-mmap") ]
        Value::BytesView(view) =>
        {
            let end = view.offset.saturating_add(view.len);
            match &view.source
            {
                crate::value::BytesViewSource::Mmap(mmap) => Ok(mmap[view.offset..end].to_vec()),
                crate::value::BytesViewSource::MmapMut(mmap) =>
                {
                    let data = mmap.borrow();
                    Ok(data[view.offset..end].to_vec())
                }
            }
        }
        #[cfg(feature = "lib-mmap") ]
        Value::Mmap(mmap) => Ok(mmap.as_ref().to_vec()),
        #[cfg(feature = "lib-mmap") ]
        Value::MmapMut(mmap) => Ok(mmap.borrow().as_ref().to_vec()),
        _ => Err(RuntimeError::simple(format!("{label} expects bytes"), line)),
    }
}

fn coerce_value_to_type(
    value: Value,
    resolved: &ResolvedType,
    line: usize,
    label: &str,
) -> Result<Value, RuntimeError>
{
    match resolved
    {
        ResolvedType::Any => Ok(value),
        ResolvedType::Bool =>
        {
            if matches!(value, Value::Boolean(_))
            {
                Ok(value)
            }
            else
            {
                Err(RuntimeError::simple(format!("{label} expects Bool"), line))
            }
        }
        ResolvedType::String =>
        {
            if matches!(value, Value::String(_))
            {
                Ok(value)
            }
            else
            {
                Err(RuntimeError::simple(format!("{label} expects String"), line))
            }
        }
        ResolvedType::Bytes =>
        {
            let bytes = value_to_bytes(&value, line, label)?;
            Ok(Value::Bytes(Rc::new(bytes)))
        }
        ResolvedType::ByteBuf =>
        {
            let bytes = value_to_bytes(&value, line, label)?;
            Ok(Value::ByteBuf(Rc::new(RefCell::new(bytes))))
        }
        ResolvedType::Array =>
        {
            if matches!(value, Value::Array(_))
            {
                Ok(value)
            }
            else
            {
                Err(RuntimeError::simple(format!("{label} expects Array"), line))
            }
        }
        ResolvedType::Map =>
        {
            if matches!(value, Value::Map(_))
            {
                Ok(value)
            }
            else
            {
                Err(RuntimeError::simple(format!("{label} expects Map"), line))
            }
        }
        ResolvedType::F32Array =>
        {
            if matches!(value, Value::F32Array(_))
            {
                Ok(value)
            }
            else
            {
                Err(RuntimeError::simple(format!("{label} expects F32Array"), line))
            }
        }
        ResolvedType::F64Array =>
        {
            if matches!(value, Value::F64Array(_))
            {
                Ok(value)
            }
            else
            {
                Err(RuntimeError::simple(format!("{label} expects F64Array"), line))
            }
        }
        ResolvedType::I32Array =>
        {
            if matches!(value, Value::I32Array(_))
            {
                Ok(value)
            }
            else
            {
                Err(RuntimeError::simple(format!("{label} expects I32Array"), line))
            }
        }
        ResolvedType::I64Array =>
        {
            if matches!(value, Value::I64Array(_))
            {
                Ok(value)
            }
            else
            {
                Err(RuntimeError::simple(format!("{label} expects I64Array"), line))
            }
        }
        ResolvedType::Int(kind) =>
        {
            let num = match value
            {
                Value::Integer { value, .. } => value,
                Value::Unsigned { value, .. } =>
                {
                    if value > i128::MAX as u128
                    {
                        return Err(RuntimeError::simple(format!("{label} out of range for {:?}", kind), line));
                    }
                    value as i128
                }
                Value::Float { value, .. } => value as i128,
                _ =>
                {
                    return Err(RuntimeError::simple(format!("{label} expects Int"), line));
                }
            };
            let min = signed_int_min(*kind);
            let max = signed_int_max(*kind);
            if num < min || num > max
            {
                return Err(RuntimeError::simple(format!("{label} out of range for {:?}", kind), line));
            }
            Ok(make_signed_int(num, *kind))
        }
        ResolvedType::Uint(kind) =>
        {
            let num = match value
            {
                Value::Unsigned { value, .. } => value,
                Value::Integer { value, .. } =>
                {
                    if value < 0
                    {
                        return Err(RuntimeError::simple(format!("{label} out of range for {:?}", kind), line));
                    }
                    value as u128
                }
                Value::Float { value, .. } =>
                {
                    if value < 0.0
                    {
                        return Err(RuntimeError::simple(format!("{label} out of range for {:?}", kind), line));
                    }
                    value as u128
                }
                _ =>
                {
                    return Err(RuntimeError::simple(format!("{label} expects Uint"), line));
                }
            };
            let max = unsigned_int_max(*kind);
            if num > max
            {
                return Err(RuntimeError::simple(format!("{label} out of range for {:?}", kind), line));
            }
            Ok(make_unsigned_int(num, *kind))
        }
        ResolvedType::Float(kind) =>
        {
            let num = match value
            {
                Value::Float { value, .. } => value,
                v => int_value_as_f64(&v).ok_or_else(|| RuntimeError::simple(format!("{label} expects Float"), line))?,
            };
            Ok(make_float(num, *kind))
        }
        ResolvedType::Struct(ty) =>
        {
            if let Value::StructInstance(inst) = value
            {
                if Rc::ptr_eq(&inst.ty, ty)
                {
                    Ok(Value::StructInstance(inst))
                }
                else
                {
                    Err(RuntimeError::simple(format!("{label} expects {}", ty.name), line))
                }
            }
            else
            {
                Err(RuntimeError::simple(format!("{label} expects {}", ty.name), line))
            }
        }
    }
}

fn coerce_param_value(
    env: &Rc<RefCell<Environment>>,
    param: &Param,
    value: Value,
    line: usize,
) -> Result<Value, RuntimeError>
{
    match &param.type_ann
    {
        Some(ParamType::Struct(fields)) =>
        {
            let label = symbol_name(param.name);
            coerce_struct_param(env, value, fields, line, label.as_str())
        }
        None => Ok(value),
    }
}

fn coerce_struct_param(
    env: &Rc<RefCell<Environment>>,
    value: Value,
    fields: &[(SymbolId, TypeRef)],
    line: usize,
    label: &str,
) -> Result<Value, RuntimeError>
{
    let inst = match value
    {
        Value::StructInstance(inst) => inst,
        _ =>
        {
            return Err(RuntimeError::simple(format!("{label} expects a struct value"), line));
        }
    };
    for (field_name, type_ref) in fields
    {
        let key = symbol_name(*field_name);
        let idx = inst.ty.field_map.get(&key).ok_or_else(|| RuntimeError::simple(format!("{label} missing field '{}'", key.as_str()), line))?;
        let field = inst.ty.fields.get(*idx).ok_or_else(|| RuntimeError::simple(format!("Struct field '{}' out of bounds", key.as_str()), line))?;
        let expected = resolve_type_ref(env, type_ref, line)?;
        let actual = resolve_type_ref(env, &field.type_ref, line)?;
        if actual != expected
        {
            return Err(RuntimeError::simple(format!(
                    "{label} expects field '{}' to be {}",
                    key.as_str(),
                    resolved_type_name(&expected)
                ), line));
        }
        let mut field_values = inst.fields.borrow_mut();
        let current = field_values[*idx].clone();
        let coerced = coerce_value_to_type(current, &expected, line, key.as_str())?;
        field_values[*idx] = coerced;
    }
    Ok(Value::StructInstance(inst))
}

fn map_keys_array(map: &MapValue) -> Value
{
    let mut vals = Vec::with_capacity(map.data.len());
    for key in map.data.keys()
    {
        vals.push(Value::String(key.clone()));
    }
    Value::Array(Rc::new(RefCell::new(vals)))
}

fn map_values_array(map: &MapValue) -> Value
{
    let mut vals = Vec::with_capacity(map.data.len());
    for val in map.data.values()
    {
        vals.push(val.clone());
    }
    Value::Array(Rc::new(RefCell::new(vals)))
}

fn env_clone_value(value: &Value) -> Value
{
    clone_frozen_value(value)
}

fn env_keys_array(env: &EnvValue) -> Value
{
    let mut vals = Vec::with_capacity(env.data.len());
    for key in env.data.keys()
    {
        vals.push(Value::String(key.clone()));
    }
    Value::Array(Rc::new(RefCell::new(vals)))
}

fn env_values_array(env: &EnvValue) -> Value
{
    let mut vals = Vec::with_capacity(env.data.len());
    for val in env.data.values()
    {
        vals.push(env_clone_value(val));
    }
    Value::Array(Rc::new(RefCell::new(vals)))
}

fn parse_signed_int(args: &[Value], kind: IntKind, label: &str) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| format!("{}.parse expects 1 argument", label))?;
    let s = match arg
    {
        Value::String(s) => s.as_str(),
        _ => return Err(format!("{}.parse expects a string argument", label)),
    };
    let value = s
        .parse::<i128>()
        .map_err(|_| format!("{}.parse failed to parse string", label))?;
    let min = signed_int_min(kind);
    let max = signed_int_max(kind);
    if value < min || value > max
    {
        return Err(format!("{}.parse out of range for {:?}", label, kind));
    }
    Ok(make_signed_int(value, kind))
}

fn parse_unsigned_int(args: &[Value], kind: IntKind, label: &str) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| format!("{}.parse expects 1 argument", label))?;
    let s = match arg
    {
        Value::String(s) => s.as_str(),
        _ => return Err(format!("{}.parse expects a string argument", label)),
    };
    let value = s
        .parse::<u128>()
        .map_err(|_| format!("{}.parse failed to parse string", label))?;
    let max = unsigned_int_max(kind);
    if value > max
    {
        return Err(format!("{}.parse out of range for {:?}", label, kind));
    }
    Ok(make_unsigned_int(value, kind))
}

fn parse_format_spec(spec: &str, line: usize) -> Result<FormatSpec, RuntimeError>
{
    if let Some(rest) = spec.strip_prefix('.')
    {
        if rest.is_empty() || !rest.chars().all(|c| c.is_ascii_digit())
        {
            return Err(RuntimeError::simple("Invalid format precision".to_string(), line));
        }
        let precision = rest.parse::<usize>().map_err(|_| RuntimeError::simple("Invalid format precision".to_string(), line))?;
        return Ok(FormatSpec {
            precision: Some(precision),
        });
    }
    Err(RuntimeError::simple("Unsupported format specifier".to_string(), line))
}

fn split_format_expr(input: &str, line: usize)
-> Result<(String, Option<FormatSpec>), RuntimeError>
{
    let mut depth_paren = 0usize;
    let mut depth_brack = 0usize;
    let mut depth_brace = 0usize;
    let mut in_string = false;
    let mut string_delim = '\0';
    let mut split_at: Option<usize> = None;
    for (idx, ch) in input.chars().enumerate()
    {
        if in_string
        {
            if ch == string_delim
            {
                in_string = false;
            }
            continue;
        }
        match ch
        {
            '"' | '`' =>
            {
                in_string = true;
                string_delim = ch;
            }
            '(' => depth_paren += 1,
            ')' => depth_paren = depth_paren.saturating_sub(1),
            '[' => depth_brack += 1,
            ']' => depth_brack = depth_brack.saturating_sub(1),
            '{' => depth_brace += 1,
            '}' => depth_brace = depth_brace.saturating_sub(1),
            ':' if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 =>
            {
                split_at = Some(idx);
                break;
            }
            _ =>
            {}
        }
    }

    if let Some(idx) = split_at
    {
        let expr = input[..idx].trim().to_string();
        let spec_str = input[idx + 1..].trim();
        if spec_str.is_empty()
        {
            return Err(RuntimeError::simple("Empty format specifier".to_string(), line));
        }
        let spec = parse_format_spec(spec_str, line)?;
        Ok((expr, Some(spec)))
    }
    else
    {
        Ok((input.trim().to_string(), None))
    }
}

fn parse_format_parts(content: &str, line: usize) -> Result<Vec<FormatPart>, RuntimeError>
{
    let mut parts: Vec<FormatPart> = Vec::new();
    let mut literal = String::new();
    let chars: Vec<char> = content.chars().collect();
    let mut i = 0;
    while i < chars.len()
    {
        let ch = chars[i];
        if ch == '{'
        {
            if i + 1 < chars.len() && chars[i + 1] == '{'
            {
                literal.push('{');
                i += 2;
                continue;
            }
            if !literal.is_empty()
            {
                parts.push(FormatPart::Literal(intern::intern_owned(literal.clone())));
                literal.clear();
            }
            let start = i + 1;
            let mut end = start;
            while end < chars.len() && chars[end] != '}'
            {
                end += 1;
            }
            if end >= chars.len()
            {
                return Err(RuntimeError::simple("Unclosed format string expression".to_string(), line));
            }
            let expr_slice: String = chars[start..end].iter().collect();
            let (expr_str, spec) = split_format_expr(&expr_slice, line)?;
            if expr_str.trim().is_empty()
            {
                return Err(RuntimeError::simple("Empty format string expression".to_string(), line));
            }
            let parse_result = crate::parser::parse_source(&expr_str);
            let expr = match parse_result
            {
                Ok(expr) => expr,
                Err(_) =>
                {
                    return Err(RuntimeError::simple("Invalid format string expression".to_string(), line));
                }
            };
            parts.push(FormatPart::Expr {
                expr: Box::new(expr),
                spec,
            });
            i = end + 1;
        }
        else if ch == '}'
        {
            if i + 1 < chars.len() && chars[i + 1] == '}'
            {
                literal.push('}');
                i += 2;
            }
            else
            {
                return Err(RuntimeError::simple("Unmatched '}' in format string".to_string(), line));
            }
        }
        else
        {
            literal.push(ch);
            i += 1;
        }
    }
    if !literal.is_empty()
    {
        parts.push(FormatPart::Literal(intern::intern_owned(literal)));
    }
    Ok(parts)
}

fn eval_format_parts(
    interpreter: &mut Interpreter,
    parts: &[FormatPart],
    slots: &mut [Value],
    _line: usize,
) -> Result<String, RuntimeError>
{
    let mut out = String::new();
    for part in parts
    {
        match part
        {
            FormatPart::Literal(s) => out.push_str(s.as_str()),
            FormatPart::Expr { expr, spec } =>
            {
                let val = interpreter.eval(expr, slots)?;
                if let Some(spec) = spec
                {
                    if let Some(precision) = spec.precision
                    {
                        let formatted = match &val
                        {
                            Value::Float { value, .. } =>
                            {
                                format!("{:.p$}", value, p = precision)
                            }
                            v if int_value_as_f64(v).is_some() =>
                            {
                                let num = int_value_as_f64(v).unwrap_or(0.0);
                                format!("{:.p$}", num, p = precision)
                            }
                            _ => val.to_string(),
                        };
                        out.push_str(&formatted);
                        continue;
                    }
                }
                out.push_str(&val.to_string());
            }
        }
    }
    Ok(out)
}

fn default_module_search_paths(main_path: Option<&std::path::Path>) -> Vec<PathBuf>
{
    if let Ok(paths) = env::var("KANSEI_MODULE_PATH")
    {
        let mut out = Vec::new();
        for entry in paths.split(':')
        {
            if !entry.is_empty()
            {
                out.push(PathBuf::from(entry));
            }
        }
        return out;
    }

    let mut out = Vec::new();
    let base = main_path
        .and_then(|p| p.parent().map(PathBuf::from))
        .or_else(|| env::current_dir().ok());
    if let Some(base) = base
    {
        out.push(base.join("modules"));
    }
    if let Ok(home) = env::var("HOME")
    {
        out.push(PathBuf::from(home.clone()).join(".local/share/kansei/modules"));
        out.push(PathBuf::from(home).join(".local/lib/kansei/modules"));
    }
    out.push(PathBuf::from("/usr/local/lib/kansei/modules"));
    out.push(PathBuf::from("/usr/lib/kansei/modules"));
    out
}

fn default_wasm_search_paths(main_path: Option<&std::path::Path>) -> Vec<PathBuf>
{
    if let Ok(paths) = env::var("KANSEI_WASM_PATH")
    {
        let mut out = Vec::new();
        for entry in paths.split(':')
        {
            if !entry.is_empty()
            {
                out.push(PathBuf::from(entry));
            }
        }
        return out;
    }

    let mut out = Vec::new();
    let base = main_path
        .and_then(|p| p.parent().map(PathBuf::from))
        .or_else(|| env::current_dir().ok());
    if let Some(base) = base
    {
        out.push(base.join("wasm"));
    }
    if let Ok(home) = env::var("HOME")
    {
        out.push(PathBuf::from(home.clone()).join(".local/share/kansei/wasm"));
        out.push(PathBuf::from(home).join(".local/lib/kansei/wasm"));
    }
    out.push(PathBuf::from("/usr/local/lib/kansei/wasm"));
    out.push(PathBuf::from("/usr/lib/kansei/wasm"));
    out
}

pub fn module_search_paths_for(main_path: Option<&std::path::Path>) -> Vec<PathBuf>
{
    default_module_search_paths(main_path)
}

pub fn wasm_search_paths_for(main_path: Option<&std::path::Path>) -> Vec<PathBuf>
{
    default_wasm_search_paths(main_path)
}

struct ModuleCacheEntry
{
    exports: Rc<RefCell<MapValue>>,
    namespace: Vec<SymbolId>,
    env: Rc<RefCell<Environment>>,
    modified: Option<SystemTime>,
    size: u64,
}

struct ModuleLookupEntry
{
    env_version: u64,
    value: Value,
}

struct ExportSpec
{
    namespace: Vec<SymbolId>,
    names: Vec<SymbolId>,
    line: usize,
}

enum LogTarget
{
    Stderr,
    File(BufWriter<fs::File>),
}

pub enum LogFileMode
{
    Append,
    Truncate,
    Rotate { max_bytes: u64 },
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum LogLevel
{
    Error = 0,
    Warn = 1,
    Info = 2,
    Debug = 3,
}

impl LogLevel
{
    fn as_str(self) -> &'static str
    {
        match self
        {
            LogLevel::Error => "error",
            LogLevel::Warn => "warn",
            LogLevel::Info => "info",
            LogLevel::Debug => "debug",
        }
    }

    fn from_str(value: &str) -> Option<Self>
    {
        match value
        {
            "error" => Some(LogLevel::Error),
            "warn" => Some(LogLevel::Warn),
            "info" => Some(LogLevel::Info),
            "debug" => Some(LogLevel::Debug),
            _ => None,
        }
    }
}

impl LogTarget
{
    fn write_line(&mut self, msg: &str, flush: bool) -> io::Result<()>
    {
        match self
        {
            Self::Stderr =>
            {
                let mut stderr = io::stderr();
                stderr.write_all(msg.as_bytes())?;
                stderr.write_all(b"\n")?;
                if flush
                {
                    stderr.flush()?;
                }
                Ok(())
            }
            Self::File(writer) =>
            {
                writer.write_all(msg.as_bytes())?;
                writer.write_all(b"\n")?;
                if flush
                {
                    writer.flush()?;
                }
                Ok(())
            }
        }
    }

    // write(...) removed; unused for now.
}

fn rotate_log_file(path: &std::path::Path, max_bytes: u64) -> io::Result<()>
{
    if max_bytes == 0
    {
        return Ok(());
    }
    let metadata = match fs::metadata(path)
    {
        Ok(meta) => meta,
        Err(_) => return Ok(()),
    };
    if metadata.len() < max_bytes
    {
        return Ok(());
    }
    let rotated_path = PathBuf::from(format!("{}.1", path.display()));
    if rotated_path.exists()
    {
        fs::remove_file(&rotated_path)?;
    }
    fs::rename(path, rotated_path)?;
    Ok(())
}

fn format_log_message(format: &str, level: LogLevel, message: &str) -> String
{
    let timestamp = current_log_timestamp();
    format
        .replace("{timestamp}", &timestamp)
        .replace("{level}", level.as_str())
        .replace("{message}", message)
}

fn current_log_timestamp() -> String
{
    let now = SystemTime::now();
    match now.duration_since(UNIX_EPOCH)
    {
        Ok(duration) =>
        {
            let secs = duration.as_secs();
            let millis = duration.subsec_millis();
            format!("{secs}.{millis:03}")
        }
        Err(_) => "0.000".to_string(),
    }
}

pub struct Interpreter
{
    // Current environment (scope)
    env: Rc<RefCell<Environment>>,
    // Stack of blocks passed to currently executing functions.
    block_stack: Vec<Option<(Rc<Closure>, Rc<RefCell<Environment>>)>>,
    // Pool of spare environments for reuse
    env_pool: Vec<Rc<RefCell<Environment>>>,
    // Pool of reusable register buffers for reg-simple functions
    reg_pool: Vec<Vec<Value>>,
    // Pool of reusable operand stacks for bytecode frames
    stack_pool: Vec<Vec<Value>>,
    bytecode_mode: BytecodeMode,
    autoload_std: bool,
    env_seed_cache: FxHashMap<usize, Vec<(SymbolId, Value)>>,
    module_cache: FxHashMap<String, ModuleCacheEntry>,
    module_lookup_cache: FxHashMap<String, ModuleLookupEntry>,
    module_search_paths: Vec<PathBuf>,
    wasm_search_paths: Vec<PathBuf>,
    module_dir_stack: Vec<PathBuf>,
    log_target: LogTarget,
    log_format: String,
    log_flush: bool,
    log_min_level: LogLevel,
    log_target_desc: String,
    log_mode_desc: String,
}

impl Interpreter
{
    pub fn new() -> Self
    {
        let module_search_paths = default_module_search_paths(None);
        let wasm_search_paths = default_wasm_search_paths(None);
        let mut root_env = Environment::new(None);
        root_env.is_partial = true;
        Self {
            env: Rc::new(RefCell::new(root_env)),
            block_stack: Vec::new(),
            env_pool: Vec::with_capacity(32),
            reg_pool: Vec::with_capacity(32),
            stack_pool: Vec::with_capacity(32),
            bytecode_mode: BytecodeMode::Simple,
            autoload_std: true,
            env_seed_cache: FxHashMap::default(),
            module_cache: FxHashMap::default(),
            module_lookup_cache: FxHashMap::default(),
            module_search_paths,
            wasm_search_paths,
            module_dir_stack: Vec::new(),
            log_target: LogTarget::Stderr,
            log_format: "{message}".to_string(),
            log_flush: true,
            log_min_level: LogLevel::Info,
            log_target_desc: "stderr".to_string(),
            log_mode_desc: "stderr".to_string(),
        }
    }

    pub fn set_bytecode_mode(&mut self, mode: BytecodeMode)
    {
        self.bytecode_mode = mode;
    }

    pub fn set_autoload_std(&mut self, enabled: bool)
    {
        self.autoload_std = enabled;
    }

    pub fn set_main_path(&mut self, path: &std::path::Path)
    {
        self.module_search_paths = default_module_search_paths(Some(path));
        self.wasm_search_paths = default_wasm_search_paths(Some(path));
        self.module_lookup_cache.clear();
        let base = if path.is_dir()
        {
            Some(path.to_path_buf())
        }
        else
        {
            path.parent().map(|p| p.to_path_buf())
        };
        self.module_dir_stack = base.into_iter().collect();
    }

    pub fn set_log_file(&mut self, path: &std::path::Path) -> io::Result<()>
    {
        self.set_log_file_with_mode(path, LogFileMode::Append)
    }

    pub fn set_log_file_with_mode(
        &mut self,
        path: &std::path::Path,
        mode: LogFileMode,
    ) -> io::Result<()>
    {
        if let LogFileMode::Rotate { max_bytes } = mode
        {
            rotate_log_file(path, max_bytes)?;
        }
        let mut options = OpenOptions::new();
        options.create(true);
        match mode
        {
            LogFileMode::Append | LogFileMode::Rotate { .. } =>
            {
                options.append(true);
            }
            LogFileMode::Truncate =>
            {
                options.truncate(true).write(true);
            }
        }
        let file = options.open(path)?;
        self.log_target = LogTarget::File(BufWriter::new(file));
        self.log_target_desc = path.to_string_lossy().to_string();
        self.log_mode_desc = match mode
        {
            LogFileMode::Append => "append".to_string(),
            LogFileMode::Truncate => "truncate".to_string(),
            LogFileMode::Rotate { max_bytes } => format!("rotate:{max_bytes}"),
        };
        Ok(())
    }

    pub fn set_log_stderr(&mut self)
    {
        self.log_target = LogTarget::Stderr;
        self.log_target_desc = "stderr".to_string();
        self.log_mode_desc = "stderr".to_string();
    }

    pub fn set_log_format(&mut self, format: String)
    {
        self.log_format = format;
    }

    pub fn set_log_flush(&mut self, flush: bool)
    {
        self.log_flush = flush;
    }

    pub fn set_log_level(&mut self, level: LogLevel)
    {
        self.log_min_level = level;
    }

    pub fn set_log_level_str(&mut self, level: &str) -> Result<(), String>
    {
        let parsed = LogLevel::from_str(level)
            .ok_or_else(|| "log.level expects: error|warn|info|debug".to_string())?;
        self.set_log_level(parsed);
        Ok(())
    }

    pub fn log_with_level(&mut self, level: &str, message: &str) -> Result<(), String>
    {
        let parsed = LogLevel::from_str(level)
            .ok_or_else(|| "log level must be: error|warn|info|debug".to_string())?;
        self.write_log(parsed, message)
            .map_err(|err| err.message)
    }

    pub fn log_config(&self) -> (String, String, String, bool, String)
    {
        (
            self.log_target_desc.clone(),
            self.log_mode_desc.clone(),
            self.log_format.clone(),
            self.log_flush,
            self.log_min_level.as_str().to_string(),
        )
    }

    pub fn get_global_value(&self, name: SymbolId) -> Option<Value>
    {
        self.env.borrow().get(name)
    }

    pub fn list_wasm_modules(&self) -> Vec<String>
    {
        let wasm_sym = intern::intern_symbol("wasm");
        match self.env.borrow().get(wasm_sym)
        {
            Some(Value::Map(map)) =>
            {
                let map_ref = map.borrow();
                map_ref
                    .data
                    .keys()
                    .map(|key| key.as_str().to_string())
                    .collect()
            }
            _ => Vec::new(),
        }
    }

    pub fn call_wasm_function_public(
        &mut self,
        func: Rc<WasmFunction>,
        args: Vec<Value>,
    ) -> Result<Value, String>
    {
        let mut arg_vals = smallvec::SmallVec::<[Value; 8]>::new();
        arg_vals.extend(args);
        self.call_wasm_function(func, arg_vals, 0)
            .map_err(|err| err.message)
    }

    fn write_log(&mut self, level: LogLevel, message: &str) -> Result<(), RuntimeError>
    {
        if level > self.log_min_level
        {
            return Ok(());
        }
        let formatted = format_log_message(&self.log_format, level, message);
        self.log_target
            .write_line(&formatted, self.log_flush)
            .map_err(|err| RuntimeError::simple(format!("log write failed: {err}"), 0))
    }

    fn get_local_value(&self, name: SymbolId) -> Option<Value>
    {
        let env_ref = self.env.borrow();
        let idx = name as usize;
        if idx < env_ref.values.len()
        {
            let val = env_ref.values[idx].clone();
            if matches!(val, Value::Uninitialized)
            {
                None
            }
            else
            {
                Some(val)
            }
        }
        else
        {
            None
        }
    }

    fn take_stack(&mut self) -> Vec<Value>
    {
        self.stack_pool
            .pop()
            .unwrap_or_else(|| Vec::with_capacity(8))
    }

    fn recycle_stack(&mut self, mut stack: Vec<Value>)
    {
        stack.clear();
        self.stack_pool.push(stack);
    }

    fn get_env(
        &mut self,
        parent: Option<Rc<RefCell<Environment>>>,
        is_partial: bool,
    ) -> Rc<RefCell<Environment>>
    {
        if let Some(env_rc) = self.env_pool.pop()
        {
            env_rc.borrow_mut().reset(parent, is_partial);
            env_rc
        }
        else
        {
            let mut env = Environment::new(parent);
            env.is_partial = is_partial;
            Rc::new(RefCell::new(env))
        }
    }

    fn apply_bound_args(&self, bound_args: &[(usize, Value)], slots: &mut [Value])
    {
        for (slot, val) in bound_args
        {
            if let Some(dst) = slots.get_mut(*slot)
            {
                *dst = val.clone();
            }
        }
    }

    fn ensure_slot_capacity(
        &self,
        slots: &mut smallvec::SmallVec<[Value; 8]>,
        param_offset: usize,
        param_len: usize,
        bound_args: &[(usize, Value)],
    )
    {
        let mut required = param_offset.saturating_add(param_len);
        if let Some(max_slot) = bound_args.iter().map(|(slot, _)| *slot).max()
        {
            required = required.max(max_slot + 1);
        }
        if slots.len() < required
        {
            slots.resize(required, Value::Uninitialized);
        }
    }

    fn recycle_env(&mut self, env_rc: Rc<RefCell<Environment>>)
    {
        if Rc::strong_count(&env_rc) == 1
        {
            if self.env_pool.len() < 128
            {
                self.env_pool.push(env_rc);
            }
        }
    }

    fn get_reg_buffer(&mut self, size: usize) -> Vec<Value>
    {
        if size == 0
        {
            return Vec::new();
        }
        if let Some(mut buf) = self.reg_pool.pop()
        {
            if buf.capacity() < size
            {
                buf.reserve(size - buf.capacity());
            }
            buf.clear();
            buf.resize(size, Value::Uninitialized);
            buf
        }
        else
        {
            vec![Value::Uninitialized; size]
        }
    }

    fn recycle_reg_buffer(&mut self, mut buf: Vec<Value>)
    {
        if self.reg_pool.len() < 128
        {
            buf.clear();
            self.reg_pool.push(buf);
        }
    }

    pub fn define_global(&mut self, name: SymbolId, val: Value)
    {
        self.env.borrow_mut().define(name, val);
    }

    fn ensure_std_module(&mut self)
    {
        let std_sym = intern::intern_symbol("std");
        let existing = { self.env.borrow().get(std_sym) };
        match existing
        {
            Some(Value::Map(map)) =>
            {
                let mut map_mut = map.borrow_mut();
                let mut changed = false;
                if !map_mut.data.contains_key(&intern::intern("Int8"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Int8"), build_int8_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("Int16"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Int16"), build_int16_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("Int32"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Int32"), build_int32_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("Int64"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Int64"), build_int64_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("Int128"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Int128"), build_int128_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("Uint8"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Uint8"), build_uint8_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("Uint16"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Uint16"), build_uint16_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("Uint32"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Uint32"), build_uint32_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("Uint64"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Uint64"), build_uint64_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("Uint128"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Uint128"), build_uint128_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("Float32"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Float32"), build_float32_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("Float64"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Float64"), build_float64_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("Float128"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("Float128"), build_float128_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("IO"))
                {
                    map_mut.data.insert(intern::intern("IO"), build_io_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("File"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("File"), build_file_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("lib"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("lib"), build_lib_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("simd"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("simd"), build_simd_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("kansei"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("kansei"), build_kansei_module());
                    changed = true;
                }
                if !map_mut.data.contains_key(&intern::intern("parallel"))
                {
                    map_mut
                        .data
                        .insert(intern::intern("parallel"), build_parallel_module());
                    changed = true;
                }
                if changed
                {
                    map_mut.version = map_mut.version.wrapping_add(1);
                }
            }
            Some(_) | None =>
            {
                self.define_global(std_sym, build_std_module());
            }
        }
    }

    fn ensure_wasm_namespace(&mut self) -> Rc<RefCell<MapValue>>
    {
        let wasm_sym = intern::intern_symbol("wasm");
        let existing = { self.env.borrow().get(wasm_sym) };
        match existing
        {
            Some(Value::Map(map)) => map,
            _ =>
            {
                let map = Rc::new(RefCell::new(MapValue::new(FxHashMap::default())));
                self.define_global(wasm_sym, Value::Map(map.clone()));
                map
            }
        }
    }

    fn env_chain_version(&self) -> u64
    {
        let mut version = 0u64;
        let mut shift = 0u32;
        let mut current = Some(self.env.clone());
        while let Some(env_rc) = current
        {
            let env_ref = env_rc.borrow();
            version ^= env_ref.version.rotate_left(shift);
            shift = (shift + 11) % 64;
            current = env_ref.parent.clone();
        }
        version
    }

    fn module_lookup_key(path: &[SymbolId]) -> String
    {
        let mut out = String::new();
        for (idx, segment) in path.iter().enumerate()
        {
            if idx > 0
            {
                out.push_str("::");
            }
            out.push_str(symbol_name(*segment).as_str());
        }
        out
    }

    fn resolve_module_file(&self, import_path: &str, line: usize) -> Result<PathBuf, RuntimeError>
    {
        if !import_path.ends_with(".ks")
        {
            return Err(RuntimeError::simple("import path must end with .ks".to_string(), line));
        }

        let mut rel = PathBuf::new();
        for segment in import_path.split("::")
        {
            for part in segment.split('/')
            {
                if !part.is_empty()
                {
                    rel.push(part);
                }
            }
        }

        for base in self.module_dir_stack.iter().rev()
        {
            let candidate = base.join(&rel);
            if candidate.exists()
            {
                return Ok(candidate);
            }
        }

        for base in &self.module_search_paths
        {
            let candidate = base.join(&rel);
            if candidate.exists()
            {
                return Ok(candidate);
            }
        }

        Err(RuntimeError::simple(format!("Module file '{}' not found", import_path), line))
    }

    fn extract_export_spec(&self, expr: &Expr) -> Result<(ExportSpec, Expr), RuntimeError>
    {
        let line = expr.line;
        match &expr.kind
        {
            ExprKind::Export { namespace, names } =>
            {
                let spec = ExportSpec {
                    namespace: namespace.clone(),
                    names: names.clone(),
                    line,
                };
                let body = Expr {
                    kind: ExprKind::Nil,
                    line,
                    column: expr.column,
                    source: expr.source.clone(),
                };
                Ok((spec, body))
            }
            ExprKind::Block(stmts) =>
            {
                if stmts.is_empty()
                {
                    return Err(RuntimeError::simple("export declaration required at top of module".to_string(), line));
                }
                match &stmts[0].kind
                {
                    ExprKind::Export { namespace, names } =>
                    {
                        let spec = ExportSpec {
                            namespace: namespace.clone(),
                            names: names.clone(),
                            line: stmts[0].line,
                        };
                        let rest = &stmts[1..];
                        let body = if rest.is_empty()
                        {
                            Expr {
                                kind: ExprKind::Nil,
                                line,
                                column: stmts[0].column,
                                source: stmts[0].source.clone(),
                            }
                        }
                        else if rest.len() == 1
                        {
                            rest[0].clone()
                        }
                        else
                        {
                            Expr {
                                kind: ExprKind::Block(rest.to_vec()),
                                line,
                                column: stmts[0].column,
                                source: stmts[0].source.clone(),
                            }
                        };
                        Ok((spec, body))
                    }
                    _ => Err(RuntimeError::simple(
                        "export declaration required at top of module".to_string(),
                        line,
                    )),
                }
            }
            _ => Err(RuntimeError::simple(
                "export declaration required at top of module".to_string(),
                line,
            )),
        }
    }

    fn build_exports(
        &self,
        env: &Rc<RefCell<Environment>>,
        spec: &ExportSpec,
    ) -> Result<Rc<RefCell<MapValue>>, RuntimeError>
    {
        let mut exports = FxHashMap::default();
        let env_ref = env.borrow();
        for name in &spec.names
        {
            let val = env_ref.get(*name).ok_or_else(|| RuntimeError::simple(format!("export '{}' not found in module", symbol_name(*name).as_str()), spec.line))?;
            if matches!(val, Value::Uninitialized)
            {
                return Err(RuntimeError::simple(format!(
                        "export '{}' not found in module",
                        symbol_name(*name).as_str()
                    ), spec.line));
            }
            exports.insert(symbol_name(*name), val);
        }
        Ok(Rc::new(RefCell::new(MapValue::new(exports))))
    }

    fn bind_module_namespace(
        &mut self,
        namespace: &[SymbolId],
        module_map: Rc<RefCell<MapValue>>,
        line: usize,
    ) -> Result<(), RuntimeError>
    {
        if namespace.is_empty()
        {
            return Err(RuntimeError::simple(
                "export namespace must not be empty".to_string(),
                line,
            ));
        }

        let first = namespace[0];
        let mut current = match self.get_local_value(first)
        {
            Some(Value::Map(map)) => map,
            Some(_) =>
            {
                return Err(RuntimeError::simple(format!(
                        "Module namespace '{}' is not a module",
                        symbol_name(first).as_str()
                    ), line));
            }
            None =>
            {
                let map = Rc::new(RefCell::new(MapValue::new(FxHashMap::default())));
                self.define_global(first, Value::Map(map.clone()));
                map
            }
        };

        if namespace.len() > 1
        {
            for segment in &namespace[1..namespace.len() - 1]
            {
                let seg_name = symbol_name(*segment);
                let next = {
                    let mut map_mut = current.borrow_mut();
                    if let Some(Value::Map(next_map)) = map_mut.data.get(&seg_name).cloned()
                    {
                        next_map
                    }
                    else
                    {
                        let new_map = Rc::new(RefCell::new(MapValue::new(FxHashMap::default())));
                        map_mut
                            .data
                            .insert(seg_name.clone(), Value::Map(new_map.clone()));
                        map_mut.version = map_mut.version.wrapping_add(1);
                        new_map
                    }
                };
                current = next;
            }
        }

        let last = *namespace.last().unwrap();
        let mut map_mut = current.borrow_mut();
        map_mut
            .data
            .insert(symbol_name(last), Value::Map(module_map));
        map_mut.version = map_mut.version.wrapping_add(1);
        Ok(())
    }

    fn import_module(
        &mut self,
        path: &Rc<String>,
        alias: Option<SymbolId>,
        line: usize,
        file_public: bool,
    ) -> Result<Vec<SymbolId>, RuntimeError>
    {
        let file_path = self.resolve_module_file(path.as_str(), line)?;
        let metadata = fs::metadata(&file_path).map_err(|e| RuntimeError::simple(format!("Failed to read module metadata: {}", e), line))?;
        let modified = metadata.modified().ok();
        let size = metadata.len();
        let key = file_path.to_string_lossy().to_string();

        let (exports_map, namespace) = if let Some(entry) = self.module_cache.get(&key)
        {
            if entry.size == size && entry.modified == modified
            {
                let _ = entry.env.clone();
                (entry.exports.clone(), entry.namespace.clone())
            }
            else
            {
                self.load_module_from_file(&file_path, modified, size, line)?
            }
        }
        else
        {
            self.load_module_from_file(&file_path, modified, size, line)?
        };

        self.bind_module_namespace(&namespace, exports_map.clone(), line)?;
        if let Some(alias) = alias
        {
            self.env.borrow_mut().define(alias, Value::Map(exports_map));
            if file_public
            {
                self.env.borrow_mut().mark_public(alias);
            }
        }
        else if file_public
        {
            if let Some(root) = namespace.first()
            {
                self.env.borrow_mut().mark_public(*root);
            }
        }
        self.module_lookup_cache
            .remove(&Self::module_lookup_key(&namespace));
        Ok(namespace)
    }

    fn load_module_from_file(
        &mut self,
        file_path: &std::path::Path,
        modified: Option<SystemTime>,
        size: u64,
        line: usize,
    ) -> Result<(Rc<RefCell<MapValue>>, Vec<SymbolId>), RuntimeError>
    {
        let source = fs::read_to_string(file_path).map_err(|e| RuntimeError::simple(format!("Failed to read module file: {}", e), line))?;

        let parse_result = crate::parser::parse_source(&source);

        let mut ast = match parse_result
        {
            Ok(ast) => ast,
            Err(_) =>
            {
                return Err(RuntimeError::simple("Failed to parse module file".to_string(), line));
            }
        };

        resolve_slots(&mut ast);
        let (export_spec, body) = self.extract_export_spec(&ast)?;

        let module_env = self.get_env(Some(self.env.clone()), false);
        let original_env = self.env.clone();
        if let Some(parent) = file_path.parent()
        {
            self.module_dir_stack.push(parent.to_path_buf());
        }
        self.env = module_env.clone();
        let eval_result = self.eval(&body, &mut []);
        self.env = original_env;
        if file_path.parent().is_some()
        {
            self.module_dir_stack.pop();
        }
        if let Err(err) = eval_result
        {
            return Err(err);
        }

        let exports_map = self.build_exports(&module_env, &export_spec)?;
        let entry = ModuleCacheEntry {
            exports: exports_map.clone(),
            namespace: export_spec.namespace.clone(),
            env: module_env.clone(),
            modified,
            size,
        };
        let key = file_path.to_string_lossy().to_string();
        self.module_cache.insert(key, entry);
        Ok((exports_map, export_spec.namespace))
    }

    fn load_wasm_module(&mut self, path: &[SymbolId], line: usize) -> EvalResult
    {
        if path.len() < 2
        {
            return Err(RuntimeError::simple("load wasm requires a module name".to_string(), line));
        }
        let wasm_sym = intern::intern_symbol("wasm");
        if path[0] != wasm_sym
        {
            return Err(RuntimeError::simple("load currently supports only wasm:: modules".to_string(), line));
        }

        let mut rel = PathBuf::new();
        for segment in &path[1..path.len() - 1]
        {
            rel.push(symbol_name(*segment).as_str());
        }
        let module_name = symbol_name(*path.last().unwrap());
        rel.push(format!("{}.wasm", module_name.as_str()));

        let mut resolved = None;
        for base in &self.wasm_search_paths
        {
            let candidate = base.join(&rel);
            if candidate.exists()
            {
                resolved = Some(candidate);
                break;
            }
        }
        let resolved = resolved.ok_or_else(|| RuntimeError::simple(format!("Wasm module '{}' not found", rel.display()), line))?;

        let backend = self.resolve_wasm_backend(line)?;
        let module = WasmModule::load(&resolved, backend)
            .map_err(|message| RuntimeError::simple(message, line))?;
        let mut exports = FxHashMap::default();
        {
            let module_ref = module.borrow();
            for name in module_ref.functions.keys()
            {
                exports.insert(
                    name.clone(),
                    Value::WasmFunction(Rc::new(WasmFunction {
                        name: name.clone(),
                        module: module.clone(),
                    })),
                );
            }
        }

        let wasm_map = self.ensure_wasm_namespace();
        let mut wasm_mut = wasm_map.borrow_mut();
        wasm_mut
            .data
            .insert(module_name, Value::Map(Rc::new(RefCell::new(MapValue::new(exports)))));
        wasm_mut.version = wasm_mut.version.wrapping_add(1);
        Ok(Value::Nil)
    }

    fn resolve_wasm_backend(&self, line: usize) -> Result<WasmBackend, RuntimeError>
    {
        let program_sym = intern::intern_symbol("program");
        if let Some(Value::Map(map)) = self.env.borrow().get(program_sym)
        {
            if let Some(value) = map.borrow().data.get(&intern::intern("wasm_backend"))
            {
                let backend = match value
                {
                    Value::String(backend) => backend.as_str(),
                    _ =>
                    {
                        return Err(RuntimeError::simple("program.wasm_backend must be a string".to_string(), line));
                    }
                };
                return parse_wasm_backend(backend)
                    .map_err(|message| RuntimeError::simple(message, line));
            }
        }
        Ok(WasmBackend::Wasmtime)
    }

    fn import_path(&mut self, path: &[SymbolId], line: usize) -> EvalResult
    {
        if path.is_empty()
        {
            return Err(RuntimeError::simple("use requires a module path".to_string(), line));
        }

        let std_sym = intern::intern_symbol("std");
        if path[0] == std_sym
        {
            if self.autoload_std
            {
                self.ensure_std_module();
            }
        }

        let key = Self::module_lookup_key(path);
        let env_version = self.env_chain_version();
        if let Some(entry) = self.module_lookup_cache.get(&key)
        {
            if entry.env_version == env_version
            {
                let _ = entry.value.clone();
                return Ok(Value::Nil);
            }
        }

        let mut current = self.env.borrow().get(path[0]).ok_or_else(|| RuntimeError::simple(format!("Module '{}' not found", symbol_name(path[0]).as_str()), line))?;
        let mut current_name = symbol_name(path[0]);

        for segment in &path[1..]
        {
            let seg_name = symbol_name(*segment);
            match current
            {
                Value::Map(map) =>
                {
                    if let Some(next) = map.borrow().data.get(&seg_name).cloned()
                    {
                        current = next;
                        current_name = seg_name;
                    }
                    else
                    {
                        return Err(RuntimeError::simple(format!(
                                "Module '{}' has no member '{}'",
                                current_name.as_str(),
                                seg_name.as_str()
                            ), line));
                    }
                }
                _ =>
                {
                    return Err(RuntimeError::simple(format!("'{}' is not a module", current_name.as_str()), line));
                }
            }
        }

        self.module_lookup_cache.insert(
            key,
            ModuleLookupEntry {
                env_version,
                value: current.clone(),
            },
        );
        Ok(Value::Nil)
    }



    fn call_value(
        &mut self,
        func_val: Value,
        arg_vals: smallvec::SmallVec<[Value; 8]>,
        line: usize,
        block: Option<Rc<Closure>>,
    ) -> EvalResult
    {
        match func_val
        {
            Value::Function(data) => self.invoke_function(data, arg_vals, line, block),
            Value::NativeFunction(func) =>
            {
                if block.is_some()
                {
                    return Err(RuntimeError::simple("Native function does not accept a block".to_string(), line));
                }
                func(&arg_vals).map_err(|message| RuntimeError::simple(message, line))
            }
            Value::HostFunction(func) =>
            {
                if block.is_some()
                {
                    return Err(RuntimeError::simple("Host function does not accept a block".to_string(), line));
                }
                func(self, &arg_vals).map_err(|message| RuntimeError::simple(message, line))
            }
            Value::WasmFunction(func) =>
            {
                if block.is_some()
                {
                    return Err(RuntimeError::simple("Wasm function does not accept a block".to_string(), line));
                }
                self.call_wasm_function(func, arg_vals, line)
            }
            Value::BoundMethod(method) =>
            {
                let mut args = smallvec::SmallVec::<[Value; 8]>::new();
                args.push(method.receiver.clone());
                args.extend(arg_vals.into_iter());
                self.call_value(method.func.clone(), args, line, block)
            }
            _ => Err(RuntimeError::simple(format!("Tried to call a non-function value: {}", func_val), line)),
        }
    }

    fn call_block_with_args(
        &mut self,
        closure: &Closure,
        saved_env: Rc<RefCell<Environment>>,
        args: &[Value],
        line: usize,
    ) -> EvalResult
    {
        let new_env = self.get_env(Some(saved_env.clone()), false);
        let mut arg_iter = args.iter();
        for param in &closure.params
        {
            if param.is_ref
            {
                let ref_val =
                    saved_env
                        .borrow_mut()
                        .promote(param.name)
                        .ok_or_else(|| RuntimeError::simple(format!(
                                "Undefined variable captured: {}",
                                symbol_name(param.name).as_str()
                            ), line))?;
                new_env.borrow_mut().define(param.name, ref_val);
            }
            else
            {
                let val = arg_iter.next().cloned().unwrap_or(Value::Nil);
                new_env.borrow_mut().define(param.name, val);
            }
        }

        let mut locals = HashSet::new();
        collect_declarations(&closure.body, &mut locals);
        for local in locals
        {
            let idx = local as usize;
            if idx >= new_env.borrow().values.len()
            {
                new_env.borrow_mut().define(local, Value::Uninitialized);
            }
        }

        let original_env = self.env.clone();
        self.env = new_env.clone();
        let result = self.eval(&closure.body, &mut []);
        self.env = original_env;
        self.recycle_env(new_env);
        result
    }

    fn apply_block_collection(
        &mut self,
        method: &str,
        target: BlockCollectionTarget,
        block: &Closure,
        saved_env: Rc<RefCell<Environment>>,
        line: usize,
    ) -> EvalResult
    {
        let collect_results = method == "each" || method == "map";
        let is_map = matches!(&target, BlockCollectionTarget::Map(_));
        let use_filter = method == "filter" && !is_map;
        let mut results = Vec::new();
        let mut f64_vals = Vec::new();
        let mut all_f64 = true;

        let mut visit =
            |args: &[Value], keep: Option<&Value>, use_filter: bool| -> Result<(), RuntimeError> {
                let result = self.call_block_with_args(block, saved_env.clone(), args, line)?;
                if use_filter
                {
                    if let Some(keep_val) = keep
                    {
                        let is_truthy = !matches!(result, Value::Boolean(false) | Value::Nil);
                        if is_truthy
                        {
                            if all_f64
                            {
                                if let Some(num) = int_value_as_f64(keep_val)
                                {
                                    f64_vals.push(num);
                                }
                                else
                                {
                                    all_f64 = false;
                                    results.extend(
                                        f64_vals.drain(..).map(|v| make_float(v, FloatKind::F64)),
                                    );
                                    results.push(keep_val.clone());
                                }
                            }
                            else
                            {
                                results.push(keep_val.clone());
                            }
                        }
                    }
                }
                else if collect_results
                {
                    if all_f64
                    {
                        if let Some(num) = int_value_as_f64(&result)
                        {
                            f64_vals.push(num);
                        }
                        else
                        {
                            all_f64 = false;
                            results
                                .extend(f64_vals.drain(..).map(|v| make_float(v, FloatKind::F64)));
                            results.push(result);
                        }
                    }
                    else
                    {
                        results.push(result);
                    }
                }
                Ok(())
            };

        let original = match target
        {
            BlockCollectionTarget::Array(arr) =>
            {
                let len = arr.borrow().len();
                for idx in 0..len
                {
                    let val = arr.borrow()[idx].clone();
                    visit(std::slice::from_ref(&val), Some(&val), use_filter)?;
                }
                Value::Array(arr)
            }
            BlockCollectionTarget::F32Array(arr) =>
            {
                let len = arr.borrow().len();
                for idx in 0..len
                {
                    let val = make_float(arr.borrow()[idx] as f64, FloatKind::F32);
                    visit(std::slice::from_ref(&val), Some(&val), use_filter)?;
                }
                Value::F32Array(arr)
            }
            BlockCollectionTarget::F64Array(arr) =>
            {
                let len = arr.borrow().len();
                for idx in 0..len
                {
                    let val = make_float(arr.borrow()[idx], FloatKind::F64);
                    visit(std::slice::from_ref(&val), Some(&val), use_filter)?;
                }
                Value::F64Array(arr)
            }
            BlockCollectionTarget::I32Array(arr) =>
            {
                let len = arr.borrow().len();
                for idx in 0..len
                {
                    let val = make_signed_int(arr.borrow()[idx] as i128, IntKind::I32);
                    visit(std::slice::from_ref(&val), Some(&val), use_filter)?;
                }
                Value::I32Array(arr)
            }
            BlockCollectionTarget::I64Array(arr) =>
            {
                let len = arr.borrow().len();
                for idx in 0..len
                {
                    let val = make_signed_int(arr.borrow()[idx] as i128, IntKind::I64);
                    visit(std::slice::from_ref(&val), Some(&val), use_filter)?;
                }
                Value::I64Array(arr)
            }
            BlockCollectionTarget::Map(map) =>
            {
                let keys: Vec<Rc<String>> = map.borrow().data.keys().cloned().collect();
                for key in keys
                {
                    let val = map.borrow().data.get(&key).cloned().unwrap_or(Value::Nil);
                    let args = [Value::String(key), val];
                    visit(&args, None, false)?;
                }
                Value::Map(map)
            }
        };

        if collect_results || use_filter
        {
            if all_f64 && !matches!(original, Value::Map(_))
            {
                Ok(Value::F64Array(Rc::new(RefCell::new(f64_vals))))
            }
            else
            {
                Ok(Value::Array(Rc::new(RefCell::new(results))))
            }
        }
        else
        {
            Ok(original)
        }
    }

    fn call_wasm_function(
        &mut self,
        func: Rc<WasmFunction>,
        arg_vals: smallvec::SmallVec<[Value; 8]>,
        line: usize,
    ) -> EvalResult
    {
        let mut module = func.module.borrow_mut();
        module.ensure_memory();
        let wasm_func = module
            .functions
            .get(&func.name)
            .ok_or_else(|| RuntimeError::simple(format!("Wasm function '{}' not found", func.name), line))?
            .clone();
        let func_type = module
            .func_types
            .get(&func.name)
            .ok_or_else(|| RuntimeError::simple(format!("Wasm function '{}' type not found", func.name), line))?
            .clone();

        let params = &func_type.params;
        let results = &func_type.results;
        if results.len() > 1
        {
            return Err(RuntimeError::simple("Wasm functions with multiple returns are not supported".to_string(), line));
        }

        let mut wasm_args: Vec<WasmValue> = Vec::new();
        let mut allocs: Vec<(i32, i32)> = Vec::new();
        let mut arg_index = 0usize;
        let mut param_index = 0usize;

        let estimated_params = arg_vals
            .iter()
            .map(|v| match v
            {
                Value::String(_) | Value::F64Array(_) | Value::Array(_) => 2,
                _ => 1,
            })
            .sum::<usize>();
        let wbindgen_mode = !params.is_empty()
            && params[0] == WasmValueType::I32
            && params.len() == estimated_params + 1
            && (results.is_empty() || results[0] == WasmValueType::I32);

        let mut retptr: Option<i32> = None;
        let mut retptr_alloc: Option<(i32, i32)> = None;
        if wbindgen_mode
        {
            if let Some(add) = module.wbindgen_add_to_stack_pointer.clone()
            {
                let mut result = [WasmValue::I32(0)];
                module
                    .call_func(&add, &[WasmValue::I32(-16)], &mut result)
                    .map_err(|message| RuntimeError::simple(message, line))?;
                let ptr = match result[0]
                {
                    WasmValue::I32(v) => v,
                    _ =>
                    {
                        return Err(RuntimeError::simple("Wasm stack adjust returned non-i32".to_string(), line));
                    }
                };
                retptr = Some(ptr);
            }
            else
            {
                let (alloc, alloc_name) = if let Some(func) = module.alloc.clone()
                {
                    (func, "alloc")
                }
                else if let Some(func) = module.wbindgen_malloc.clone()
                {
                    (func, "__wbindgen_malloc")
                }
                else
                {
                    return Err(RuntimeError::simple("Wasm module has no alloc export".to_string(), line));
                };
                let mut results = [WasmValue::I32(0)];
                let alloc_params = module
                    .func_types
                    .get(&intern::intern(alloc_name))
                    .map(|t| t.params.len())
                    .unwrap_or(1);
                let alloc_args = if alloc_params == 2
                {
                    vec![WasmValue::I32(8), WasmValue::I32(1)]
                }
                else
                {
                    vec![WasmValue::I32(8)]
                };
                module
                    .call_func(&alloc, &alloc_args, &mut results)
                    .map_err(|message| RuntimeError::simple(message, line))?;
                let ptr = match results[0]
                {
                    WasmValue::I32(v) => v,
                    _ =>
                    {
                        return Err(RuntimeError::simple("Wasm alloc returned non-i32".to_string(), line));
                    }
                };
                retptr = Some(ptr);
                retptr_alloc = Some((ptr, 8));
            }
            if let Some(ptr) = retptr
            {
                wasm_args.push(WasmValue::I32(ptr));
                param_index += 1;
            }
        }

        while arg_index < arg_vals.len()
        {
            if param_index >= params.len()
            {
                return Err(RuntimeError::simple("Wasm function argument count mismatch".to_string(), line));
            }
            let arg = &arg_vals[arg_index];
            match arg
            {
                Value::String(s) =>
                {
                    if param_index + 1 >= params.len()
                        || params[param_index] != WasmValueType::I32
                        || params[param_index + 1] != WasmValueType::I32
                    {
                        return Err(RuntimeError::simple("Wasm string arguments require two i32 params".to_string(), line));
                    }
                    let (alloc, alloc_name) = if let Some(func) = module.alloc.clone()
                    {
                        (func, "alloc")
                    }
                    else if let Some(func) = module.wbindgen_malloc.clone()
                    {
                        (func, "__wbindgen_malloc")
                    }
                    else
                    {
                        return Err(RuntimeError::simple("Wasm module has no alloc export".to_string(), line));
                    };
                    let bytes = s.as_bytes();
                    let mut results = [WasmValue::I32(0)];
                    let alloc_params = module
                        .func_types
                        .get(&intern::intern(alloc_name))
                        .map(|t| t.params.len())
                        .unwrap_or(1);
                    let alloc_args = if alloc_params == 2
                    {
                        vec![WasmValue::I32(bytes.len() as i32), WasmValue::I32(1)]
                    }
                    else
                    {
                        vec![WasmValue::I32(bytes.len() as i32)]
                    };
                    module
                        .call_func(&alloc, &alloc_args, &mut results)
                        .map_err(|message| RuntimeError::simple(message, line))?;
                    let ptr = match results[0]
                    {
                        WasmValue::I32(v) => v,
                        _ =>
                        {
                            return Err(RuntimeError::simple("Wasm alloc returned non-i32".to_string(), line));
                        }
                    };
                    let memory = module.memory_data_mut().ok_or_else(|| RuntimeError::simple("Wasm module has no memory export".to_string(), line))?;
                    let start = ptr as usize;
                    let end = start + bytes.len();
                    if end > memory.len()
                    {
                        return Err(RuntimeError::simple("Wasm memory overflow writing string".to_string(), line));
                    }
                    memory[start..end].copy_from_slice(bytes);
                    allocs.push((ptr, bytes.len() as i32));
                    wasm_args.push(WasmValue::I32(ptr));
                    wasm_args.push(WasmValue::I32(bytes.len() as i32));
                    arg_index += 1;
                    param_index += 2;
                }
                Value::F64Array(arr) =>
                {
                    if param_index + 1 >= params.len()
                        || params[param_index] != WasmValueType::I32
                        || params[param_index + 1] != WasmValueType::I32
                    {
                        return Err(RuntimeError::simple("Wasm f64 array arguments require two i32 params".to_string(), line));
                    }
                    let len = arr.borrow().len();
                    let len_i32 = i32::try_from(len).map_err(|_| RuntimeError::simple("Wasm array length too large".to_string(), line))?;
                    if len == 0
                    {
                        wasm_args.push(WasmValue::I32(0));
                        wasm_args.push(WasmValue::I32(0));
                        arg_index += 1;
                        param_index += 2;
                        continue;
                    }
                    let (alloc, alloc_name) = if let Some(func) = module.alloc.clone()
                    {
                        (func, "alloc")
                    }
                    else if let Some(func) = module.wbindgen_malloc.clone()
                    {
                        (func, "__wbindgen_malloc")
                    }
                    else
                    {
                        return Err(RuntimeError::simple("Wasm module has no alloc export".to_string(), line));
                    };
                    let byte_len = len.checked_mul(8).ok_or_else(|| RuntimeError::simple("Wasm array length too large".to_string(), line))?;
                    let mut results = [WasmValue::I32(0)];
                    let alloc_params = module
                        .func_types
                        .get(&intern::intern(alloc_name))
                        .map(|t| t.params.len())
                        .unwrap_or(1);
                    let alloc_args = if alloc_params == 2
                    {
                        vec![WasmValue::I32(byte_len as i32), WasmValue::I32(8)]
                    }
                    else
                    {
                        vec![WasmValue::I32(byte_len as i32)]
                    };
                    module
                        .call_func(&alloc, &alloc_args, &mut results)
                        .map_err(|message| RuntimeError::simple(message, line))?;
                    let ptr = match results[0]
                    {
                        WasmValue::I32(v) => v,
                        _ =>
                        {
                            return Err(RuntimeError::simple("Wasm alloc returned non-i32".to_string(), line));
                        }
                    };
                    let memory = module.memory_data_mut().ok_or_else(|| RuntimeError::simple("Wasm module has no memory export".to_string(), line))?;
                    let start = ptr as usize;
                    let end = start + byte_len;
                    if end > memory.len()
                    {
                        return Err(RuntimeError::simple("Wasm memory overflow writing array".to_string(), line));
                    }
                    let slice = arr.borrow();
                    for (idx, value) in slice.iter().enumerate()
                    {
                        let offset = start + idx * 8;
                        memory[offset..offset + 8].copy_from_slice(&value.to_le_bytes());
                    }
                    allocs.push((ptr, byte_len as i32));
                    wasm_args.push(WasmValue::I32(ptr));
                    wasm_args.push(WasmValue::I32(len_i32));
                    arg_index += 1;
                    param_index += 2;
                }
                Value::Array(arr) =>
                {
                    if param_index + 1 >= params.len()
                        || params[param_index] != WasmValueType::I32
                        || params[param_index + 1] != WasmValueType::I32
                    {
                        return Err(RuntimeError::simple("Wasm array arguments require two i32 params".to_string(), line));
                    }
                    let values = arr.borrow();
                    let len = values.len();
                    let len_i32 = i32::try_from(len).map_err(|_| RuntimeError::simple("Wasm array length too large".to_string(), line))?;
                    if len == 0
                    {
                        wasm_args.push(WasmValue::I32(0));
                        wasm_args.push(WasmValue::I32(0));
                        arg_index += 1;
                        param_index += 2;
                        continue;
                    }
                    let (alloc, alloc_name) = if let Some(func) = module.alloc.clone()
                    {
                        (func, "alloc")
                    }
                    else if let Some(func) = module.wbindgen_malloc.clone()
                    {
                        (func, "__wbindgen_malloc")
                    }
                    else
                    {
                        return Err(RuntimeError::simple("Wasm module has no alloc export".to_string(), line));
                    };
                    let byte_len = len.checked_mul(4).ok_or_else(|| RuntimeError::simple("Wasm array length too large".to_string(), line))?;
                    let mut results = [WasmValue::I32(0)];
                    let alloc_params = module
                        .func_types
                        .get(&intern::intern(alloc_name))
                        .map(|t| t.params.len())
                        .unwrap_or(1);
                    let alloc_args = if alloc_params == 2
                    {
                        vec![WasmValue::I32(byte_len as i32), WasmValue::I32(4)]
                    }
                    else
                    {
                        vec![WasmValue::I32(byte_len as i32)]
                    };
                    module
                        .call_func(&alloc, &alloc_args, &mut results)
                        .map_err(|message| RuntimeError::simple(message, line))?;
                    let ptr = match results[0]
                    {
                        WasmValue::I32(v) => v,
                        _ =>
                        {
                            return Err(RuntimeError::simple("Wasm alloc returned non-i32".to_string(), line));
                        }
                    };
                    let memory = module.memory_data_mut().ok_or_else(|| RuntimeError::simple("Wasm module has no memory export".to_string(), line))?;
                    let start = ptr as usize;
                    let end = start + byte_len;
                    if end > memory.len()
                    {
                        return Err(RuntimeError::simple("Wasm memory overflow writing array".to_string(), line));
                    }
                    for (idx, value) in values.iter().enumerate()
                    {
                        let num = int_value_as_i64(value).ok_or_else(|| RuntimeError::simple("Wasm array elements must be integers".to_string(), line))?;
                        if num < 0 || num > u32::MAX as i64
                        {
                            return Err(RuntimeError::simple("Wasm array elements must fit in u32".to_string(), line));
                        }
                        let bytes = (num as u32).to_le_bytes();
                        let offset = start + idx * 4;
                        memory[offset..offset + 4].copy_from_slice(&bytes);
                    }
                    allocs.push((ptr, byte_len as i32));
                    wasm_args.push(WasmValue::I32(ptr));
                    wasm_args.push(WasmValue::I32(len_i32));
                    arg_index += 1;
                    param_index += 2;
                }
                Value::Boolean(b) =>
                {
                    let val = if *b { 1 } else { 0 };
                    match params[param_index]
                    {
                        WasmValueType::I32 => wasm_args.push(WasmValue::I32(val)),
                        WasmValueType::I64 => wasm_args.push(WasmValue::I64(val as i64)),
                        _ =>
                        {
                            return Err(RuntimeError::simple("Wasm bool expects i32/i64 param".to_string(), line));
                        }
                    }
                    arg_index += 1;
                    param_index += 1;
                }
                Value::Float { value, .. } =>
                {
                    match params[param_index]
                    {
                        WasmValueType::F32 => wasm_args.push(WasmValue::F32(*value as f32)),
                        WasmValueType::F64 => wasm_args.push(WasmValue::F64(*value)),
                        _ =>
                        {
                            return Err(RuntimeError::simple("Wasm float expects f32/f64 param".to_string(), line));
                        }
                    }
                    arg_index += 1;
                    param_index += 1;
                }
                v =>
                {
                    let num = int_value_as_i64(v).ok_or_else(|| RuntimeError::simple("Wasm numeric arguments must be numbers".to_string(), line))?;
                    match params[param_index]
                    {
                        WasmValueType::I32 => wasm_args.push(WasmValue::I32(num as i32)),
                        WasmValueType::I64 => wasm_args.push(WasmValue::I64(num as i64)),
                        WasmValueType::F32 => wasm_args.push(WasmValue::F32(num as f32)),
                        WasmValueType::F64 => wasm_args.push(WasmValue::F64(num as f64)),
                    }
                    arg_index += 1;
                    param_index += 1;
                }
            }
        }
        if param_index != params.len()
        {
            return Err(RuntimeError::simple(format!(
                    "Wasm function argument count mismatch for '{}': expected {} params ({:?}), got {}",
                    func.name,
                    params.len(),
                    params,
                    param_index
                ), line));
        }

        let mut wasm_results = vec![WasmValue::I32(0); results.len()];
        module
            .call_func(&wasm_func, &wasm_args, &mut wasm_results)
            .map_err(|message| RuntimeError::simple(message, line))?;

        let (dealloc, dealloc_name) = if let Some(func) = module.dealloc.clone()
        {
            (Some(func), "dealloc")
        }
        else if let Some(func) = module.wbindgen_free.clone()
        {
            (Some(func), "__wbindgen_free")
        }
        else
        {
            (None, "")
        };
        if let Some(dealloc) = dealloc.as_ref()
        {
            for (ptr, len) in allocs
            {
                let dealloc_params = module
                    .func_types
                    .get(&intern::intern(dealloc_name))
                    .map(|t| t.params.len())
                    .unwrap_or(2);
                let args = if dealloc_params == 3
                {
                    vec![WasmValue::I32(ptr), WasmValue::I32(len), WasmValue::I32(1)]
                }
                else
                {
                    vec![WasmValue::I32(ptr), WasmValue::I32(len)]
                };
                let _ = module.call_func(dealloc, &args, &mut []);
            }
        }

        if let Some(ptr) = retptr
        {
            let memory = module.memory_data().ok_or_else(|| RuntimeError::simple("Wasm module has no memory export".to_string(), line))?;
            let result_str = {
                let start = ptr as usize;
                if start + 8 > memory.len()
                {
                    return Err(RuntimeError::simple("Wasm memory overflow reading return".to_string(), line));
                }
                let ptr_bytes = &memory[start..start + 4];
                let len_bytes = &memory[start + 4..start + 8];
                let str_ptr =
                    u32::from_le_bytes([ptr_bytes[0], ptr_bytes[1], ptr_bytes[2], ptr_bytes[3]]);
                let str_len =
                    u32::from_le_bytes([len_bytes[0], len_bytes[1], len_bytes[2], len_bytes[3]]);
                let start = str_ptr as usize;
                let end = start + str_len as usize;
                if end > memory.len()
                {
                    return Err(RuntimeError::simple("Wasm memory overflow reading string".to_string(), line));
                }
                Some((String::from_utf8_lossy(&memory[start..end]).to_string(), str_ptr, str_len))
            };
            if let Some(add) = module.wbindgen_add_to_stack_pointer.clone()
            {
                let _ = module.call_func(&add, &[WasmValue::I32(16)], &mut []);
            }
            if let Some(dealloc) = dealloc.as_ref()
            {
                if let Some((ptr, len)) = retptr_alloc
                {
                    let dealloc_params = module
                        .func_types
                        .get(&intern::intern(dealloc_name))
                        .map(|t| t.params.len())
                        .unwrap_or(2);
                    let args = if dealloc_params == 3
                    {
                        vec![WasmValue::I32(ptr), WasmValue::I32(len), WasmValue::I32(1)]
                    }
                    else
                    {
                        vec![WasmValue::I32(ptr), WasmValue::I32(len)]
                    };
                    let _ = module.call_func(dealloc, &args, &mut []);
                }
            }
            if let Some((s, str_ptr, str_len)) = result_str
            {
                if let Some(dealloc) = dealloc.as_ref()
                {
                    let dealloc_params = module
                        .func_types
                        .get(&intern::intern(dealloc_name))
                        .map(|t| t.params.len())
                        .unwrap_or(2);
                    let args = if dealloc_params == 3
                    {
                        vec![
                            WasmValue::I32(str_ptr as i32),
                            WasmValue::I32(str_len as i32),
                            WasmValue::I32(1),
                        ]
                    }
                    else
                    {
                        vec![
                            WasmValue::I32(str_ptr as i32),
                            WasmValue::I32(str_len as i32),
                        ]
                    };
                    let _ = module.call_func(dealloc, &args, &mut []);
                }
                return Ok(Value::String(intern::intern_owned(s)));
            }
        }

        if results.is_empty()
        {
            return Ok(Value::Nil);
        }
        let wasm_result = wasm_results.get(0).cloned().unwrap_or(WasmValue::I32(0));
        match (results[0], wasm_result)
        {
            (WasmValueType::I32, WasmValue::I32(v)) => Ok(make_signed_int(v as i128, IntKind::I32)),
            (WasmValueType::I64, WasmValue::I64(v)) =>
            {
                if module.memory.is_some() && func.name.ends_with("_str")
                {
                    let ptr = (v & 0xFFFF_FFFF) as u32;
                    let len = ((v >> 32) & 0xFFFF_FFFF) as u32;
                    let memory = module.memory_data().ok_or_else(|| RuntimeError::simple("Wasm module has no memory export".to_string(), line))?;
                    let start = ptr as usize;
                    let end = start + len as usize;
                    if end > memory.len()
                    {
                        return Err(RuntimeError::simple("Wasm memory overflow reading string".to_string(), line));
                    }
                    let s = String::from_utf8_lossy(&memory[start..end]).to_string();
                    Ok(Value::String(intern::intern_owned(s)))
                }
                else
                {
                    Ok(make_signed_int(v as i128, IntKind::I64))
                }
            }
            (WasmValueType::F32, WasmValue::F32(value)) =>
            {
                Ok(make_float(value as f64, FloatKind::F32))
            }
            (WasmValueType::F64, WasmValue::F64(value)) => Ok(make_float(value, FloatKind::F64)),
            _ => Ok(Value::Nil),
        }
    }

    fn invoke_function(
        &mut self,
        data: Rc<crate::value::FunctionData>,
        arg_vals: smallvec::SmallVec<[Value; 8]>,
        line: usize,
        block: Option<Rc<Closure>>,
    ) -> EvalResult
    {
        let arg_len = arg_vals.len();
        if arg_len < data.params.len()
        {
            let new_env = self.get_env(Some(data.env.clone()), true);
            let mut bound_args: Vec<(usize, Value)> = data
                .bound_args
                .iter()
                .map(|(slot, val)| (*slot, val.clone()))
                .collect();
            for (idx, (param, val)) in data.params.iter().zip(arg_vals.iter()).enumerate()
            {
                let coerced = coerce_param_value(&data.env, param, val.clone(), line)?;
                new_env.borrow_mut().define(param.name, coerced.clone());
                bound_args.push((data.param_offset + idx, coerced));
            }
            let num_bound = arg_vals.len();
            let remaining_params = data.params[num_bound..].to_vec();
            return Ok(Value::Function(Rc::new(crate::value::FunctionData {
                params: remaining_params,
                body: data.body.clone(),
                declarations: data.declarations.clone(),
                param_offset: data.param_offset + num_bound,
                is_simple: data.is_simple,
                uses_env: true,
                code: None,
                reg_code: None,
                fast_reg_code: None,
                const_pool: data.const_pool.clone(),
                bound_args: Rc::new(bound_args),
                env: new_env,
            })));
        }
        else if arg_len > data.params.len()
        {
            return Err(RuntimeError::simple("Too many arguments".to_string(), line));
        }

        let mut coerced_args: smallvec::SmallVec<[Value; 8]> = smallvec::SmallVec::new();
        for (param, val) in data.params.iter().zip(arg_vals.into_iter())
        {
            let coerced = coerce_param_value(&data.env, param, val, line)?;
            coerced_args.push(coerced);
        }

        // FULL CALL
        if let Some(fast) = &data.fast_reg_code
        {
            let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(
                Value::Uninitialized,
                data.declarations.len(),
            );
            self.ensure_slot_capacity(
                &mut new_slots,
                data.param_offset,
                data.params.len(),
                &data.bound_args,
            );
            self.apply_bound_args(&data.bound_args, &mut new_slots);
            for (i, val) in coerced_args.iter().cloned().enumerate()
            {
                new_slots[i + data.param_offset] = val;
            }
            if let Some(result) = try_execute_fast_float_reg(fast, &mut new_slots)
            {
                return Ok(result);
            }
        }
        if let Some(reg) = &data.reg_code
        {
            let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(
                Value::Uninitialized,
                data.declarations.len(),
            );
            self.ensure_slot_capacity(
                &mut new_slots,
                data.param_offset,
                data.params.len(),
                &data.bound_args,
            );
            self.apply_bound_args(&data.bound_args, &mut new_slots);
            for (i, val) in coerced_args.iter().cloned().enumerate()
            {
                new_slots[i + data.param_offset] = val;
            }
            return execute_reg_instructions(self, reg, &mut new_slots);
        }
        if let Some(code) = &data.code
        {
            let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(
                Value::Uninitialized,
                data.declarations.len(),
            );
            self.ensure_slot_capacity(
                &mut new_slots,
                data.param_offset,
                data.params.len(),
                &data.bound_args,
            );
            self.apply_bound_args(&data.bound_args, &mut new_slots);
            for (i, val) in coerced_args.iter().cloned().enumerate()
            {
                new_slots[i + data.param_offset] = val;
            }
            if data.uses_env
            {
                let new_env = self.get_env(Some(data.env.clone()), false);
                for (param, val) in data.params.iter().zip(coerced_args.iter().cloned())
                {
                    new_env.borrow_mut().define(param.name, val);
                }
                let original_env = self.env.clone();
                self.env = new_env.clone();
                let result = execute_instructions(self, code, &data.const_pool, &mut new_slots);
                self.env = original_env;
                self.recycle_env(new_env);
                return result;
            }
            return execute_instructions(self, code, &data.const_pool, &mut new_slots);
        }

        if data.is_simple
        {
            let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(
                Value::Uninitialized,
                data.declarations.len(),
            );
            self.ensure_slot_capacity(
                &mut new_slots,
                data.param_offset,
                data.params.len(),
                &data.bound_args,
            );
            self.apply_bound_args(&data.bound_args, &mut new_slots);
            for (i, val) in coerced_args.iter().cloned().enumerate()
            {
                new_slots[i + data.param_offset] = val;
            }
            if data.uses_env
            {
                let new_env = self.get_env(Some(data.env.clone()), false);
                for (param, val) in data.params.iter().zip(coerced_args.iter().cloned())
                {
                    new_env.borrow_mut().define(param.name, val);
                }
                let original_env = self.env.clone();
                self.env = new_env.clone();
                let result = handle_eval_result(self.eval(&data.body, &mut new_slots))?;
                self.env = original_env;
                self.recycle_env(new_env);
                return Ok(result);
            }
            return handle_eval_result(self.eval(&data.body, &mut new_slots));
        }

        let block_entry = if let Some(closure) = block
        {
            Some((closure, self.env.clone()))
        }
        else
        {
            None
        };
        self.block_stack.push(block_entry);

        let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(
            Value::Uninitialized,
            data.declarations.len(),
        );
        for (i, val) in coerced_args.into_iter().enumerate()
        {
            new_slots[i + data.param_offset] = val;
        }

        let result = if data.uses_env
        {
            let new_env = self.get_env(Some(data.env.clone()), false);
            for (i, param) in data.params.iter().enumerate()
            {
                let val = new_slots[i + data.param_offset].clone();
                new_env.borrow_mut().define(param.name, val);
            }
            let original_env = self.env.clone();
            self.env = new_env.clone();
            let result = handle_eval_result(self.eval(&data.body, &mut new_slots))?;
            self.env = original_env;
            self.recycle_env(new_env);
            result
        }
        else
        {
            handle_eval_result(self.eval(&data.body, &mut new_slots))?
        };
        self.block_stack.pop();
        Ok(result)
    }

    pub fn call_value_from_host(
        &mut self,
        func_val: Value,
        args: Vec<Value>,
    ) -> Result<Value, String>
    {
        let arg_vals: smallvec::SmallVec<[Value; 8]> = args.into_iter().collect();
        self.call_value(func_val, arg_vals, 0, None)
            .map_err(|err| {
                let loc = if err.column > 0
                {
                    format!("{}:{}", err.line, err.column)
                }
                else
                {
                    err.line.to_string()
                };
                format!("Error at line {}: {}", loc, err.message)
            })
    }




}
