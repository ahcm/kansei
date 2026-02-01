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
use rustc_hash::FxHashMap;
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

fn native_int64_parse(args: &[Value]) -> Result<Value, String>
{
    parse_signed_int(args, IntKind::I64, "Int64")
}

fn native_float64_parse(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "Float64.parse expects 1 argument".to_string())?;
    match arg
    {
        Value::String(s) => s
            .parse::<f64>()
            .map(|value| make_float(value, FloatKind::F64))
            .map_err(|_| "Float64.parse failed to parse string".to_string()),
        _ => Err("Float64.parse expects a string argument".to_string()),
    }
}

fn native_float64_sqrt(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "Float64.sqrt expects 1 argument".to_string())?;
    let value = match arg
    {
        Value::Float { value, .. } => *value,
        v => int_value_as_f64(v).ok_or_else(|| "Float64.sqrt expects a number".to_string())?,
    };
    Ok(make_float(value.sqrt(), FloatKind::F64))
}

fn native_float32_parse(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "Float32.parse expects 1 argument".to_string())?;
    match arg
    {
        Value::String(s) => s
            .parse::<f32>()
            .map(|value| make_float(value as f64, FloatKind::F32))
            .map_err(|_| "Float32.parse failed to parse string".to_string()),
        _ => Err("Float32.parse expects a string argument".to_string()),
    }
}

fn native_float32_sqrt(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "Float32.sqrt expects 1 argument".to_string())?;
    let value = match arg
    {
        Value::Float { value, .. } => *value,
        v => int_value_as_f64(v).ok_or_else(|| "Float32.sqrt expects a number".to_string())?,
    };
    Ok(make_float((value as f32).sqrt() as f64, FloatKind::F32))
}

fn native_float128_parse(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "Float128.parse expects 1 argument".to_string())?;
    match arg
    {
        Value::String(s) => s
            .parse::<f64>()
            .map(|value| make_float(value, FloatKind::F128))
            .map_err(|_| "Float128.parse failed to parse string".to_string()),
        _ => Err("Float128.parse expects a string argument".to_string()),
    }
}

fn native_float128_sqrt(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "Float128.sqrt expects 1 argument".to_string())?;
    let value = match arg
    {
        Value::Float { value, .. } => *value,
        v => int_value_as_f64(v).ok_or_else(|| "Float128.sqrt expects a number".to_string())?,
    };
    Ok(make_float(value.sqrt(), FloatKind::F128))
}

fn native_f64(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("f64 expects 1 argument".to_string());
    }
    cast_f64_value(&args[0])
}

fn native_f32(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("f32 expects 1 argument".to_string());
    }
    cast_f32_value(&args[0])
}

fn native_i64(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("i64 expects 1 argument".to_string());
    }
    cast_i64_value(&args[0])
}

fn native_i32(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("i32 expects 1 argument".to_string());
    }
    cast_i32_value(&args[0])
}

fn native_u64(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("u64 expects 1 argument".to_string());
    }
    cast_u64_value(&args[0])
}

fn native_u32(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("u32 expects 1 argument".to_string());
    }
    cast_u32_value(&args[0])
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
        Value::Mmap(mmap) => Ok(mmap.as_ref().to_vec()),
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
            let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let lexer = crate::lexer::Lexer::new(&expr_str);
                let mut parser = crate::parser::Parser::new(lexer);
                parser.parse()
            }));
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

fn native_int8_parse(args: &[Value]) -> Result<Value, String>
{
    parse_signed_int(args, IntKind::I8, "Int8")
}
fn native_int16_parse(args: &[Value]) -> Result<Value, String>
{
    parse_signed_int(args, IntKind::I16, "Int16")
}
fn native_int32_parse(args: &[Value]) -> Result<Value, String>
{
    parse_signed_int(args, IntKind::I32, "Int32")
}
fn native_int128_parse(args: &[Value]) -> Result<Value, String>
{
    parse_signed_int(args, IntKind::I128, "Int128")
}

fn native_uint8_parse(args: &[Value]) -> Result<Value, String>
{
    parse_unsigned_int(args, IntKind::U8, "Uint8")
}
fn native_uint16_parse(args: &[Value]) -> Result<Value, String>
{
    parse_unsigned_int(args, IntKind::U16, "Uint16")
}
fn native_uint32_parse(args: &[Value]) -> Result<Value, String>
{
    parse_unsigned_int(args, IntKind::U32, "Uint32")
}
fn native_uint64_parse(args: &[Value]) -> Result<Value, String>
{
    parse_unsigned_int(args, IntKind::U64, "Uint64")
}
fn native_uint128_parse(args: &[Value]) -> Result<Value, String>
{
    parse_unsigned_int(args, IntKind::U128, "Uint128")
}

fn build_int64_module() -> Value
{
    let mut int64_map = FxHashMap::default();
    int64_map.insert(intern::intern("parse"), Value::NativeFunction(native_int64_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(int64_map))))
}

fn build_int8_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_int8_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn build_int16_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_int16_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn build_int32_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_int32_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn build_int128_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_int128_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn build_uint8_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint8_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn build_uint16_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint16_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn build_uint32_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint32_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn build_uint64_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint64_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn build_uint128_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint128_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn build_float32_module() -> Value
{
    let mut float32_map = FxHashMap::default();
    float32_map.insert(intern::intern("parse"), Value::NativeFunction(native_float32_parse));
    float32_map.insert(intern::intern("sqrt"), Value::NativeFunction(native_float32_sqrt));
    Value::Map(Rc::new(RefCell::new(MapValue::new(float32_map))))
}

fn build_float64_module() -> Value
{
    let mut float64_map = FxHashMap::default();
    float64_map.insert(intern::intern("parse"), Value::NativeFunction(native_float64_parse));
    float64_map.insert(intern::intern("sqrt"), Value::NativeFunction(native_float64_sqrt));
    Value::Map(Rc::new(RefCell::new(MapValue::new(float64_map))))
}

fn build_float128_module() -> Value
{
    let mut float128_map = FxHashMap::default();
    float128_map.insert(intern::intern("parse"), Value::NativeFunction(native_float128_parse));
    float128_map.insert(intern::intern("sqrt"), Value::NativeFunction(native_float128_sqrt));
    Value::Map(Rc::new(RefCell::new(MapValue::new(float128_map))))
}

fn build_std_module() -> Value
{
    let mut std_map = FxHashMap::default();
    std_map.insert(intern::intern("Int8"), build_int8_module());
    std_map.insert(intern::intern("Int16"), build_int16_module());
    std_map.insert(intern::intern("Int32"), build_int32_module());
    std_map.insert(intern::intern("Int64"), build_int64_module());
    std_map.insert(intern::intern("Int128"), build_int128_module());
    std_map.insert(intern::intern("Uint8"), build_uint8_module());
    std_map.insert(intern::intern("Uint16"), build_uint16_module());
    std_map.insert(intern::intern("Uint32"), build_uint32_module());
    std_map.insert(intern::intern("Uint64"), build_uint64_module());
    std_map.insert(intern::intern("Uint128"), build_uint128_module());
    std_map.insert(intern::intern("Float32"), build_float32_module());
    std_map.insert(intern::intern("Float64"), build_float64_module());
    std_map.insert(intern::intern("Float128"), build_float128_module());
    std_map.insert(intern::intern("IO"), build_io_module());
    std_map.insert(intern::intern("OS"), build_os_module());
    std_map.insert(intern::intern("log"), build_log_module());
    std_map.insert(intern::intern("File"), build_file_module());
    std_map.insert(intern::intern("lib"), build_lib_module());
    std_map.insert(intern::intern("simd"), build_simd_module());
    std_map.insert(intern::intern("kansei"), build_kansei_module());
    std_map.insert(intern::intern("parallel"), build_parallel_module());
    std_map.insert(intern::intern("wasm"), build_wasm_module());
    std_map.insert(intern::intern("collect"), Value::NativeFunction(native_collect));
    std_map.insert(intern::intern("f64"), Value::NativeFunction(native_f64));
    std_map.insert(intern::intern("f32"), Value::NativeFunction(native_f32));
    std_map.insert(intern::intern("i64"), Value::NativeFunction(native_i64));
    std_map.insert(intern::intern("i32"), Value::NativeFunction(native_i32));
    std_map.insert(intern::intern("u64"), Value::NativeFunction(native_u64));
    std_map.insert(intern::intern("u32"), Value::NativeFunction(native_u32));
    Value::Map(Rc::new(RefCell::new(MapValue::new(std_map))))
}

fn normalize_float_value(value: f64, kind: FloatKind) -> f64
{
    match kind
    {
        FloatKind::F32 => (value as f32) as f64,
        FloatKind::F64 | FloatKind::F128 => value,
    }
}

fn clone_value(value: &Value) -> Value
{
    deep_clone_value(value)
}

fn promote_float_kind(left: FloatKind, right: FloatKind) -> FloatKind
{
    let rank = |kind| match kind
    {
        FloatKind::F32 => 0,
        FloatKind::F64 => 1,
        FloatKind::F128 => 2,
    };
    if rank(left) >= rank(right)
    {
        left
    }
    else
    {
        right
    }
}

fn pow_signed_int(base: i128, exp: i128, kind: IntKind) -> Value
{
    if exp < 0
    {
        return make_float((base as f64).powf(exp as f64), FloatKind::F64);
    }
    if let Ok(exp_u32) = u32::try_from(exp)
    {
        make_signed_int(base.pow(exp_u32), kind)
    }
    else
    {
        make_float((base as f64).powf(exp as f64), FloatKind::F64)
    }
}

fn pow_unsigned_int(base: u128, exp: u128, kind: IntKind) -> Value
{
    if let Ok(exp_u32) = u32::try_from(exp)
    {
        make_unsigned_int(base.pow(exp_u32), kind)
    }
    else
    {
        make_float((base as f64).powf(exp as f64), FloatKind::F64)
    }
}

#[derive(Debug, Clone, Copy)]
enum BinOpKind
{
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Eq,
    Gt,
    Lt,
}

fn eval_binop(op: BinOpKind, l: Value, r: Value) -> EvalResult
{
    let res = match (l, r)
    {
        (
            Value::Integer {
                value: i1,
                kind: k1,
            },
            Value::Integer {
                value: i2,
                kind: k2,
            },
        ) =>
        {
            let kind = signed_kind_for_bits(int_kind_bits(k1).max(int_kind_bits(k2)));
            match op
            {
                BinOpKind::Add => make_signed_int(i1 + i2, kind),
                BinOpKind::Sub => make_signed_int(i1 - i2, kind),
                BinOpKind::Mul => make_signed_int(i1 * i2, kind),
                BinOpKind::Div => make_signed_int(i1 / i2, kind),
                BinOpKind::Pow => pow_signed_int(i1, i2, kind),
                BinOpKind::Eq => Value::Boolean(i1 == i2),
                BinOpKind::Gt => Value::Boolean(i1 > i2),
                BinOpKind::Lt => Value::Boolean(i1 < i2),
            }
        }
        (
            Value::Unsigned {
                value: u1,
                kind: k1,
            },
            Value::Unsigned {
                value: u2,
                kind: k2,
            },
        ) =>
        {
            let kind = unsigned_kind_for_bits(int_kind_bits(k1).max(int_kind_bits(k2)));
            match op
            {
                BinOpKind::Add => make_unsigned_int(u1 + u2, kind),
                BinOpKind::Sub => make_unsigned_int(u1 - u2, kind),
                BinOpKind::Mul => make_unsigned_int(u1 * u2, kind),
                BinOpKind::Div => make_unsigned_int(u1 / u2, kind),
                BinOpKind::Pow => pow_unsigned_int(u1, u2, kind),
                BinOpKind::Eq => Value::Boolean(u1 == u2),
                BinOpKind::Gt => Value::Boolean(u1 > u2),
                BinOpKind::Lt => Value::Boolean(u1 < u2),
            }
        }
        (Value::Integer { value: i1, .. }, Value::Unsigned { value: u2, .. }) =>
        {
            let u2_i = i128::try_from(u2).map_err(|_| RuntimeError::simple("Unsigned value too large for signed operation".to_string(), 0))?;
            match op
            {
                BinOpKind::Add => make_signed_int(i1 + u2_i, IntKind::I128),
                BinOpKind::Sub => make_signed_int(i1 - u2_i, IntKind::I128),
                BinOpKind::Mul => make_signed_int(i1 * u2_i, IntKind::I128),
                BinOpKind::Div => make_signed_int(i1 / u2_i, IntKind::I128),
                BinOpKind::Pow => pow_signed_int(i1, u2_i, IntKind::I128),
                BinOpKind::Eq => Value::Boolean(i1 == u2_i),
                BinOpKind::Gt => Value::Boolean(i1 > u2_i),
                BinOpKind::Lt => Value::Boolean(i1 < u2_i),
            }
        }
        (Value::Unsigned { value: u1, .. }, Value::Integer { value: i2, .. }) =>
        {
            let u1_i = i128::try_from(u1).map_err(|_| RuntimeError::simple("Unsigned value too large for signed operation".to_string(), 0))?;
            match op
            {
                BinOpKind::Add => make_signed_int(u1_i + i2, IntKind::I128),
                BinOpKind::Sub => make_signed_int(u1_i - i2, IntKind::I128),
                BinOpKind::Mul => make_signed_int(u1_i * i2, IntKind::I128),
                BinOpKind::Div => make_signed_int(u1_i / i2, IntKind::I128),
                BinOpKind::Pow => pow_signed_int(u1_i, i2, IntKind::I128),
                BinOpKind::Eq => Value::Boolean(u1_i == i2),
                BinOpKind::Gt => Value::Boolean(u1_i > i2),
                BinOpKind::Lt => Value::Boolean(u1_i < i2),
            }
        }
        (
            Value::Float {
                value: f1,
                kind: k1,
            },
            Value::Float {
                value: f2,
                kind: k2,
            },
        ) =>
        {
            let kind = promote_float_kind(k1, k2);
            match op
            {
                BinOpKind::Add => make_float(f1 + f2, kind),
                BinOpKind::Sub => make_float(f1 - f2, kind),
                BinOpKind::Mul => make_float(f1 * f2, kind),
                BinOpKind::Div => make_float(f1 / f2, kind),
                BinOpKind::Pow => make_float(f1.powf(f2), kind),
                BinOpKind::Eq => Value::Boolean(f1 == f2),
                BinOpKind::Gt => Value::Boolean(f1 > f2),
                BinOpKind::Lt => Value::Boolean(f1 < f2),
            }
        }
        (v @ Value::Integer { .. }, Value::Float { value: f, kind })
        | (v @ Value::Unsigned { .. }, Value::Float { value: f, kind }) =>
        {
            let f1 = int_value_as_f64(&v).unwrap_or(0.0);
            match op
            {
                BinOpKind::Add => make_float(f1 + f, kind),
                BinOpKind::Sub => make_float(f1 - f, kind),
                BinOpKind::Mul => make_float(f1 * f, kind),
                BinOpKind::Div => make_float(f1 / f, kind),
                BinOpKind::Pow => make_float(f1.powf(f), kind),
                BinOpKind::Eq => Value::Boolean(f1 == f),
                BinOpKind::Gt => Value::Boolean(f1 > f),
                BinOpKind::Lt => Value::Boolean(f1 < f),
            }
        }
        (Value::Float { value: f, kind }, v @ Value::Integer { .. })
        | (Value::Float { value: f, kind }, v @ Value::Unsigned { .. }) =>
        {
            let f2 = int_value_as_f64(&v).unwrap_or(0.0);
            match op
            {
                BinOpKind::Add => make_float(f + f2, kind),
                BinOpKind::Sub => make_float(f - f2, kind),
                BinOpKind::Mul => make_float(f * f2, kind),
                BinOpKind::Div => make_float(f / f2, kind),
                BinOpKind::Pow => make_float(f.powf(f2), kind),
                BinOpKind::Eq => Value::Boolean(f == f2),
                BinOpKind::Gt => Value::Boolean(f > f2),
                BinOpKind::Lt => Value::Boolean(f < f2),
            }
        }
        (Value::String(s1), Value::String(s2)) => match op
        {
            BinOpKind::Add =>
            {
                let mut out = s1.clone();
                Rc::make_mut(&mut out).push_str(&s2);
                Value::String(out)
            }
            _ =>
            {
                return Err(RuntimeError::simple("Invalid types for operation".to_string(), 0));
            }
        },
        (Value::String(s), v2) => match op
        {
            BinOpKind::Add =>
            {
                let mut out = s.clone();
                Rc::make_mut(&mut out).push_str(&v2.inspect());
                Value::String(out)
            }
            _ =>
            {
                return Err(RuntimeError::simple("Invalid types for operation".to_string(), 0));
            }
        },
        _ =>
        {
            return Err(RuntimeError::simple("Invalid types for operation".to_string(), 0));
        }
    };
    Ok(res)
}

fn eval_cached_binop(
    op: BinOpKind,
    cache: &Rc<RefCell<BinaryOpCache>>,
    l: Value,
    r: Value,
) -> EvalResult
{
    let cached = cache.borrow().kind.clone();
    if let Some(kind) = cached.clone()
    {
        match kind
        {
            BinaryOpCacheKind::Float =>
            {
                if let (
                    Value::Float {
                        value: f1,
                        kind: k1,
                    },
                    Value::Float {
                        value: f2,
                        kind: k2,
                    },
                ) = (&l, &r)
                {
                    let kind = promote_float_kind(*k1, *k2);
                    let res = match op
                    {
                        BinOpKind::Add => make_float(f1 + f2, kind),
                        BinOpKind::Sub => make_float(f1 - f2, kind),
                        BinOpKind::Mul => make_float(f1 * f2, kind),
                        BinOpKind::Div => make_float(f1 / f2, kind),
                        BinOpKind::Pow => make_float(f1.powf(*f2), kind),
                        BinOpKind::Eq => Value::Boolean(f1 == f2),
                        BinOpKind::Gt => Value::Boolean(f1 > f2),
                        BinOpKind::Lt => Value::Boolean(f1 < f2),
                    };
                    cache.borrow_mut().hits += 1;
                    return Ok(res);
                }
            }
            BinaryOpCacheKind::Int =>
            {
                if let (
                    Value::Integer {
                        value: i1,
                        kind: k1,
                    },
                    Value::Integer {
                        value: i2,
                        kind: k2,
                    },
                ) = (&l, &r)
                {
                    let kind = signed_kind_for_bits(int_kind_bits(*k1).max(int_kind_bits(*k2)));
                    let res = match op
                    {
                        BinOpKind::Add => make_signed_int(*i1 + *i2, kind),
                        BinOpKind::Sub => make_signed_int(*i1 - *i2, kind),
                        BinOpKind::Mul => make_signed_int(*i1 * *i2, kind),
                        BinOpKind::Div => make_signed_int(*i1 / *i2, kind),
                        BinOpKind::Pow => pow_signed_int(*i1, *i2, kind),
                        BinOpKind::Eq => Value::Boolean(i1 == i2),
                        BinOpKind::Gt => Value::Boolean(i1 > i2),
                        BinOpKind::Lt => Value::Boolean(i1 < i2),
                    };
                    cache.borrow_mut().hits += 1;
                    return Ok(res);
                }
            }
            BinaryOpCacheKind::Uint =>
            {
                if let (
                    Value::Unsigned {
                        value: u1,
                        kind: k1,
                    },
                    Value::Unsigned {
                        value: u2,
                        kind: k2,
                    },
                ) = (&l, &r)
                {
                    let kind = unsigned_kind_for_bits(int_kind_bits(*k1).max(int_kind_bits(*k2)));
                    let res = match op
                    {
                        BinOpKind::Add => make_unsigned_int(*u1 + *u2, kind),
                        BinOpKind::Sub => make_unsigned_int(*u1 - *u2, kind),
                        BinOpKind::Mul => make_unsigned_int(*u1 * *u2, kind),
                        BinOpKind::Div => make_unsigned_int(*u1 / *u2, kind),
                        BinOpKind::Pow => pow_unsigned_int(*u1, *u2, kind),
                        BinOpKind::Eq => Value::Boolean(u1 == u2),
                        BinOpKind::Gt => Value::Boolean(u1 > u2),
                        BinOpKind::Lt => Value::Boolean(u1 < u2),
                    };
                    cache.borrow_mut().hits += 1;
                    return Ok(res);
                }
            }
        }
    }

    cache.borrow_mut().misses += 1;
    let res = eval_binop(op, l, r)?;
    let new_kind = match (&res, &cached)
    {
        (Value::Float { .. }, _) => Some(BinaryOpCacheKind::Float),
        (Value::Integer { .. }, _) => Some(BinaryOpCacheKind::Int),
        (Value::Unsigned { .. }, _) => Some(BinaryOpCacheKind::Uint),
        _ => None,
    };
    if let Some(kind) = new_kind
    {
        cache.borrow_mut().kind = Some(kind);
    }
    Ok(res)
}
fn make_float(value: f64, kind: FloatKind) -> Value
{
    Value::Float {
        value: normalize_float_value(value, kind),
        kind,
    }
}

fn reg_binop_from_op(op: &Op) -> Option<RegBinOp>
{
    match op
    {
        Op::Add => Some(RegBinOp::Add),
        Op::Subtract => Some(RegBinOp::Sub),
        Op::Multiply => Some(RegBinOp::Mul),
        Op::Divide => Some(RegBinOp::Div),
        Op::Power => Some(RegBinOp::Pow),
        Op::Equal => Some(RegBinOp::Eq),
        Op::GreaterThan => Some(RegBinOp::Gt),
        Op::LessThan => Some(RegBinOp::Lt),
        _ => None,
    }
}

fn int_kind_bits(kind: IntKind) -> u32
{
    match kind
    {
        IntKind::I8 | IntKind::U8 => 8,
        IntKind::I16 | IntKind::U16 => 16,
        IntKind::I32 | IntKind::U32 => 32,
        IntKind::I64 | IntKind::U64 => 64,
        IntKind::I128 | IntKind::U128 => 128,
    }
}

fn signed_kind_for_bits(bits: u32) -> IntKind
{
    match bits
    {
        8 => IntKind::I8,
        16 => IntKind::I16,
        32 => IntKind::I32,
        64 => IntKind::I64,
        _ => IntKind::I128,
    }
}

fn unsigned_kind_for_bits(bits: u32) -> IntKind
{
    match bits
    {
        8 => IntKind::U8,
        16 => IntKind::U16,
        32 => IntKind::U32,
        64 => IntKind::U64,
        _ => IntKind::U128,
    }
}

fn make_signed_int(value: i128, kind: IntKind) -> Value
{
    Value::Integer { value, kind }
}

fn make_unsigned_int(value: u128, kind: IntKind) -> Value
{
    Value::Unsigned { value, kind }
}

fn cast_f64_value(value: &Value) -> Result<Value, String>
{
    match value
    {
        Value::Float { value, .. } =>
        {
            Ok(Value::Float {
                value: *value,
                kind: FloatKind::F64,
            })
        }
        Value::Integer { value, .. } =>
        {
            Ok(Value::Float {
                value: *value as f64,
                kind: FloatKind::F64,
            })
        }
        Value::Unsigned { value, .. } =>
        {
            Ok(Value::Float {
                value: *value as f64,
                kind: FloatKind::F64,
            })
        }
        Value::String(s) => s
            .parse::<f64>()
            .map(|value| Value::Float {
                value,
                kind: FloatKind::F64,
            })
            .map_err(|_| "f64 expects a numeric string".to_string()),
        _ => Err("f64 expects a number or numeric string".to_string()),
    }
}

fn cast_f32_value(value: &Value) -> Result<Value, String>
{
    match value
    {
        Value::Float { value, .. } =>
        {
            Ok(Value::Float {
                value: *value as f32 as f64,
                kind: FloatKind::F32,
            })
        }
        Value::Integer { value, .. } =>
        {
            Ok(Value::Float {
                value: *value as f32 as f64,
                kind: FloatKind::F32,
            })
        }
        Value::Unsigned { value, .. } =>
        {
            Ok(Value::Float {
                value: *value as f32 as f64,
                kind: FloatKind::F32,
            })
        }
        Value::String(s) => s
            .parse::<f32>()
            .map(|value| Value::Float {
                value: value as f64,
                kind: FloatKind::F32,
            })
            .map_err(|_| "f32 expects a numeric string".to_string()),
        _ => Err("f32 expects a number or numeric string".to_string()),
    }
}

fn cast_i64_value(value: &Value) -> Result<Value, String>
{
    match value
    {
        Value::Integer { value, .. } => i64::try_from(*value)
            .map(|value| make_signed_int(value as i128, IntKind::I64))
            .map_err(|_| "i64 out of range".to_string()),
        Value::Unsigned { value, .. } => i64::try_from(*value)
            .map(|value| make_signed_int(value as i128, IntKind::I64))
            .map_err(|_| "i64 out of range".to_string()),
        Value::String(s) => s
            .parse::<i64>()
            .map(|value| make_signed_int(value as i128, IntKind::I64))
            .map_err(|_| "i64 expects an integer string".to_string()),
        Value::Float { .. } =>
        {
            Err("i64 does not accept float values (rounding not specified)".to_string())
        }
        _ => Err("i64 expects an integer or integer string".to_string()),
    }
}

fn cast_i32_value(value: &Value) -> Result<Value, String>
{
    match value
    {
        Value::Integer { value, .. } => i32::try_from(*value)
            .map(|value| make_signed_int(value as i128, IntKind::I32))
            .map_err(|_| "i32 out of range".to_string()),
        Value::Unsigned { value, .. } => i32::try_from(*value)
            .map(|value| make_signed_int(value as i128, IntKind::I32))
            .map_err(|_| "i32 out of range".to_string()),
        Value::String(s) => s
            .parse::<i32>()
            .map(|value| make_signed_int(value as i128, IntKind::I32))
            .map_err(|_| "i32 expects an integer string".to_string()),
        Value::Float { .. } =>
        {
            Err("i32 does not accept float values (rounding not specified)".to_string())
        }
        _ => Err("i32 expects an integer or integer string".to_string()),
    }
}

fn cast_u64_value(value: &Value) -> Result<Value, String>
{
    match value
    {
        Value::Unsigned { value, .. } =>
        {
            Ok(make_unsigned_int(*value, IntKind::U64))
        }
        Value::Integer { value, .. } =>
        {
            if *value < 0
            {
                return Err("u64 out of range".to_string());
            }
            u64::try_from(*value)
                .map(|value| make_unsigned_int(value as u128, IntKind::U64))
                .map_err(|_| "u64 out of range".to_string())
        }
        Value::String(s) => s
            .parse::<u64>()
            .map(|value| make_unsigned_int(value as u128, IntKind::U64))
            .map_err(|_| "u64 expects an unsigned integer string".to_string()),
        Value::Float { .. } =>
        {
            Err("u64 does not accept float values (rounding not specified)".to_string())
        }
        _ => Err("u64 expects an integer or integer string".to_string()),
    }
}

fn cast_u32_value(value: &Value) -> Result<Value, String>
{
    match value
    {
        Value::Unsigned { value, .. } =>
        {
            if *value > u32::MAX as u128
            {
                return Err("u32 out of range".to_string());
            }
            Ok(make_unsigned_int(*value, IntKind::U32))
        }
        Value::Integer { value, .. } =>
        {
            if *value < 0
            {
                return Err("u32 out of range".to_string());
            }
            u32::try_from(*value)
                .map(|value| make_unsigned_int(value as u128, IntKind::U32))
                .map_err(|_| "u32 out of range".to_string())
        }
        Value::String(s) => s
            .parse::<u32>()
            .map(|value| make_unsigned_int(value as u128, IntKind::U32))
            .map_err(|_| "u32 expects an unsigned integer string".to_string()),
        Value::Float { .. } =>
        {
            Err("u32 does not accept float values (rounding not specified)".to_string())
        }
        _ => Err("u32 expects an integer or integer string".to_string()),
    }
}

fn int_value_as_f64(value: &Value) -> Option<f64>
{
    match value
    {
        Value::Integer { value, .. } => Some(*value as f64),
        Value::Unsigned { value, .. } => Some(*value as f64),
        Value::Float { value, .. } => Some(*value),
        _ => None,
    }
}

fn int_value_as_i64(value: &Value) -> Option<i64>
{
    match value
    {
        Value::Integer { value, .. } => i64::try_from(*value).ok(),
        Value::Unsigned { value, .. } => i64::try_from(*value).ok(),
        _ => None,
    }
}

fn default_int(value: i128) -> Value
{
    make_signed_int(value, IntKind::I64)
}

fn int_value_as_usize(value: &Value) -> Option<usize>
{
    match value
    {
        Value::Integer { value, .. } if *value >= 0 => usize::try_from(*value).ok(),
        Value::Unsigned { value, .. } => usize::try_from(*value).ok(),
        _ => None,
    }
}

fn number_to_usize(value: &Value) -> Option<usize>
{
    match value
    {
        Value::Float { value, .. } if *value >= 0.0 => Some(*value as usize),
        _ => int_value_as_usize(value),
    }
}

fn collect_declarations(expr: &Expr, decls: &mut HashSet<SymbolId>)
{
    match &expr.kind
    {
        ExprKind::Assignment { name, .. } =>
        {
            decls.insert(name.clone());
        }
        ExprKind::FunctionDef { name, .. } =>
        {
            decls.insert(name.clone());
        }
        ExprKind::AnonymousFunction { .. } =>
        {}
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            collect_declarations(target, decls);
            collect_declarations(index, decls);
            collect_declarations(value, decls);
        }
        ExprKind::Block(stmts) =>
        {
            for stmt in stmts
            {
                collect_declarations(stmt, decls);
            }
        }
        ExprKind::If {
            then_branch,
            else_branch,
            ..
        } =>
        {
            collect_declarations(then_branch, decls);
            if let Some(else_expr) = else_branch
            {
                collect_declarations(else_expr, decls);
            }
        }
        ExprKind::Result {
            body,
            else_expr,
            else_binding,
            ..
        } =>
        {
            if let Some(name) = else_binding
            {
                decls.insert(name.clone());
            }
            collect_declarations(body, decls);
            collect_declarations(else_expr, decls);
        }
        ExprKind::While { body, .. } =>
        {
            collect_declarations(body, decls);
        }
        ExprKind::For {
            var,
            iterable,
            body,
            ..
        } =>
        {
            decls.insert(var.clone());
            collect_declarations(iterable, decls);
            collect_declarations(body, decls);
        }
        ExprKind::Loop {
            var, count, body, ..
        } =>
        {
            if let Some(name) = var
            {
                decls.insert(name.clone());
            }
            collect_declarations(count, decls);
            collect_declarations(body, decls);
        }
        ExprKind::Collect {
            var, count, into, body, ..
        } =>
        {
            if let Some(name) = var
            {
                decls.insert(name.clone());
            }
            collect_declarations(count, decls);
            if let Some(into) = into
            {
                collect_declarations(into, decls);
            }
            collect_declarations(body, decls);
        }
        ExprKind::Array(elements) =>
        {
            for e in elements
            {
                collect_declarations(e, decls);
            }
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            collect_declarations(generator, decls);
            collect_declarations(size, decls);
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                collect_declarations(k, decls);
                collect_declarations(v, decls);
            }
        }
        ExprKind::Clone(expr) =>
        {
            collect_declarations(expr, decls);
        }
        ExprKind::ErrorRaise(expr) =>
        {
            collect_declarations(expr, decls);
        }
        ExprKind::EnvFreeze(expr) =>
        {
            collect_declarations(expr, decls);
        }
        ExprKind::FilePublic(expr) =>
        {
            collect_declarations(expr, decls);
        }
        ExprKind::FunctionPublic(expr) =>
        {
            collect_declarations(expr, decls);
        }
        ExprKind::Not(expr) =>
        {
            collect_declarations(expr, decls);
        }
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } =>
        {
            collect_declarations(left, decls);
            collect_declarations(right, decls);
        }
        ExprKind::FormatString(parts) =>
        {
            for part in parts
            {
                if let crate::ast::FormatPart::Expr { expr, .. } = part
                {
                    collect_declarations(expr, decls);
                }
            }
        }
        ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. } =>
        {}
        _ =>
        {}
    }
}

fn build_slot_map(
    params: &[Param],
    locals: HashSet<SymbolId>,
) -> (FxHashMap<SymbolId, usize>, Vec<Rc<String>>)
{
    let mut slot_map = FxHashMap::default();
    let mut slot_names = Vec::new();
    for param in params
    {
        if !slot_map.contains_key(&param.name)
        {
            slot_map.insert(param.name, slot_names.len());
            slot_names.push(symbol_name(param.name));
        }
    }
    for l in locals
    {
        if !slot_map.contains_key(&l)
        {
            slot_map.insert(l.clone(), slot_names.len());
            slot_names.push(symbol_name(l));
        }
    }
    (slot_map, slot_names)
}

pub fn resolve_slots(expr: &mut Expr)
{
    resolve_functions(expr);
}

fn resolve_functions(expr: &mut Expr)
{
    match &mut expr.kind
    {
        ExprKind::FunctionDef {
            params,
            body,
            slots,
            ..
        } =>
        {
            if slots.is_none()
            {
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);
                let (slot_map, slot_names) = build_slot_map(params, locals);
                resolve(body.as_mut(), &slot_map);
                *slots = Some(Rc::new(slot_names));
            }
            resolve_functions(body);
        }
        ExprKind::AnonymousFunction {
            params,
            body,
            slots,
        } =>
        {
            if slots.is_none()
            {
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);
                let (slot_map, slot_names) = build_slot_map(params, locals);
                resolve(body.as_mut(), &slot_map);
                *slots = Some(Rc::new(slot_names));
            }
            resolve_functions(body);
        }
        ExprKind::Assignment { value, .. } => resolve_functions(value),
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            resolve_functions(target);
            resolve_functions(index);
            resolve_functions(value);
        }
        ExprKind::BinaryOp { left, right, .. } =>
        {
            resolve_functions(left);
            resolve_functions(right);
        }
        ExprKind::Not(expr) =>
        {
            resolve_functions(expr);
        }
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } =>
        {
            resolve_functions(left);
            resolve_functions(right);
        }
        ExprKind::Clone(expr) =>
        {
            resolve_functions(expr);
        }
        ExprKind::ErrorRaise(expr) =>
        {
            resolve_functions(expr);
        }
        ExprKind::EnvFreeze(expr) =>
        {
            resolve_functions(expr);
        }
        ExprKind::FilePublic(expr) =>
        {
            resolve_functions(expr.as_mut());
        }
        ExprKind::FunctionPublic(expr) =>
        {
            resolve_functions(expr.as_mut());
        }
        ExprKind::Block(stmts) =>
        {
            for stmt in stmts
            {
                resolve_functions(stmt);
            }
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            resolve_functions(condition);
            resolve_functions(then_branch);
            if let Some(eb) = else_branch
            {
                resolve_functions(eb);
            }
        }
        ExprKind::Result {
            body,
            else_expr,
            ..
        } =>
        {
            resolve_functions(body);
            resolve_functions(else_expr);
        }
        ExprKind::While { condition, body } =>
        {
            resolve_functions(condition);
            resolve_functions(body);
        }
        ExprKind::For { iterable, body, .. } =>
        {
            resolve_functions(iterable);
            resolve_functions(body);
        }
        ExprKind::Loop { count, body, .. } =>
        {
            resolve_functions(count);
            resolve_functions(body);
        }
        ExprKind::Collect { count, into, body, .. } =>
        {
            resolve_functions(count);
            if let Some(into) = into
            {
                resolve_functions(into);
            }
            resolve_functions(body);
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            resolve_functions(function);
            for arg in args
            {
                resolve_functions(arg);
            }
            if let Some(c) = block
            {
                resolve_functions(&mut c.body);
            }
        }
        ExprKind::Array(elements) =>
        {
            for e in elements
            {
                resolve_functions(e);
            }
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            resolve_functions(generator);
            resolve_functions(size);
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                resolve_functions(k);
                resolve_functions(v);
            }
        }
        ExprKind::Index { target, index } =>
        {
            resolve_functions(target);
            resolve_functions(index);
        }
        ExprKind::FormatString(parts) =>
        {
            for part in parts
            {
                if let crate::ast::FormatPart::Expr { expr, .. } = part
                {
                    resolve_functions(expr);
                }
            }
        }
        ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. } =>
        {}
        ExprKind::Yield(args) =>
        {
            for a in args
            {
                resolve_functions(a);
            }
        }
        _ =>
        {}
    }
}

fn uses_environment(expr: &Expr) -> bool
{
    match &expr.kind
    {
        ExprKind::Identifier { slot: None, .. } => true,
        ExprKind::Identifier { .. } => false,
        ExprKind::BinaryOp { left, right, .. } => uses_environment(left) || uses_environment(right),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            uses_environment(condition)
                || uses_environment(then_branch)
                || else_branch.as_ref().map_or(false, |e| uses_environment(e))
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            uses_environment(function)
                || args.iter().any(uses_environment)
                || block.as_ref().map_or(false, |c| uses_environment(&c.body))
        }
        ExprKind::Not(expr) => uses_environment(expr),
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } => uses_environment(left) || uses_environment(right),
        ExprKind::Clone(expr) | ExprKind::EnvFreeze(expr) => uses_environment(expr),
        ExprKind::Use(_) => true,
        ExprKind::Load(_) => true,
        ExprKind::Import { .. } => true,
        ExprKind::Export { .. } => true,
        ExprKind::FilePublic(_) => true,
        ExprKind::FunctionPublic(_) => true,
        ExprKind::FormatString(parts) => parts.iter().any(|part| {
            if let crate::ast::FormatPart::Expr { expr, .. } = part
            {
                uses_environment(expr)
            }
            else
            {
                false
            }
        }),
        // Simple functions (is_simple) only have these constructs roughly.
        // We can be conservative.
        ExprKind::Integer { .. }
        | ExprKind::Unsigned { .. }
        | ExprKind::Float { .. }
        | ExprKind::String(_)
        | ExprKind::Boolean(_)
        | ExprKind::Nil => false,
        _ => true, // Conservative fallback for blocks, loops, etc. if they slipped into is_simple
    }
}

fn builtin_from_symbol(name: SymbolId) -> Option<Builtin>
{
    match symbol_name(name).as_str()
    {
        "puts" => Some(Builtin::Puts),
        "print" => Some(Builtin::Print),
        "eputs" => Some(Builtin::Eputs),
        "eprint" => Some(Builtin::Eprint),
        "log" => Some(Builtin::Log),
        "assert" => Some(Builtin::Assert),
        "assert_eq" => Some(Builtin::AssertEq),
        "len" => Some(Builtin::Len),
        "read_file" => Some(Builtin::ReadFile),
        "write_file" => Some(Builtin::WriteFile),
        "typeof" => Some(Builtin::Typeof),
        "f64" => Some(Builtin::F64),
        "f32" => Some(Builtin::F32),
        "i64" => Some(Builtin::I64),
        "i32" => Some(Builtin::I32),
        "u64" => Some(Builtin::U64),
        "u32" => Some(Builtin::U32),
        _ => None,
    }
}

fn push_const(consts: &mut Vec<Value>, value: Value) -> usize
{
    if let Some(idx) = consts.iter().position(|v| v == &value)
    {
        return idx;
    }
    consts.push(value);
    consts.len() - 1
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum RangeStep
{
    Int(i64),
    Float(f64, FloatKind),
}

fn range_step_from_literal(expr: &Expr) -> Option<RangeStep>
{
    match &expr.kind
    {
        ExprKind::Integer { value, .. } =>
        {
            let step = i64::try_from(*value).ok()?;
            if step > 0
            {
                Some(RangeStep::Int(step))
            }
            else
            {
                None
            }
        }
        ExprKind::Unsigned { value, .. } =>
        {
            let step = i64::try_from(*value).ok()?;
            if step > 0
            {
                Some(RangeStep::Int(step))
            }
            else
            {
                None
            }
        }
        ExprKind::Float { value, kind } =>
        {
            if *value > 0.0
            {
                Some(RangeStep::Float(*value, *kind))
            }
            else
            {
                None
            }
        }
        _ => None,
    }
}

fn match_for_range(
    condition: &Expr,
    body: &Expr,
    consts: &mut Vec<Value>,
) -> Option<(usize, RangeEnd, RangeStep, Expr)>
{
    let (index_slot, end) = match &condition.kind
    {
        ExprKind::BinaryOp {
            left,
            op: Op::LessThan,
            right,
        } =>
        {
            let idx_slot = match &left.kind
            {
                ExprKind::Identifier { slot: Some(s), .. } => *s,
                _ => return None,
            };
            let end = match &right.kind
            {
                ExprKind::Identifier { slot: Some(s), .. } => RangeEnd::Slot(*s),
                ExprKind::Integer { value, kind } =>
                {
                    let idx = push_const(consts, make_signed_int(*value, *kind));
                    RangeEnd::Const(idx)
                }
                ExprKind::Unsigned { value, kind } =>
                {
                    let idx = push_const(consts, make_unsigned_int(*value, *kind));
                    RangeEnd::Const(idx)
                }
                ExprKind::Float { value, kind } =>
                {
                    let idx = push_const(consts, make_float(*value, *kind));
                    RangeEnd::Const(idx)
                }
                _ => return None,
            };
            (idx_slot, end)
        }
        _ => return None,
    };

    let (stmts, line) = match &body.kind
    {
        ExprKind::Block(stmts) => (stmts, body.line),
        _ => return None,
    };
    if stmts.is_empty()
    {
        return None;
    }
    let (body_stmts, increment) = stmts.split_at(stmts.len() - 1);
    let inc_stmt = &increment[0];
    let step = match &inc_stmt.kind
    {
        ExprKind::Assignment {
            slot: Some(s),
            value,
            ..
        } if *s == index_slot => match &value.kind
        {
            ExprKind::BinaryOp {
                left,
                op: Op::Add,
                right,
            } =>
            {
                let left_is_index = matches!(left.kind, ExprKind::Identifier { slot: Some(ls), .. } if ls == index_slot);
                let right_is_index = matches!(right.kind, ExprKind::Identifier { slot: Some(rs), .. } if rs == index_slot);
                if left_is_index
                {
                    range_step_from_literal(right)?
                }
                else if right_is_index
                {
                    range_step_from_literal(left)?
                }
                else
                {
                    return None;
                }
            }
            _ => return None,
        },
        _ => return None,
    };

    let body_expr = Expr {
        kind: ExprKind::Block(body_stmts.to_vec()),
        line,
        column: body.column,
        source: body.source.clone(),
    };
    Some((index_slot, end, step, body_expr))
}

fn match_dot_assign(stmt: &Expr, index_slot: usize) -> Option<(usize, usize, usize)>
{
    let (acc_slot, value) = match &stmt.kind
    {
        ExprKind::Assignment {
            slot: Some(s),
            value,
            ..
        } => (*s, value.as_ref()),
        _ => return None,
    };
    let (add_left, add_right) = match &value.kind
    {
        ExprKind::BinaryOp {
            left,
            op: Op::Add,
            right,
        } => (left.as_ref(), right.as_ref()),
        _ => return None,
    };
    let is_acc = |expr: &Expr| matches!(expr.kind, ExprKind::Identifier { slot: Some(s), .. } if s == acc_slot);
    let mul_expr = if is_acc(add_left)
    {
        add_right
    }
    else if is_acc(add_right)
    {
        add_left
    }
    else
    {
        return None;
    };
    let (a_slot, b_slot) = match &mul_expr.kind
    {
        ExprKind::BinaryOp {
            left,
            op: Op::Multiply,
            right,
        } =>
        {
            let (a_slot, a_idx) = match_f64_index(left)?;
            let (b_slot, b_idx) = match_f64_index(right)?;
            if a_idx != index_slot || b_idx != index_slot
            {
                return None;
            }
            (a_slot, b_slot)
        }
        _ => return None,
    };
    Some((acc_slot, a_slot, b_slot))
}

fn match_dot_range_body(body: &Expr, index_slot: usize) -> Option<(usize, usize, usize)>
{
    let stmt = match &body.kind
    {
        ExprKind::Block(stmts) if stmts.len() == 1 => &stmts[0],
        _ => body,
    };
    match_dot_assign(stmt, index_slot)
}

fn match_dot2_range_body(
    body: &Expr,
    index_slot: usize,
) -> Option<(usize, usize, usize, usize, usize, usize)>
{
    let stmts = match &body.kind
    {
        ExprKind::Block(stmts) if stmts.len() == 2 => stmts,
        _ => return None,
    };
    let (acc1, a1, b1) = match_dot_assign(&stmts[0], index_slot)?;
    let (acc2, a2, b2) = match_dot_assign(&stmts[1], index_slot)?;
    if acc1 == acc2
    {
        return None;
    }
    Some((acc1, a1, b1, acc2, a2, b2))
}

fn match_range_end(expr: &Expr, consts: &mut Vec<Value>) -> Option<RangeEnd>
{
    match &expr.kind
    {
        ExprKind::Identifier { slot: Some(s), .. } => Some(RangeEnd::Slot(*s)),
        ExprKind::Integer { value, kind } =>
        {
            let idx = push_const(consts, make_signed_int(*value, *kind));
            Some(RangeEnd::Const(idx))
        }
        ExprKind::Unsigned { value, kind } =>
        {
            let idx = push_const(consts, make_unsigned_int(*value, *kind));
            Some(RangeEnd::Const(idx))
        }
        ExprKind::Float { value, kind } =>
        {
            let idx = push_const(consts, make_float(*value, *kind));
            Some(RangeEnd::Const(idx))
        }
        _ => None,
    }
}

fn is_pure_expr(expr: &Expr) -> bool
{
    match &expr.kind
    {
        ExprKind::Integer { .. }
        | ExprKind::Unsigned { .. }
        | ExprKind::Float { .. }
        | ExprKind::Boolean(_)
        | ExprKind::Nil => true,
        ExprKind::Identifier { .. } => true,
        ExprKind::Not(expr) => is_pure_expr(expr),
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } => is_pure_expr(left) && is_pure_expr(right),
        ExprKind::BinaryOp { left, right, .. } => is_pure_expr(left) && is_pure_expr(right),
        _ => false,
    }
}

fn match_f64_index(expr: &Expr) -> Option<(usize, usize)>
{
    match &expr.kind
    {
        ExprKind::Index { target, index } =>
        {
            let target_slot = match &target.kind
            {
                ExprKind::Identifier { slot: Some(s), .. } => *s,
                _ => return None,
            };
            let index_slot = match &index.kind
            {
                ExprKind::Identifier { slot: Some(s), .. } => *s,
                _ => return None,
            };
            Some((target_slot, index_slot))
        }
        _ => None,
    }
}

fn match_f64_mul(expr: &Expr) -> Option<(Expr, usize, usize)>
{
    match &expr.kind
    {
        ExprKind::BinaryOp {
            left,
            op: Op::Multiply,
            right,
        } =>
        {
            if let Some((src_slot, src_index_slot)) = match_f64_index(left)
            {
                return Some(((*right.as_ref()).clone(), src_slot, src_index_slot));
            }
            if let Some((src_slot, src_index_slot)) = match_f64_index(right)
            {
                return Some(((*left.as_ref()).clone(), src_slot, src_index_slot));
            }
            None
        }
        _ => None,
    }
}

fn match_f64_axpy(target: &Expr, value: &Expr) -> Option<(usize, usize, usize, usize, Expr)>
{
    let (dst_slot, dst_index_slot) = match_f64_index(target)?;
    let (add_left, add_right) = match &value.kind
    {
        ExprKind::BinaryOp {
            left,
            op: Op::Add,
            right,
        } => (left.as_ref(), right.as_ref()),
        _ => return None,
    };
    if let Some((slot, idx)) = match_f64_index(add_left)
    {
        if slot == dst_slot && idx == dst_index_slot
        {
            if let Some((scalar, src_slot, src_index_slot)) = match_f64_mul(add_right)
            {
                return Some((dst_slot, dst_index_slot, src_slot, src_index_slot, scalar));
            }
        }
    }
    if let Some((slot, idx)) = match_f64_index(add_right)
    {
        if slot == dst_slot && idx == dst_index_slot
        {
            if let Some((scalar, src_slot, src_index_slot)) = match_f64_mul(add_left)
            {
                return Some((dst_slot, dst_index_slot, src_slot, src_index_slot, scalar));
            }
        }
    }
    None
}

fn array_expr_is_f64(expr: &Expr) -> Option<bool>
{
    match &expr.kind
    {
        ExprKind::Array(elements) =>
        {
            if elements.is_empty()
            {
                return Some(false);
            }
            let mut all_numeric = true;
            let mut all_f32 = true;
            let mut all_i32 = true;
            let mut any_float = false;
            let mut any_int = false;
            for e in elements
            {
                match &e.kind
                {
                    ExprKind::Float { kind, .. } =>
                    {
                        any_float = true;
                        if *kind != FloatKind::F32
                        {
                            all_f32 = false;
                        }
                    }
                    ExprKind::Integer { kind, .. } =>
                    {
                        any_int = true;
                        if *kind != IntKind::I32
                        {
                            all_i32 = false;
                        }
                    }
                    ExprKind::Unsigned { kind, .. } =>
                    {
                        any_int = true;
                        if *kind != IntKind::U32
                        {
                            all_i32 = false;
                        }
                    }
                    _ =>
                    {
                        all_numeric = false;
                        break;
                    }
                }
            }
            if !all_numeric
            {
                return None;
            }
            if any_float
            {
                if any_int
                {
                    return Some(true);
                }
                return if all_f32 { None } else { Some(true) };
            }
            if all_i32
            {
                return None;
            }
            None
        }
        ExprKind::ArrayGenerator { generator, .. } => match &generator.kind
        {
            ExprKind::Float {
                kind: FloatKind::F64,
                ..
            } => Some(true),
            ExprKind::Float { .. } => None,
            ExprKind::Integer { .. } | ExprKind::Unsigned { .. } => None,
            _ => None,
        },
        _ => None,
    }
}

thread_local! {
    static COMPILE_USE_CACHES: Cell<bool> = Cell::new(true);
}

fn with_compile_use_caches<F, R>(use_caches: bool, f: F) -> R
where
    F: FnOnce() -> R,
{
    COMPILE_USE_CACHES.with(|flag| {
        let prev = flag.replace(use_caches);
        let result = f();
        flag.set(prev);
        result
    })
}

fn compile_expr(
    expr: &Expr,
    code: &mut Vec<Instruction>,
    consts: &mut Vec<Value>,
    want_value: bool,
) -> bool
{
    let use_caches = COMPILE_USE_CACHES.with(|flag| flag.get());
    match &expr.kind
    {
        ExprKind::Integer { value, kind } =>
        {
            if want_value
            {
                let idx = push_const(consts, make_signed_int(*value, *kind));
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Unsigned { value, kind } =>
        {
            if want_value
            {
                let idx = push_const(consts, make_unsigned_int(*value, *kind));
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Float { value, kind } =>
        {
            if want_value
            {
                let idx = push_const(consts, make_float(*value, *kind));
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Identifier { slot: Some(s), .. } =>
        {
            if want_value
            {
                code.push(Instruction::LoadSlot(*s));
            }
        }
        ExprKind::Identifier { slot: None, name } =>
        {
            if want_value
            {
                if use_caches
                {
                    code.push(Instruction::LoadGlobalCached(
                        *name,
                        Rc::new(RefCell::new(GlobalCache::default())),
                    ));
                }
                else
                {
                    code.push(Instruction::LoadGlobal(*name));
                }
            }
        }
        ExprKind::Boolean(b) =>
        {
            if want_value
            {
                let idx = push_const(consts, Value::Boolean(*b));
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Nil =>
        {
            if want_value
            {
                let idx = push_const(consts, Value::Nil);
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Assignment {
            value,
            slot: Some(s),
            ..
        } =>
        {
            if !compile_expr(value, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::StoreSlot(*s));
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Clone(expr) =>
        {
            if !compile_expr(expr, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::CloneValue);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::EnvFreeze(_) =>
        {
            return false;
        }
        ExprKind::Result { .. } | ExprKind::ErrorRaise(_) =>
        {
            return false;
        }
        ExprKind::Not(expr) =>
        {
            if !compile_expr(expr, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::Not);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::And { left, right } =>
        {
            if !compile_expr(left, code, consts, true)
            {
                return false;
            }
            let jump_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            if !compile_expr(right, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::Not);
            code.push(Instruction::Not);
            let jump_end_idx = code.len();
            code.push(Instruction::Jump(usize::MAX));
            let false_target = code.len();
            let false_idx = push_const(consts, Value::Boolean(false));
            code.push(Instruction::LoadConstIdx(false_idx));
            let end_target = code.len();
            code[jump_false_idx] = Instruction::JumpIfFalse(false_target);
            code[jump_end_idx] = Instruction::Jump(end_target);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::AndBool { left, right } =>
        {
            if !compile_expr(left, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::CheckBool);
            let jump_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            if !compile_expr(right, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::CheckBool);
            let jump_end_idx = code.len();
            code.push(Instruction::Jump(usize::MAX));
            let false_target = code.len();
            let false_idx = push_const(consts, Value::Boolean(false));
            code.push(Instruction::LoadConstIdx(false_idx));
            let end_target = code.len();
            code[jump_false_idx] = Instruction::JumpIfFalse(false_target);
            code[jump_end_idx] = Instruction::Jump(end_target);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Or { left, right } =>
        {
            if !compile_expr(left, code, consts, true)
            {
                return false;
            }
            let jump_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            let true_idx = push_const(consts, Value::Boolean(true));
            code.push(Instruction::LoadConstIdx(true_idx));
            let jump_end_idx = code.len();
            code.push(Instruction::Jump(usize::MAX));
            let false_target = code.len();
            if !compile_expr(right, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::Not);
            code.push(Instruction::Not);
            let end_target = code.len();
            code[jump_false_idx] = Instruction::JumpIfFalse(false_target);
            code[jump_end_idx] = Instruction::Jump(end_target);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::OrBool { left, right } =>
        {
            if !compile_expr(left, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::CheckBool);
            let jump_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            let true_idx = push_const(consts, Value::Boolean(true));
            code.push(Instruction::LoadConstIdx(true_idx));
            let jump_end_idx = code.len();
            code.push(Instruction::Jump(usize::MAX));
            let false_target = code.len();
            if !compile_expr(right, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::CheckBool);
            let end_target = code.len();
            code[jump_false_idx] = Instruction::JumpIfFalse(false_target);
            code[jump_end_idx] = Instruction::Jump(end_target);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Assignment {
            name,
            value,
            slot: None,
        } =>
        {
            if !compile_expr(value, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::StoreGlobal(*name));
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            if block.is_some()
            {
                if let ExprKind::Index { target, index } = &function.kind
                {
                    if let ExprKind::String(name) = &index.kind
                    {
                        if !compile_expr(target, code, consts, true)
                        {
                            return false;
                        }
                        if use_caches
                        {
                            for arg in args
                            {
                                if !compile_expr(arg, code, consts, true)
                                {
                                    return false;
                                }
                            }
                            let map_cache = Rc::new(RefCell::new(MapAccessCache::default()));
                            let call_cache = Rc::new(RefCell::new(CallSiteCache::default()));
                            let block_ref = Rc::new(block.as_ref().unwrap().clone());
                            match args.len()
                            {
                                0 => code.push(Instruction::CallMethodWithBlockCached0(
                                    name.clone(),
                                    map_cache,
                                    call_cache,
                                    block_ref,
                                )),
                                1 => code.push(Instruction::CallMethodWithBlockCached1(
                                    name.clone(),
                                    map_cache,
                                    call_cache,
                                    block_ref,
                                )),
                                _ => code.push(Instruction::CallMethodWithBlockCached(
                                    name.clone(),
                                    map_cache,
                                    call_cache,
                                    block_ref,
                                    args.len(),
                                )),
                            }
                        }
                        else
                        {
                            if !compile_expr(index, code, consts, true)
                            {
                                return false;
                            }
                            code.push(Instruction::Index);
                            for arg in args
                            {
                                if !compile_expr(arg, code, consts, true)
                                {
                                    return false;
                                }
                            }
                            code.push(Instruction::CallValueWithBlock(
                                Rc::new(block.as_ref().unwrap().clone()),
                                args.len(),
                            ));
                        }
                        if !want_value
                        {
                            code.push(Instruction::Pop);
                        }
                        return true;
                    }
                }
                if !compile_expr(function, code, consts, true)
                {
                    return false;
                }
                for arg in args
                {
                    if !compile_expr(arg, code, consts, true)
                    {
                        return false;
                    }
                }
                if use_caches
                {
                    let call_cache = Rc::new(RefCell::new(crate::value::CallSiteCache::default()));
                    let block_ref = Rc::new(block.as_ref().unwrap().clone());
                    match args.len()
                    {
                        0 =>
                        {
                            code.push(Instruction::CallValueWithBlockCached0(call_cache, block_ref))
                        }
                        1 =>
                        {
                            code.push(Instruction::CallValueWithBlockCached1(call_cache, block_ref))
                        }
                        _ => code.push(Instruction::CallValueWithBlockCached(
                            call_cache,
                            block_ref,
                            args.len(),
                        )),
                    }
                }
                else
                {
                    code.push(Instruction::CallValueWithBlock(
                        Rc::new(block.as_ref().unwrap().clone()),
                        args.len(),
                    ));
                }
                if !want_value
                {
                    code.push(Instruction::Pop);
                }
                return true;
            }
            if let ExprKind::Index { target, index } = &function.kind
            {
                if let ExprKind::String(name) = &index.kind
                {
                    if let ExprKind::Identifier {
                        name: target_name, ..
                    } = &target.kind
                    {
                        if symbol_name(*target_name).as_str() == "Bytes"
                        {
                            match name.as_str()
                            {
                                "len" if args.len() == 1 =>
                                {
                                    if !compile_expr(&args[0], code, consts, true)
                                    {
                                        return false;
                                    }
                                    code.push(Instruction::Len);
                                    if !want_value
                                    {
                                        code.push(Instruction::Pop);
                                    }
                                    return true;
                                }
                                "get" if args.len() == 2 =>
                                {
                                    if !compile_expr(&args[0], code, consts, true)
                                    {
                                        return false;
                                    }
                                    if !compile_expr(&args[1], code, consts, true)
                                    {
                                        return false;
                                    }
                                    code.push(Instruction::Index);
                                    if !want_value
                                    {
                                        code.push(Instruction::Pop);
                                    }
                                    return true;
                                }
                                _ =>
                                {}
                            }
                        }
                    }
                }
            }
            if let ExprKind::Identifier { name, .. } = &function.kind
            {
                if let Some(builtin) = builtin_from_symbol(*name)
                {
                    for arg in args
                    {
                        if !compile_expr(arg, code, consts, true)
                        {
                            return false;
                        }
                    }
                    match builtin
                    {
                        Builtin::Len if args.len() == 1 => code.push(Instruction::Len),
                        _ => code.push(Instruction::CallBuiltin(builtin, args.len())),
                    }
                    if !want_value
                    {
                        code.push(Instruction::Pop);
                    }
                    return true;
                }
                if let ExprKind::Identifier { slot: None, name } = &function.kind
                {
                    if use_caches
                    {
                        for arg in args
                        {
                            if !compile_expr(arg, code, consts, true)
                            {
                                return false;
                            }
                        }
                        let global_cache = Rc::new(RefCell::new(GlobalCache::default()));
                        let call_cache = Rc::new(RefCell::new(CallSiteCache::default()));
                        match args.len()
                        {
                            0 => code.push(Instruction::CallGlobalCached0(
                                *name,
                                global_cache,
                                call_cache,
                            )),
                            1 => code.push(Instruction::CallGlobalCached1(
                                *name,
                                global_cache,
                                call_cache,
                            )),
                            _ => code.push(Instruction::CallGlobalCached(
                                *name,
                                global_cache,
                                call_cache,
                                args.len(),
                            )),
                        }
                    }
                    else
                    {
                        code.push(Instruction::LoadGlobal(*name));
                        for arg in args
                        {
                            if !compile_expr(arg, code, consts, true)
                            {
                                return false;
                            }
                        }
                        code.push(Instruction::CallValue(args.len()));
                    }
                    if !want_value
                    {
                        code.push(Instruction::Pop);
                    }
                    return true;
                }
            }
            if let ExprKind::Index { target, index } = &function.kind
            {
                if let ExprKind::String(name) = &index.kind
                {
                    if !compile_expr(target, code, consts, true)
                    {
                        return false;
                    }
                    if use_caches
                    {
                        for arg in args
                        {
                            if !compile_expr(arg, code, consts, true)
                            {
                                return false;
                            }
                        }
                        let map_cache = Rc::new(RefCell::new(MapAccessCache::default()));
                        let call_cache = Rc::new(RefCell::new(CallSiteCache::default()));
                        match args.len()
                        {
                            0 => code.push(Instruction::CallMethodCached0(
                                name.clone(),
                                map_cache,
                                call_cache,
                            )),
                            1 => code.push(Instruction::CallMethodCached1(
                                name.clone(),
                                map_cache,
                                call_cache,
                            )),
                            _ => code.push(Instruction::CallMethodCached(
                                name.clone(),
                                map_cache,
                                call_cache,
                                args.len(),
                            )),
                        }
                    }
                    else
                    {
                        if !compile_expr(index, code, consts, true)
                        {
                            return false;
                        }
                        code.push(Instruction::Index);
                        for arg in args
                        {
                            if !compile_expr(arg, code, consts, true)
                            {
                                return false;
                            }
                        }
                        code.push(Instruction::CallValue(args.len()));
                    }
                    if !want_value
                    {
                        code.push(Instruction::Pop);
                    }
                    return true;
                }
            }
            if !compile_expr(function, code, consts, true)
            {
                return false;
            }
            for arg in args
            {
                if !compile_expr(arg, code, consts, true)
                {
                    return false;
                }
            }
            if let Some(block) = block
            {
                code.push(Instruction::CallValueWithBlock(Rc::new(block.clone()), args.len()));
            }
            else
            {
                if use_caches
                {
                    let call_cache = Rc::new(RefCell::new(crate::value::CallSiteCache::default()));
                    match args.len()
                    {
                        0 => code.push(Instruction::CallValueCached0(call_cache)),
                        1 => code.push(Instruction::CallValueCached1(call_cache)),
                        _ => code.push(Instruction::CallValueCached(call_cache, args.len())),
                    }
                }
                else
                {
                    code.push(Instruction::CallValue(args.len()));
                }
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Index { target, index } =>
        {
            if !compile_expr(target, code, consts, true)
            {
                return false;
            }
            if let ExprKind::String(name) = &index.kind
            {
                match name.as_str()
                {
                    "keys" =>
                    {
                        code.push(Instruction::MapKeys);
                        if !want_value
                        {
                            code.push(Instruction::Pop);
                        }
                        return true;
                    }
                    "values" =>
                    {
                        code.push(Instruction::MapValues);
                        if !want_value
                        {
                            code.push(Instruction::Pop);
                        }
                        return true;
                    }
                    _ =>
                    {}
                }
                if !compile_expr(index, code, consts, true)
                {
                    return false;
                }
                if use_caches
                {
                    code.push(Instruction::MapIndexCached(Rc::new(RefCell::new(
                        MapAccessCache::default(),
                    ))));
                }
                else
                {
                    code.push(Instruction::Index);
                }
                if !want_value
                {
                    code.push(Instruction::Pop);
                }
                return true;
            }
            if !compile_expr(index, code, consts, true)
            {
                return false;
            }
            if use_caches
            {
                let use_f64_cache = match array_expr_is_f64(target)
                {
                    Some(true) | None => true,
                    Some(false) => false,
                };
                if use_f64_cache
                {
                    code.push(Instruction::F64IndexCached(Rc::new(RefCell::new(
                        crate::value::IndexCache::default(),
                    ))));
                }
                else
                {
                    code.push(Instruction::IndexCached(Rc::new(RefCell::new(
                        crate::value::IndexCache::default(),
                    ))));
                }
            }
            else
            {
                code.push(Instruction::Index);
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Slice { target, start, end } =>
        {
            if !compile_expr(target, code, consts, true)
            {
                return false;
            }
            if !compile_expr(start, code, consts, true)
            {
                return false;
            }
            if !compile_expr(end, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::Slice);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            if let Some((dst_slot, dst_index_slot, src_slot, src_index_slot, scalar)) =
                match_f64_axpy(target, value)
            {
                if !compile_expr(&scalar, code, consts, true)
                {
                    return false;
                }
                code.push(Instruction::F64Axpy {
                    dst_slot,
                    dst_index_slot,
                    src_slot,
                    src_index_slot,
                });
            }
            else
            {
                if !compile_expr(target, code, consts, true)
                {
                    return false;
                }
                if !compile_expr(index, code, consts, true)
                {
                    return false;
                }
                if !compile_expr(value, code, consts, true)
                {
                    return false;
                }
                if use_caches
                {
                    code.push(Instruction::F64IndexAssignCached(Rc::new(RefCell::new(
                        crate::value::IndexCache::default(),
                    ))));
                }
                else
                {
                    code.push(Instruction::IndexAssign);
                }
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. }
        | ExprKind::FilePublic(_)
        | ExprKind::FunctionPublic(_) =>
        {
            return false;
        }
        ExprKind::FormatString(_) =>
        {
            return false;
        }
        ExprKind::BinaryOp { left, op, right } =>
        {
            let mut handled = false;
            if *op == Op::Multiply
            {
                let one = |expr: &Expr| {
                    matches!(
                        expr.kind,
                        ExprKind::Integer { value: 1, .. }
                            | ExprKind::Unsigned { value: 1, .. }
                            | ExprKind::Float { value: 1.0, .. }
                    )
                };
                if is_pure_expr(left)
                {
                    match &right.kind
                    {
                        ExprKind::BinaryOp {
                            left: r_left,
                            op: Op::Add,
                            right: r_right,
                        } =>
                        {
                            if (r_left.as_ref() == left.as_ref() && one(r_right))
                                || (r_right.as_ref() == left.as_ref() && one(r_left))
                            {
                                if !compile_expr(left, code, consts, true)
                                {
                                    return false;
                                }
                                code.push(Instruction::Dup);
                                let idx = push_const(consts, default_int(1));
                                code.push(Instruction::LoadConstIdx(idx));
                                if use_caches
                                {
                                    code.push(Instruction::AddCached(Rc::new(RefCell::new(
                                        BinaryOpCache::default(),
                                    ))));
                                    code.push(Instruction::MulCached(Rc::new(RefCell::new(
                                        BinaryOpCache::default(),
                                    ))));
                                }
                                else
                                {
                                    code.push(Instruction::Add);
                                    code.push(Instruction::Mul);
                                }
                                handled = true;
                            }
                        }
                        _ =>
                        {}
                    }
                }
                if !handled && is_pure_expr(right)
                {
                    match &left.kind
                    {
                        ExprKind::BinaryOp {
                            left: l_left,
                            op: Op::Add,
                            right: l_right,
                        } =>
                        {
                            if (l_left.as_ref() == right.as_ref() && one(l_right))
                                || (l_right.as_ref() == right.as_ref() && one(l_left))
                            {
                                if !compile_expr(right, code, consts, true)
                                {
                                    return false;
                                }
                                code.push(Instruction::Dup);
                                let idx = push_const(consts, default_int(1));
                                code.push(Instruction::LoadConstIdx(idx));
                                if use_caches
                                {
                                    code.push(Instruction::AddCached(Rc::new(RefCell::new(
                                        BinaryOpCache::default(),
                                    ))));
                                    code.push(Instruction::MulCached(Rc::new(RefCell::new(
                                        BinaryOpCache::default(),
                                    ))));
                                }
                                else
                                {
                                    code.push(Instruction::Add);
                                    code.push(Instruction::Mul);
                                }
                                handled = true;
                            }
                        }
                        _ =>
                        {}
                    }
                }
            }
            if !handled
            {
                if !compile_expr(left, code, consts, true)
                {
                    return false;
                }
                if !compile_expr(right, code, consts, true)
                {
                    return false;
                }
                match op
                {
                    Op::Add =>
                    {
                        if use_caches
                        {
                            code.push(Instruction::AddCached(Rc::new(RefCell::new(
                                BinaryOpCache::default(),
                            ))));
                        }
                        else
                        {
                            code.push(Instruction::Add);
                        }
                    }
                    Op::Subtract =>
                    {
                        if use_caches
                        {
                            code.push(Instruction::SubCached(Rc::new(RefCell::new(
                                BinaryOpCache::default(),
                            ))));
                        }
                        else
                        {
                            code.push(Instruction::Sub);
                        }
                    }
                    Op::Multiply =>
                    {
                        if use_caches
                        {
                            code.push(Instruction::MulCached(Rc::new(RefCell::new(
                                BinaryOpCache::default(),
                            ))));
                        }
                        else
                        {
                            code.push(Instruction::Mul);
                        }
                    }
                    Op::Divide =>
                    {
                        if use_caches
                        {
                            code.push(Instruction::DivCached(Rc::new(RefCell::new(
                                BinaryOpCache::default(),
                            ))));
                        }
                        else
                        {
                            code.push(Instruction::Div);
                        }
                    }
                    Op::Power =>
                    {
                        if use_caches
                        {
                            code.push(Instruction::PowCached(Rc::new(RefCell::new(
                                BinaryOpCache::default(),
                            ))));
                        }
                        else
                        {
                            code.push(Instruction::Pow);
                        }
                    }
                    Op::Equal => code.push(Instruction::Eq),
                    Op::GreaterThan => code.push(Instruction::Gt),
                    Op::LessThan => code.push(Instruction::Lt),
                    _ => return false,
                }
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Block(stmts) =>
        {
            if stmts.is_empty()
            {
                if want_value
                {
                    let idx = push_const(consts, Value::Nil);
                    code.push(Instruction::LoadConstIdx(idx));
                }
            }
            else
            {
                let last_idx = stmts.len() - 1;
                for (idx, stmt) in stmts.iter().enumerate()
                {
                    let is_last = idx == last_idx;
                    if !compile_expr(stmt, code, consts, is_last && want_value)
                    {
                        return false;
                    }
                }
            }
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            if !compile_expr(condition, code, consts, true)
            {
                return false;
            }
            let jump_if_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            if !compile_expr(then_branch, code, consts, want_value)
            {
                return false;
            }
            let jump_end_idx = code.len();
            code.push(Instruction::Jump(usize::MAX));
            let else_target = code.len();
            if let Some(else_expr) = else_branch
            {
                if !compile_expr(else_expr, code, consts, want_value)
                {
                    return false;
                }
            }
            else
            {
                if want_value
                {
                    let idx = push_const(consts, Value::Nil);
                    code.push(Instruction::LoadConstIdx(idx));
                }
            }
            let end_target = code.len();
            code[jump_if_false_idx] = Instruction::JumpIfFalse(else_target);
            code[jump_end_idx] = Instruction::Jump(end_target);
        }
        ExprKind::While { condition, body } =>
        {
            if !want_value
            {
                if let Some((index_slot, end, step, body_expr)) =
                    match_for_range(condition, body, consts)
                {
                    let is_unit_step = matches!(step, RangeStep::Int(1));
                    if is_unit_step
                    {
                        if let Some((acc1, a1, b1, acc2, a2, b2)) =
                            match_dot2_range_body(&body_expr, index_slot)
                        {
                            code.push(Instruction::F64Dot2Range {
                                acc1_slot: acc1,
                                a1_slot: a1,
                                b1_slot: b1,
                                acc2_slot: acc2,
                                a2_slot: a2,
                                b2_slot: b2,
                                index_slot,
                                end,
                            });
                            code.push(Instruction::Pop);
                            return true;
                        }
                        else if let Some((acc_slot, a_slot, b_slot)) =
                            match_dot_range_body(&body_expr, index_slot)
                        {
                            code.push(Instruction::F64DotRange {
                                acc_slot,
                                a_slot,
                                b_slot,
                                index_slot,
                                end,
                            });
                            code.push(Instruction::Pop);
                            return true;
                        }
                    }
                    let mut body_code = Vec::new();
                    if !compile_expr(&body_expr, &mut body_code, consts, true)
                    {
                        return false;
                    }
                    match step
                    {
                        RangeStep::Int(step) =>
                        {
                            code.push(Instruction::ForRangeInt {
                                index_slot,
                                end,
                                step,
                                body: Rc::new(body_code),
                            });
                        }
                        RangeStep::Float(step, kind) =>
                        {
                            code.push(Instruction::ForRangeFloat {
                                index_slot,
                                end,
                                step,
                                kind,
                                body: Rc::new(body_code),
                            });
                        }
                    }
                    code.push(Instruction::Pop);
                    return true;
                }
            }
            if want_value
            {
                let nil_idx = push_const(consts, Value::Nil);
                code.push(Instruction::LoadConstIdx(nil_idx));
            }
            let loop_start = code.len();
            if !compile_expr(condition, code, consts, true)
            {
                return false;
            }
            let jump_if_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            if want_value
            {
                code.push(Instruction::Pop);
                if !compile_expr(body, code, consts, true)
                {
                    return false;
                }
            }
            else
            {
                if !compile_expr(body, code, consts, false)
                {
                    return false;
                }
            }
            code.push(Instruction::Jump(loop_start));
            let loop_end = code.len();
            code[jump_if_false_idx] = Instruction::JumpIfFalse(loop_end);
        }
        ExprKind::For {
            var_slot: Some(var_slot),
            iterable,
            body,
            ..
        } =>
        {
            if !compile_expr(iterable, code, consts, true)
            {
                return false;
            }
            let mut body_code = Vec::new();
            if !compile_expr(body, &mut body_code, consts, true)
            {
                return false;
            }
            if let Some(is_f64) = array_expr_is_f64(iterable)
            {
                if is_f64
                {
                    code.push(Instruction::ForEachF64Array {
                        var_slot: *var_slot,
                        body: Rc::new(body_code),
                    });
                }
                else
                {
                    code.push(Instruction::ForEachArray {
                        var_slot: *var_slot,
                        body: Rc::new(body_code),
                    });
                }
            }
            else
            {
                code.push(Instruction::ForEach {
                    var_slot: *var_slot,
                    body: Rc::new(body_code),
                });
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Loop {
            count,
            var_slot: Some(var_slot),
            body,
            ..
        } =>
        {
            let end = match_range_end(count, consts);
            if let Some(end) = end
            {
                let zero_idx = push_const(consts, default_int(0));
                code.push(Instruction::LoadConstIdx(zero_idx));
                code.push(Instruction::StoreSlot(*var_slot));
                let mut body_code = Vec::new();
                if !compile_expr(body, &mut body_code, consts, true)
                {
                    return false;
                }
                code.push(Instruction::ForRange {
                    index_slot: *var_slot,
                    end,
                    body: Rc::new(body_code),
                });
                code.push(Instruction::Pop);
                if want_value
                {
                    let idx = push_const(consts, Value::Nil);
                    code.push(Instruction::LoadConstIdx(idx));
                }
            }
            else
            {
                return false;
            }
        }
        ExprKind::Array(elements) =>
        {
            let mut all_f64 = true;
            for e in elements
            {
                match &e.kind
                {
                    ExprKind::Float { value, kind } =>
                    {
                        let idx = push_const(consts, make_float(*value, *kind));
                        code.push(Instruction::LoadConstIdx(idx));
                    }
                    ExprKind::Integer { value, kind } =>
                    {
                        let idx = push_const(consts, make_signed_int(*value, *kind));
                        code.push(Instruction::LoadConstIdx(idx));
                    }
                    ExprKind::Unsigned { value, kind } =>
                    {
                        let idx = push_const(consts, make_unsigned_int(*value, *kind));
                        code.push(Instruction::LoadConstIdx(idx));
                    }
                    _ =>
                    {
                        all_f64 = false;
                        if !compile_expr(e, code, consts, true)
                        {
                            return false;
                        }
                    }
                }
                if all_f64
                {
                    match &e.kind
                    {
                        ExprKind::Float { .. }
                        | ExprKind::Integer { .. }
                        | ExprKind::Unsigned { .. } =>
                        {}
                        _ => all_f64 = false,
                    }
                }
            }
            if all_f64
            {
                code.push(Instruction::F64ArrayGen {
                    count: Some(elements.len()),
                });
            }
            else
            {
                code.push(Instruction::MakeArray(elements.len()));
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::StructDef { .. }
        | ExprKind::StructLiteral { .. }
        | ExprKind::MethodDef { .. } =>
        {
            return false;
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            let mut use_f64_gen = false;
            match &generator.kind
            {
                ExprKind::Float { value, kind } =>
                {
                    let idx = push_const(consts, make_float(*value, *kind));
                    code.push(Instruction::LoadConstIdx(idx));
                    use_f64_gen = true;
                }
                ExprKind::Integer { value, kind } =>
                {
                    let idx = push_const(consts, make_signed_int(*value, *kind));
                    code.push(Instruction::LoadConstIdx(idx));
                    use_f64_gen = true;
                }
                ExprKind::Unsigned { value, kind } =>
                {
                    let idx = push_const(consts, make_unsigned_int(*value, *kind));
                    code.push(Instruction::LoadConstIdx(idx));
                    use_f64_gen = true;
                }
                _ =>
                {
                    if !compile_expr(generator, code, consts, true)
                    {
                        return false;
                    }
                }
            }
            if !compile_expr(size, code, consts, true)
            {
                return false;
            }
            if use_f64_gen
            {
                code.push(Instruction::F64ArrayGen { count: None });
            }
            else
            {
                code.push(Instruction::ArrayGen);
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                if !compile_expr(k, code, consts, true)
                {
                    return false;
                }
                if !compile_expr(v, code, consts, true)
                {
                    return false;
                }
            }
            code.push(Instruction::MakeMap(entries.len()));
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        _ => return false,
    }
    true
}

fn find_compile_failure(expr: &Expr) -> Option<String>
{
    match &expr.kind
    {
        ExprKind::Identifier { slot: None, .. } =>
        {}
        ExprKind::FunctionDef { .. } =>
        {
            return Some("function definition not supported in bytecode dump; functions are dumped separately".to_string());
        }
        ExprKind::AnonymousFunction { .. } =>
        {
            return Some("anonymous function not supported in bytecode".to_string());
        }
        ExprKind::Use(_) =>
        {
            return Some("use not supported in bytecode".to_string());
        }
        ExprKind::Load(_) =>
        {
            return Some("load not supported in bytecode".to_string());
        }
        ExprKind::Import { .. } =>
        {
            return Some("import not supported in bytecode".to_string());
        }
        ExprKind::Export { .. } =>
        {
            return Some("export not supported in bytecode".to_string());
        }
        ExprKind::FormatString(_) =>
        {
            return Some("format string not supported in bytecode".to_string());
        }
        ExprKind::Shell(_) =>
        {
            return Some("shell command not supported in bytecode".to_string());
        }
        ExprKind::Result { .. } =>
        {
            return Some("result not supported in bytecode".to_string());
        }
        ExprKind::ErrorRaise(_) =>
        {
            return Some("error not supported in bytecode".to_string());
        }
        ExprKind::Reference(_) =>
        {
            return Some("reference expression not supported in bytecode".to_string());
        }
        ExprKind::For { var_slot: None, .. } =>
        {
            return Some("for-loop variable not resolved to slot".to_string());
        }
        ExprKind::Loop { var_slot: None, .. } =>
        {
            return Some("loop variable not resolved to slot".to_string());
        }
        ExprKind::Collect { var_slot: None, .. } =>
        {
            return Some("collect variable not resolved to slot".to_string());
        }
        ExprKind::Loop {
            count,
            var_slot: Some(_),
            ..
        } => match &count.kind
        {
            ExprKind::Identifier { slot: Some(_), .. }
            | ExprKind::Integer { .. }
            | ExprKind::Unsigned { .. }
            | ExprKind::Float { .. } =>
            {}
            _ =>
            {
                return Some(
                    "loop count not supported for bytecode (needs literal or slot)".to_string(),
                );
            }
        },
        ExprKind::Collect { .. } =>
        {
            return Some("collect not supported in bytecode".to_string());
        }
        ExprKind::StructDef { .. } =>
        {
            return Some("struct definition not supported in bytecode".to_string());
        }
        ExprKind::StructLiteral { .. } =>
        {
            return Some("struct literal not supported in bytecode".to_string());
        }
        ExprKind::MethodDef { .. } =>
        {
            return Some("method definition not supported in bytecode".to_string());
        }
        _ =>
        {}
    }

    match &expr.kind
    {
        ExprKind::BinaryOp { left, right, .. } =>
        {
            find_compile_failure(left).or_else(|| find_compile_failure(right))
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => find_compile_failure(condition)
            .or_else(|| find_compile_failure(then_branch))
            .or_else(|| else_branch.as_ref().and_then(|e| find_compile_failure(e))),
        ExprKind::Result {
            body,
            else_expr,
            ..
        } => find_compile_failure(body).or_else(|| find_compile_failure(else_expr)),
        ExprKind::While { condition, body } =>
        {
            find_compile_failure(condition).or_else(|| find_compile_failure(body))
        }
        ExprKind::For { iterable, body, .. } =>
        {
            find_compile_failure(iterable).or_else(|| find_compile_failure(body))
        }
        ExprKind::Loop { count, body, .. } =>
        {
            find_compile_failure(count).or_else(|| find_compile_failure(body))
        }
        ExprKind::Collect { count, into, body, .. } =>
        {
            find_compile_failure(count)
                .or_else(|| into.as_ref().and_then(|e| find_compile_failure(e)))
                .or_else(|| find_compile_failure(body))
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } => find_compile_failure(function)
            .or_else(|| args.iter().find_map(find_compile_failure))
            .or_else(|| block.as_ref().and_then(|c| find_compile_failure(&c.body))),
        ExprKind::Array(elements) => elements.iter().find_map(find_compile_failure),
        ExprKind::ArrayGenerator { generator, size } =>
        {
            find_compile_failure(generator).or_else(|| find_compile_failure(size))
        }
        ExprKind::Map(entries) => entries
            .iter()
            .find_map(|(k, v)| find_compile_failure(k).or_else(|| find_compile_failure(v))),
        ExprKind::Index { target, index } =>
        {
            find_compile_failure(target).or_else(|| find_compile_failure(index))
        }
        ExprKind::Slice { target, start, end } => find_compile_failure(target)
            .or_else(|| find_compile_failure(start))
            .or_else(|| find_compile_failure(end)),
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } => find_compile_failure(target)
            .or_else(|| find_compile_failure(index))
            .or_else(|| find_compile_failure(value)),
        ExprKind::Clone(expr)
        | ExprKind::EnvFreeze(expr)
        | ExprKind::ErrorRaise(expr) => find_compile_failure(expr),
        ExprKind::Not(expr) => find_compile_failure(expr),
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } =>
        {
            find_compile_failure(left).or_else(|| find_compile_failure(right))
        }
        ExprKind::Block(stmts) => stmts.iter().find_map(find_compile_failure),
        ExprKind::Assignment { value, .. } => find_compile_failure(value),
        ExprKind::Yield(args) => args.iter().find_map(find_compile_failure),
        ExprKind::Return(expr) => expr.as_ref().and_then(|e| find_compile_failure(e)),
        ExprKind::FormatString(parts) => parts.iter().find_map(|part| {
            if let crate::ast::FormatPart::Expr { expr, .. } = part
            {
                find_compile_failure(expr)
            }
            else
            {
                None
            }
        }),
        ExprKind::FilePublic(expr) => find_compile_failure(expr),
        ExprKind::FunctionPublic(expr) => find_compile_failure(expr),
        _ => None,
    }
}

fn collect_function_exprs(
    expr: &Expr,
    out: &mut Vec<(Option<SymbolId>, Vec<Param>, Expr, Option<Rc<Vec<Rc<String>>>>, usize)>,
)
{
    match &expr.kind
    {
        ExprKind::FunctionDef {
            name,
            params,
            body,
            slots,
        } =>
        {
            out.push((Some(*name), params.clone(), *body.clone(), slots.clone(), expr.line));
            collect_function_exprs(body, out);
        }
        ExprKind::AnonymousFunction {
            params,
            body,
            slots,
        } =>
        {
            out.push((None, params.clone(), *body.clone(), slots.clone(), expr.line));
            collect_function_exprs(body, out);
        }
        ExprKind::MethodDef {
            params,
            body,
            slots,
            ..
        } =>
        {
            out.push((None, params.clone(), *body.clone(), slots.clone(), expr.line));
            collect_function_exprs(body, out);
        }
        ExprKind::Block(stmts) =>
        {
            for stmt in stmts
            {
                collect_function_exprs(stmt, out);
            }
        }
        ExprKind::FilePublic(expr) =>
        {
            collect_function_exprs(expr, out);
        }
        ExprKind::FunctionPublic(expr) =>
        {
            collect_function_exprs(expr, out);
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            collect_function_exprs(condition, out);
            collect_function_exprs(then_branch, out);
            if let Some(else_expr) = else_branch
            {
                collect_function_exprs(else_expr, out);
            }
        }
        ExprKind::Result {
            body,
            else_expr,
            ..
        } =>
        {
            collect_function_exprs(body, out);
            collect_function_exprs(else_expr, out);
        }
        ExprKind::While { condition, body } =>
        {
            collect_function_exprs(condition, out);
            collect_function_exprs(body, out);
        }
        ExprKind::For { iterable, body, .. } =>
        {
            collect_function_exprs(iterable, out);
            collect_function_exprs(body, out);
        }
        ExprKind::Loop { count, body, .. } =>
        {
            collect_function_exprs(count, out);
            collect_function_exprs(body, out);
        }
        ExprKind::Collect { count, into, body, .. } =>
        {
            collect_function_exprs(count, out);
            if let Some(into) = into
            {
                collect_function_exprs(into, out);
            }
            collect_function_exprs(body, out);
        }
        ExprKind::ErrorRaise(expr) =>
        {
            collect_function_exprs(expr, out);
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            collect_function_exprs(function, out);
            for arg in args
            {
                collect_function_exprs(arg, out);
            }
            if let Some(c) = block
            {
                collect_function_exprs(&c.body, out);
            }
        }
        ExprKind::Array(elements) =>
        {
            for e in elements
            {
                collect_function_exprs(e, out);
            }
        }
        ExprKind::StructLiteral { fields, .. } =>
        {
            for (_, expr) in fields
            {
                collect_function_exprs(expr, out);
            }
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            collect_function_exprs(generator, out);
            collect_function_exprs(size, out);
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                collect_function_exprs(k, out);
                collect_function_exprs(v, out);
            }
        }
        ExprKind::Index { target, index } =>
        {
            collect_function_exprs(target, out);
            collect_function_exprs(index, out);
        }
        ExprKind::Slice { target, start, end } =>
        {
            collect_function_exprs(target, out);
            collect_function_exprs(start, out);
            collect_function_exprs(end, out);
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            collect_function_exprs(target, out);
            collect_function_exprs(index, out);
            collect_function_exprs(value, out);
        }
        ExprKind::Clone(expr) =>
        {
            collect_function_exprs(expr, out);
        }
        ExprKind::EnvFreeze(expr) =>
        {
            collect_function_exprs(expr, out);
        }
        ExprKind::Not(expr) =>
        {
            collect_function_exprs(expr, out);
        }
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } =>
        {
            collect_function_exprs(left, out);
            collect_function_exprs(right, out);
        }
        ExprKind::FormatString(parts) =>
        {
            for part in parts
            {
                if let crate::ast::FormatPart::Expr { expr, .. } = part
                {
                    collect_function_exprs(expr, out);
                }
            }
        }
        ExprKind::Yield(args) =>
        {
            for arg in args
            {
                collect_function_exprs(arg, out);
            }
        }
        ExprKind::Return(expr) =>
        {
            if let Some(e) = expr
            {
                collect_function_exprs(e, out);
            }
        }
        ExprKind::Assignment { value, .. } =>
        {
            collect_function_exprs(value, out);
        }
        ExprKind::BinaryOp { left, right, .. } =>
        {
            collect_function_exprs(left, out);
            collect_function_exprs(right, out);
        }
        ExprKind::Reference(_)
        | ExprKind::Identifier { .. }
        | ExprKind::Integer { .. }
        | ExprKind::Unsigned { .. }
        | ExprKind::Float { .. }
        | ExprKind::String(_)
        | ExprKind::Boolean(_)
        | ExprKind::Nil
        | ExprKind::Shell(_)
        | ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. }
        | ExprKind::StructDef { .. } =>
        {}
    }
}

fn collect_cache_metrics(code: &[Instruction])
-> (u64, u64, u64, u64, u64, u64, u64, u64, u64, u64)
{
    let mut bin_hits = 0u64;
    let mut bin_misses = 0u64;
    let mut idx_hits = 0u64;
    let mut idx_misses = 0u64;
    let mut map_hits = 0u64;
    let mut map_misses = 0u64;
    let mut call_hits = 0u64;
    let mut call_misses = 0u64;
    let mut global_hits = 0u64;
    let mut global_misses = 0u64;

    for inst in code
    {
        match inst
        {
            Instruction::AddCached(cache)
            | Instruction::SubCached(cache)
            | Instruction::MulCached(cache)
            | Instruction::DivCached(cache)
            | Instruction::PowCached(cache) =>
            {
                let cache = cache.borrow();
                bin_hits += cache.hits;
                bin_misses += cache.misses;
            }
            Instruction::IndexCached(cache)
            | Instruction::F64IndexCached(cache)
            | Instruction::F64IndexAssignCached(cache) =>
            {
                let cache = cache.borrow();
                idx_hits += cache.hits;
                idx_misses += cache.misses;
            }
            Instruction::MapIndexCached(cache) =>
            {
                let cache = cache.borrow();
                map_hits += cache.hits;
                map_misses += cache.misses;
            }
            Instruction::CallValueCached(cache, _) =>
            {
                let cache = cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
            }
            Instruction::CallValueCached0(cache) | Instruction::CallValueCached1(cache) =>
            {
                let cache = cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
            }
            Instruction::CallValueWithBlockCached(cache, _, _) =>
            {
                let cache = cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
            }
            Instruction::CallValueWithBlockCached0(cache, _)
            | Instruction::CallValueWithBlockCached1(cache, _) =>
            {
                let cache = cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
            }
            Instruction::CallGlobalCached(_, global_cache, call_cache, _) =>
            {
                let cache = call_cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
                let cache = global_cache.borrow();
                global_hits += cache.hits;
                global_misses += cache.misses;
            }
            Instruction::CallMethodCached(_, map_cache, call_cache, _) =>
            {
                let cache = call_cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
                let cache = map_cache.borrow();
                map_hits += cache.hits;
                map_misses += cache.misses;
            }
            Instruction::CallMethodCached0(_, map_cache, call_cache)
            | Instruction::CallMethodCached1(_, map_cache, call_cache) =>
            {
                let cache = call_cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
                let cache = map_cache.borrow();
                map_hits += cache.hits;
                map_misses += cache.misses;
            }
            Instruction::CallMethodWithBlockCached(_, map_cache, call_cache, _, _) =>
            {
                let cache = call_cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
                let cache = map_cache.borrow();
                map_hits += cache.hits;
                map_misses += cache.misses;
            }
            Instruction::CallMethodWithBlockCached0(_, map_cache, call_cache, _)
            | Instruction::CallMethodWithBlockCached1(_, map_cache, call_cache, _) =>
            {
                let cache = call_cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
                let cache = map_cache.borrow();
                map_hits += cache.hits;
                map_misses += cache.misses;
            }
            Instruction::LoadGlobalCached(_, cache) =>
            {
                let cache = cache.borrow();
                global_hits += cache.hits;
                global_misses += cache.misses;
            }
            Instruction::CallGlobalCached0(_, global_cache, call_cache)
            | Instruction::CallGlobalCached1(_, global_cache, call_cache) =>
            {
                let cache = call_cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
                let cache = global_cache.borrow();
                global_hits += cache.hits;
                global_misses += cache.misses;
            }
            _ =>
            {}
        }
    }

    (
        bin_hits,
        bin_misses,
        idx_hits,
        idx_misses,
        map_hits,
        map_misses,
        call_hits,
        call_misses,
        global_hits,
        global_misses,
    )
}

fn format_bytecode_instruction(inst: &Instruction) -> String
{
    match inst
    {
        Instruction::CallValueWithBlock(_, argc) =>
        {
            format!("CallValueWithBlock(<block>, {argc})")
        }
        Instruction::CallValueWithBlockCached(_, _, argc) =>
        {
            format!("CallValueWithBlockCached(<cache>, <block>, {argc})")
        }
        Instruction::CallValueWithBlockCached0(_, _) =>
        {
            "CallValueWithBlockCached0(<cache>, <block>)".to_string()
        }
        Instruction::CallValueWithBlockCached1(_, _) =>
        {
            "CallValueWithBlockCached1(<cache>, <block>)".to_string()
        }
        Instruction::CallValueCached0(_) => "CallValueCached0(<cache>)".to_string(),
        Instruction::CallValueCached1(_) => "CallValueCached1(<cache>)".to_string(),
        Instruction::CallMethodWithBlockCached(name, _, _, _, argc) =>
        {
            format!(
                "CallMethodWithBlockCached({:?}, <map_cache>, <call_cache>, <block>, {argc})",
                name
            )
        }
        Instruction::CallMethodWithBlockCached0(name, _, _, _) =>
        {
            format!("CallMethodWithBlockCached0({:?}, <map_cache>, <call_cache>, <block>)", name)
        }
        Instruction::CallMethodWithBlockCached1(name, _, _, _) =>
        {
            format!("CallMethodWithBlockCached1({:?}, <map_cache>, <call_cache>, <block>)", name)
        }
        Instruction::CallMethodCached0(name, _, _) =>
        {
            format!("CallMethodCached0({:?}, <map_cache>, <call_cache>)", name)
        }
        Instruction::CallMethodCached1(name, _, _) =>
        {
            format!("CallMethodCached1({:?}, <map_cache>, <call_cache>)", name)
        }
        Instruction::CallGlobalCached0(name, _, _) =>
        {
            format!("CallGlobalCached0({:?}, <global_cache>, <call_cache>)", name)
        }
        Instruction::CallGlobalCached1(name, _, _) =>
        {
            format!("CallGlobalCached1({:?}, <global_cache>, <call_cache>)", name)
        }
        Instruction::CheckBool => "CheckBool".to_string(),
        _ => format!("{:?}", inst),
    }
}

pub fn dump_bytecode(ast: &Expr, mode: BytecodeMode) -> String
{
    let mut out = String::new();

    out.push_str(&format!("Bytecode mode: {:?}\n", mode));
    out.push_str("Top-level bytecode:\n");
    {
        let simple = is_simple(ast);
        let uses_env = uses_environment(ast);
        if mode == BytecodeMode::Off
        {
            out.push_str("  <bytecode disabled>\n");
        }
        else if !should_compile(simple, uses_env, mode)
        {
            if !simple
            {
                out.push_str("  <not compiled: not simple>\n");
            }
            else if uses_env
            {
                out.push_str("  <not compiled: uses_env>\n");
            }
            else
            {
                out.push_str("  <not compiled>\n");
            }
        }
        else
        {
            let mut code = Vec::new();
            let mut consts = Vec::new();
            let use_caches = mode == BytecodeMode::Advanced;
            if with_compile_use_caches(use_caches, || {
                compile_expr(ast, &mut code, &mut consts, true)
            })
            {
                out.push_str("  Constants:\n");
                for (idx, value) in consts.iter().enumerate()
                {
                    out.push_str(&format!("  [{idx}] {}\n", value.inspect()));
                }
                out.push_str("  Bytecode:\n");
                for (idx, inst) in code.iter().enumerate()
                {
                    out.push_str(&format!("  {idx:04} {}\n", format_bytecode_instruction(inst)));
                }
                let (
                    bin_hits,
                    bin_misses,
                    idx_hits,
                    idx_misses,
                    map_hits,
                    map_misses,
                    call_hits,
                    call_misses,
                    global_hits,
                    global_misses,
                ) = collect_cache_metrics(&code);
                out.push_str(&format!(
                "  CacheMetrics bin(hits={}, misses={}) index(hits={}, misses={}) map(hits={}, misses={}) call(hits={}, misses={}) global(hits={}, misses={})\n",
                bin_hits,
                bin_misses,
                idx_hits,
                idx_misses,
                map_hits,
                map_misses,
                call_hits,
                call_misses,
                global_hits,
                global_misses
            ));
            }
            else if let Some(reason) = find_compile_failure(ast)
            {
                out.push_str(&format!("  <compile failed: {reason}>\n"));
            }
            else
            {
                out.push_str("  <compile failed: unknown reason>\n");
            }
        }
    }

    let mut functions = Vec::new();
    collect_function_exprs(ast, &mut functions);
    if !functions.is_empty()
    {
        out.push_str("Functions:\n");
    }

    for (name, params, body, slots, line) in functions
    {
        let label = match name
        {
            Some(sym) => format!("{}", symbol_name(sym).as_str()),
            None => format!("<anon@line {}>", line),
        };
        out.push_str(&format!("- {label} (line {line})\n"));

        let (resolved_body, _slot_names) = if let Some(slot_names) = slots
        {
            (body, slot_names)
        }
        else
        {
            let mut locals = HashSet::new();
            collect_declarations(&body, &mut locals);
            let (slot_map, slot_names) = build_slot_map(&params, locals);
            let mut resolved = body;
            resolve(&mut resolved, &slot_map);
            (resolved, Rc::new(slot_names))
        };

        let simple = is_simple(&resolved_body);
        let uses_env = uses_environment(&resolved_body);
        out.push_str(&format!("  simple: {}, uses_env: {}\n", simple, uses_env));

        if mode == BytecodeMode::Off
        {
            out.push_str("  <bytecode disabled>\n");
        }
        else if !should_compile(simple, uses_env, mode)
        {
            if !simple
            {
                out.push_str("  <not compiled: not simple>\n");
            }
            else if uses_env
            {
                out.push_str("  <not compiled: uses_env>\n");
            }
            else
            {
                out.push_str("  <not compiled>\n");
            }
        }
        else
        {
            let mut code = Vec::new();
            let mut consts = Vec::new();
            let use_caches = mode == BytecodeMode::Advanced;
            if with_compile_use_caches(use_caches, || {
                compile_expr(&resolved_body, &mut code, &mut consts, true)
            })
            {
                out.push_str("  Constants:\n");
                for (idx, value) in consts.iter().enumerate()
                {
                    out.push_str(&format!("  [{idx}] {}\n", value.inspect()));
                }
                out.push_str("  Bytecode:\n");
                for (idx, inst) in code.iter().enumerate()
                {
                    out.push_str(&format!("  {idx:04} {}\n", format_bytecode_instruction(inst)));
                }
                let (
                    bin_hits,
                    bin_misses,
                    idx_hits,
                    idx_misses,
                    map_hits,
                    map_misses,
                    call_hits,
                    call_misses,
                    global_hits,
                    global_misses,
                ) = collect_cache_metrics(&code);
                out.push_str(&format!(
                    "  CacheMetrics bin(hits={}, misses={}) index(hits={}, misses={}) map(hits={}, misses={}) call(hits={}, misses={}) global(hits={}, misses={})\n",
                    bin_hits,
                    bin_misses,
                    idx_hits,
                    idx_misses,
                    map_hits,
                    map_misses,
                    call_hits,
                    call_misses,
                    global_hits,
                    global_misses
                ));
            }
            else if let Some(reason) = find_compile_failure(&resolved_body)
            {
                out.push_str(&format!("  <compile failed: {reason}>\n"));
            }
            else
            {
                out.push_str("  <compile failed: unknown reason>\n");
            }
        }
    }

    out
}

fn substitute(expr: &Expr, args: &[Expr]) -> Expr
{
    match &expr.kind
    {
        ExprKind::Identifier { slot: Some(s), .. } =>
        {
            if *s < args.len()
            {
                args[*s].clone()
            }
            else
            {
                expr.clone()
            }
        }
        ExprKind::Assignment { name, value, slot } => Expr {
            kind: ExprKind::Assignment {
                name: name.clone(),
                value: Box::new(substitute(value, args)),
                slot: *slot,
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } => Expr {
            kind: ExprKind::IndexAssignment {
                target: Box::new(substitute(target, args)),
                index: Box::new(substitute(index, args)),
                value: Box::new(substitute(value, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::BinaryOp { left, op, right } => Expr {
            kind: ExprKind::BinaryOp {
                left: Box::new(substitute(left, args)),
                op: op.clone(),
                right: Box::new(substitute(right, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Not(expr) => Expr {
            kind: ExprKind::Not(Box::new(substitute(expr, args))),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::And { left, right } => Expr {
            kind: ExprKind::And {
                left: Box::new(substitute(left, args)),
                right: Box::new(substitute(right, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::AndBool { left, right } => Expr {
            kind: ExprKind::AndBool {
                left: Box::new(substitute(left, args)),
                right: Box::new(substitute(right, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Or { left, right } => Expr {
            kind: ExprKind::Or {
                left: Box::new(substitute(left, args)),
                right: Box::new(substitute(right, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::OrBool { left, right } => Expr {
            kind: ExprKind::OrBool {
                left: Box::new(substitute(left, args)),
                right: Box::new(substitute(right, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Clone(expr) => Expr {
            kind: ExprKind::Clone(Box::new(substitute(expr, args))),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::EnvFreeze(expr) => Expr {
            kind: ExprKind::EnvFreeze(Box::new(substitute(expr, args))),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => Expr {
            kind: ExprKind::If {
                condition: Box::new(substitute(condition, args)),
                then_branch: Box::new(substitute(then_branch, args)),
                else_branch: else_branch.as_ref().map(|e| Box::new(substitute(e, args))),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::While { condition, body } => Expr {
            kind: ExprKind::While {
                condition: Box::new(substitute(condition, args)),
                body: Box::new(substitute(body, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::For {
            var,
            var_slot,
            iterable,
            body,
        } => Expr {
            kind: ExprKind::For {
                var: var.clone(),
                var_slot: *var_slot,
                iterable: Box::new(substitute(iterable, args)),
                body: Box::new(substitute(body, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Loop {
            count,
            var,
            var_slot,
            body,
        } => Expr {
            kind: ExprKind::Loop {
                count: Box::new(substitute(count, args)),
                var: var.clone(),
                var_slot: *var_slot,
                body: Box::new(substitute(body, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Collect {
            count,
            into,
            var,
            var_slot,
            body,
        } => Expr {
            kind: ExprKind::Collect {
                count: Box::new(substitute(count, args)),
                into: into.as_ref().map(|expr| Box::new(substitute(expr, args))),
                var: var.clone(),
                var_slot: *var_slot,
                body: Box::new(substitute(body, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Call {
            function,
            args: call_args,
            block,
            inlined_body,
        } => Expr {
            kind: ExprKind::Call {
                function: Box::new(substitute(function, args)),
                args: call_args.iter().map(|a| substitute(a, args)).collect(),
                block: block.clone(), // Blocks shouldn't be here in is_simple, but safe to clone
                inlined_body: inlined_body.clone(),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Array(elements) => Expr {
            kind: ExprKind::Array(elements.iter().map(|e| substitute(e, args)).collect()),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::ArrayGenerator { generator, size } => Expr {
            kind: ExprKind::ArrayGenerator {
                generator: Box::new(substitute(generator, args)),
                size: Box::new(substitute(size, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Map(entries) => Expr {
            kind: ExprKind::Map(
                entries
                    .iter()
                    .map(|(k, v)| (substitute(k, args), substitute(v, args)))
                    .collect(),
            ),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::StructLiteral { name, fields } => Expr {
            kind: ExprKind::StructLiteral {
                name: *name,
                fields: fields
                    .iter()
                    .map(|(field, value)| (*field, substitute(value, args)))
                    .collect(),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Index { target, index } => Expr {
            kind: ExprKind::Index {
                target: Box::new(substitute(target, args)),
                index: Box::new(substitute(index, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Slice { target, start, end } => Expr {
            kind: ExprKind::Slice {
                target: Box::new(substitute(target, args)),
                start: Box::new(substitute(start, args)),
                end: Box::new(substitute(end, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Yield(args_exprs) => Expr {
            kind: ExprKind::Yield(args_exprs.iter().map(|a| substitute(a, args)).collect()),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Block(stmts) => Expr {
            kind: ExprKind::Block(stmts.iter().map(|s| substitute(s, args)).collect()),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::FilePublic(expr) => Expr {
            kind: ExprKind::FilePublic(Box::new(substitute(expr, args))),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::FunctionPublic(expr) => Expr {
            kind: ExprKind::FunctionPublic(Box::new(substitute(expr, args))),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::FormatString(parts) => Expr {
            kind: ExprKind::FormatString(
                parts
                    .iter()
                    .map(|part| match part
                    {
                        crate::ast::FormatPart::Literal(s) =>
                        {
                            crate::ast::FormatPart::Literal(s.clone())
                        }
                        crate::ast::FormatPart::Expr { expr, spec } =>
                        {
                            crate::ast::FormatPart::Expr {
                                expr: Box::new(substitute(expr, args)),
                                spec: spec.clone(),
                            }
                        }
                    })
                    .collect(),
            ),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::StructDef { .. } | ExprKind::MethodDef { .. } => expr.clone(),
        // Literals
        _ => expr.clone(),
    }
}

fn expr_size(expr: &Expr) -> usize
{
    match &expr.kind
    {
        ExprKind::BinaryOp { left, right, .. } => 1 + expr_size(left) + expr_size(right),
        ExprKind::Not(expr)
        | ExprKind::Clone(expr)
        | ExprKind::EnvFreeze(expr)
        | ExprKind::ErrorRaise(expr) =>
        {
            1 + expr_size(expr)
        }
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } => 1 + expr_size(left) + expr_size(right),
        ExprKind::Assignment { value, .. } => 1 + expr_size(value),
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } => 1 + expr_size(target) + expr_size(index) + expr_size(value),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            1 + expr_size(condition)
                + expr_size(then_branch)
                + else_branch.as_ref().map_or(0, |e| expr_size(e))
        }
        ExprKind::Result {
            body,
            else_expr,
            ..
        } => 1 + expr_size(body) + expr_size(else_expr),
        ExprKind::While { condition, body } => 1 + expr_size(condition) + expr_size(body),
        ExprKind::For { iterable, body, .. } => 1 + expr_size(iterable) + expr_size(body),
        ExprKind::Loop { count, body, .. } => 1 + expr_size(count) + expr_size(body),
        ExprKind::Collect { count, into, body, .. } =>
        {
            let into_size = into.as_ref().map_or(0, |expr| expr_size(expr));
            1 + expr_size(count) + into_size + expr_size(body)
        }
        ExprKind::Call { function, args, .. } =>
        {
            1 + expr_size(function) + args.iter().map(expr_size).sum::<usize>()
        }
        ExprKind::Array(elements) => 1 + elements.iter().map(expr_size).sum::<usize>(),
        ExprKind::StructLiteral { fields, .. } =>
        {
            1 + fields.iter().map(|(_, v)| expr_size(v)).sum::<usize>()
        }
        ExprKind::ArrayGenerator { generator, size } => 1 + expr_size(generator) + expr_size(size),
        ExprKind::Map(entries) =>
        {
            1 + entries
                .iter()
                .map(|(k, v)| expr_size(k) + expr_size(v))
                .sum::<usize>()
        }
        ExprKind::Index { target, index } => 1 + expr_size(target) + expr_size(index),
        ExprKind::Slice { target, start, end } =>
        {
            1 + expr_size(target) + expr_size(start) + expr_size(end)
        }
        ExprKind::Yield(args) => 1 + args.iter().map(expr_size).sum::<usize>(),
        ExprKind::Block(stmts) => 1 + stmts.iter().map(expr_size).sum::<usize>(),
        ExprKind::FilePublic(expr) => 1 + expr_size(expr),
        ExprKind::FunctionPublic(expr) => 1 + expr_size(expr),
        ExprKind::FormatString(parts) =>
        {
            1 + parts
                .iter()
                .map(|part| {
                    if let crate::ast::FormatPart::Expr { expr, .. } = part
                    {
                        expr_size(expr)
                    }
                    else
                    {
                        1
                    }
                })
                .sum::<usize>()
        }
        _ => 1,
    }
}

fn is_reg_simple(expr: &Expr) -> bool
{
    match &expr.kind
    {
        ExprKind::Yield(_)
        | ExprKind::FunctionDef { .. }
        | ExprKind::MethodDef { .. }
        | ExprKind::AnonymousFunction { .. }
        | ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. }
        | ExprKind::FilePublic(_)
        | ExprKind::FunctionPublic(_)
        | ExprKind::StructDef { .. } => false,
        ExprKind::If { .. }
        | ExprKind::While { .. }
        | ExprKind::For { .. }
        | ExprKind::Loop { .. }
        | ExprKind::Collect { .. }
        | ExprKind::Result { .. } => false,
        ExprKind::Array(_)
        | ExprKind::ArrayGenerator { .. }
        | ExprKind::Map(_)
        | ExprKind::StructLiteral { .. }
        | ExprKind::FormatString(_)
        | ExprKind::EnvFreeze(_)
        | ExprKind::ErrorRaise(_)
        | ExprKind::Not(_)
        | ExprKind::And { .. }
        | ExprKind::AndBool { .. }
        | ExprKind::Or { .. }
        | ExprKind::OrBool { .. } => false,
        ExprKind::Block(stmts) => stmts.iter().all(is_reg_simple),
        ExprKind::BinaryOp { left, right, .. } => is_reg_simple(left) && is_reg_simple(right),
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            if block.is_some()
            {
                return false;
            }
            is_reg_simple(function) && args.iter().all(is_reg_simple)
        }
        ExprKind::Index { target, index } => is_reg_simple(target) && is_reg_simple(index),
        ExprKind::Slice { target, start, end } =>
        {
            is_reg_simple(target) && is_reg_simple(start) && is_reg_simple(end)
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } => is_reg_simple(target) && is_reg_simple(index) && is_reg_simple(value),
        ExprKind::Assignment { value, .. } => is_reg_simple(value),
        ExprKind::Clone(expr) => is_reg_simple(expr),
        _ => true,
    }
}

struct RegAllocator
{
    next_reg: usize,
}

impl RegAllocator
{
    fn new() -> Self
    {
        Self { next_reg: 0 }
    }

    fn alloc(&mut self) -> usize
    {
        let reg = self.next_reg;
        self.next_reg += 1;
        reg
    }
}

fn compile_reg_expr(
    expr: &Expr,
    code: &mut Vec<RegInstruction>,
    consts: &mut Vec<Value>,
    alloc: &mut RegAllocator,
) -> Option<usize>
{
    match &expr.kind
    {
        ExprKind::Integer { value, kind } =>
        {
            let dst = alloc.alloc();
            let idx = push_const(consts, make_signed_int(*value, *kind));
            code.push(RegInstruction::LoadConst { dst, idx });
            Some(dst)
        }
        ExprKind::Unsigned { value, kind } =>
        {
            let dst = alloc.alloc();
            let idx = push_const(consts, make_unsigned_int(*value, *kind));
            code.push(RegInstruction::LoadConst { dst, idx });
            Some(dst)
        }
        ExprKind::Float { value, kind } =>
        {
            let dst = alloc.alloc();
            let idx = push_const(consts, make_float(*value, *kind));
            code.push(RegInstruction::LoadConst { dst, idx });
            Some(dst)
        }
        ExprKind::Boolean(b) =>
        {
            let dst = alloc.alloc();
            let idx = push_const(consts, Value::Boolean(*b));
            code.push(RegInstruction::LoadConst { dst, idx });
            Some(dst)
        }
        ExprKind::Nil =>
        {
            let dst = alloc.alloc();
            let idx = push_const(consts, Value::Nil);
            code.push(RegInstruction::LoadConst { dst, idx });
            Some(dst)
        }
        ExprKind::String(s) =>
        {
            let dst = alloc.alloc();
            let idx = push_const(consts, Value::String(s.clone()));
            code.push(RegInstruction::LoadConst { dst, idx });
            Some(dst)
        }
        ExprKind::Identifier { slot: Some(s), .. } =>
        {
            let dst = alloc.alloc();
            code.push(RegInstruction::LoadSlot { dst, slot: *s });
            Some(dst)
        }
        ExprKind::Identifier { slot: None, .. } => None,
        ExprKind::Assignment {
            slot: Some(s),
            value,
            ..
        } =>
        {
            let src = compile_reg_expr(value, code, consts, alloc)?;
            code.push(RegInstruction::StoreSlot { slot: *s, src });
            Some(src)
        }
        ExprKind::Assignment { slot: None, .. } => None,
        ExprKind::Clone(expr) =>
        {
            let src = compile_reg_expr(expr, code, consts, alloc)?;
            let dst = alloc.alloc();
            code.push(RegInstruction::CloneValue { dst, src });
            Some(dst)
        }
        ExprKind::EnvFreeze(_) => None,
        ExprKind::Not(_) => None,
        ExprKind::And { .. }
        | ExprKind::AndBool { .. }
        | ExprKind::Or { .. }
        | ExprKind::OrBool { .. } => None,
        ExprKind::BinaryOp { left, op, right } =>
        {
            let left = compile_reg_expr(left, code, consts, alloc)?;
            let right = compile_reg_expr(right, code, consts, alloc)?;
            let op = reg_binop_from_op(op)?;
            let dst = alloc.alloc();
            code.push(RegInstruction::BinOpCached {
                dst,
                op,
                left,
                right,
                cache: Rc::new(RefCell::new(BinaryOpCache::default())),
            });
            Some(dst)
        }
        ExprKind::Index { target, index } =>
        {
            let target = compile_reg_expr(target, code, consts, alloc)?;
            if let ExprKind::String(name) = &index.kind
            {
                match name.as_str()
                {
                    "keys" =>
                    {
                        let dst = alloc.alloc();
                        code.push(RegInstruction::MapKeys { dst, src: target });
                        return Some(dst);
                    }
                    "values" =>
                    {
                        let dst = alloc.alloc();
                        code.push(RegInstruction::MapValues { dst, src: target });
                        return Some(dst);
                    }
                    _ =>
                    {}
                }
                let index = compile_reg_expr(index, code, consts, alloc)?;
                let dst = alloc.alloc();
                code.push(RegInstruction::MapIndexCached {
                    dst,
                    target,
                    index,
                    cache: Rc::new(RefCell::new(MapAccessCache::default())),
                });
                return Some(dst);
            }
            let index = compile_reg_expr(index, code, consts, alloc)?;
            let dst = alloc.alloc();
            code.push(RegInstruction::F64IndexCached {
                dst,
                target,
                index,
                cache: Rc::new(RefCell::new(IndexCache::default())),
            });
            Some(dst)
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            let target = compile_reg_expr(target, code, consts, alloc)?;
            let index = compile_reg_expr(index, code, consts, alloc)?;
            let value = compile_reg_expr(value, code, consts, alloc)?;
            let dst = alloc.alloc();
            code.push(RegInstruction::F64IndexAssignCached {
                dst,
                target,
                index,
                value,
                cache: Rc::new(RefCell::new(IndexCache::default())),
            });
            Some(dst)
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            if block.is_some()
            {
                return None;
            }
            if let ExprKind::Index { target, index } = &function.kind
            {
                if let ExprKind::String(name) = &index.kind
                {
                    if let ExprKind::Identifier {
                        name: target_name, ..
                    } = &target.kind
                    {
                        if symbol_name(*target_name).as_str() == "Bytes"
                        {
                            match name.as_str()
                            {
                                "len" if args.len() == 1 =>
                                {
                                    let src = compile_reg_expr(&args[0], code, consts, alloc)?;
                                    let dst = alloc.alloc();
                                    code.push(RegInstruction::Len { dst, src });
                                    return Some(dst);
                                }
                                "get" if args.len() == 2 =>
                                {
                                    let target_reg =
                                        compile_reg_expr(&args[0], code, consts, alloc)?;
                                    let index_reg =
                                        compile_reg_expr(&args[1], code, consts, alloc)?;
                                    let dst = alloc.alloc();
                                    code.push(RegInstruction::F64IndexCached {
                                        dst,
                                        target: target_reg,
                                        index: index_reg,
                                        cache: Rc::new(RefCell::new(IndexCache::default())),
                                    });
                                    return Some(dst);
                                }
                                _ =>
                                {}
                            }
                        }
                    }
                }
            }
            if let ExprKind::Identifier { name, .. } = &function.kind
            {
                if let Some(builtin) = builtin_from_symbol(*name)
                {
                    if matches!(builtin, Builtin::Len) && args.len() == 1
                    {
                        let src = compile_reg_expr(&args[0], code, consts, alloc)?;
                        let dst = alloc.alloc();
                        code.push(RegInstruction::Len { dst, src });
                        return Some(dst);
                    }
                }
            }
            let func = compile_reg_expr(function, code, consts, alloc)?;
            let dst = alloc.alloc();
            let cache = Rc::new(RefCell::new(crate::value::CallSiteCache::default()));
            match args.len()
            {
                0 =>
                {
                    code.push(RegInstruction::CallValueCached0 { dst, func, cache });
                }
                1 =>
                {
                    let arg0 = compile_reg_expr(&args[0], code, consts, alloc)?;
                    code.push(RegInstruction::CallValueCached1 {
                        dst,
                        func,
                        arg0,
                        cache,
                    });
                }
                2 =>
                {
                    let arg0 = compile_reg_expr(&args[0], code, consts, alloc)?;
                    let arg1 = compile_reg_expr(&args[1], code, consts, alloc)?;
                    code.push(RegInstruction::CallValueCached2 {
                        dst,
                        func,
                        arg0,
                        arg1,
                        cache,
                    });
                }
                3 =>
                {
                    let arg0 = compile_reg_expr(&args[0], code, consts, alloc)?;
                    let arg1 = compile_reg_expr(&args[1], code, consts, alloc)?;
                    let arg2 = compile_reg_expr(&args[2], code, consts, alloc)?;
                    code.push(RegInstruction::CallValueCached3 {
                        dst,
                        func,
                        arg0,
                        arg1,
                        arg2,
                        cache,
                    });
                }
                _ =>
                {
                    let mut arg_regs = Vec::with_capacity(args.len());
                    for arg in args
                    {
                        arg_regs.push(compile_reg_expr(arg, code, consts, alloc)?);
                    }
                    code.push(RegInstruction::CallValueCached {
                        dst,
                        func,
                        args: arg_regs,
                        cache,
                    });
                }
            }
            Some(dst)
        }
        ExprKind::Block(stmts) =>
        {
            let mut last = None;
            for stmt in stmts
            {
                last = compile_reg_expr(stmt, code, consts, alloc);
                if last.is_none()
                {
                    return None;
                }
            }
            last
        }
        _ => None,
    }
}

fn compile_reg_function(expr: &Expr) -> Option<RegFunction>
{
    let mut code = Vec::new();
    let mut consts = Vec::new();
    let mut alloc = RegAllocator::new();
    let ret_reg = compile_reg_expr(expr, &mut code, &mut consts, &mut alloc)?;
    Some(RegFunction {
        code,
        reg_count: alloc.next_reg,
        ret_reg,
        const_pool: Rc::new(consts),
    })
}

fn compile_fast_float_expr(
    expr: &Expr,
    code: &mut Vec<FastRegInstruction>,
    next_reg: &mut usize,
) -> Option<usize>
{
    match &expr.kind
    {
        ExprKind::Float {
            value,
            kind: FloatKind::F64,
        } =>
        {
            let dst = *next_reg;
            *next_reg += 1;
            code.push(FastRegInstruction::LoadConst { dst, value: *value });
            Some(dst)
        }
        ExprKind::Identifier {
            slot: Some(slot), ..
        } =>
        {
            let dst = *next_reg;
            *next_reg += 1;
            code.push(FastRegInstruction::LoadSlot { dst, slot: *slot });
            Some(dst)
        }
        ExprKind::BinaryOp { left, op, right } =>
        {
            let left = compile_fast_float_expr(left, code, next_reg)?;
            let right = compile_fast_float_expr(right, code, next_reg)?;
            let op = match op
            {
                Op::Add => RegBinOp::Add,
                Op::Subtract => RegBinOp::Sub,
                Op::Multiply => RegBinOp::Mul,
                Op::Divide => RegBinOp::Div,
                Op::Power => RegBinOp::Pow,
                _ => return None,
            };
            let dst = *next_reg;
            *next_reg += 1;
            code.push(FastRegInstruction::BinOp {
                dst,
                op,
                left,
                right,
            });
            Some(dst)
        }
        _ => None,
    }
}

fn compile_fast_float_function(expr: &Expr) -> Option<FastRegFunction>
{
    let mut code = Vec::new();
    let mut next_reg = 0usize;
    let ret_reg = compile_fast_float_expr(expr, &mut code, &mut next_reg)?;
    Some(FastRegFunction {
        code,
        reg_count: next_reg,
        ret_reg,
    })
}

fn is_inline_safe_arg(expr: &Expr) -> bool
{
    match &expr.kind
    {
        ExprKind::Integer { .. }
        | ExprKind::Unsigned { .. }
        | ExprKind::Float { .. }
        | ExprKind::Boolean(_)
        | ExprKind::Nil => true,
        ExprKind::Identifier { .. } => true,
        ExprKind::EnvFreeze(_) => false,
        ExprKind::Not(expr) => is_inline_safe_arg(expr),
        ExprKind::And { left, right } | ExprKind::AndBool { left, right } =>
        {
            is_inline_safe_arg(left) && is_inline_safe_arg(right)
        }
        ExprKind::Or { left, right } | ExprKind::OrBool { left, right } =>
        {
            is_inline_safe_arg(left) && is_inline_safe_arg(right)
        }
        ExprKind::BinaryOp { left, right, .. } =>
        {
            is_inline_safe_arg(left) && is_inline_safe_arg(right)
        }
        ExprKind::Index { target, index } =>
        {
            is_inline_safe_arg(target) && is_inline_safe_arg(index)
        }
        ExprKind::FormatString(parts) => parts.iter().all(|part| {
            if let crate::ast::FormatPart::Expr { expr, .. } = part
            {
                is_inline_safe_arg(expr)
            }
            else
            {
                true
            }
        }),
        _ => false,
    }
}

fn eval_map_index_cached(
    map: &MapValue,
    map_ptr: usize,
    key: &Rc<String>,
    cache: &Rc<RefCell<MapAccessCache>>,
) -> Value
{
    let mut cache_mut = cache.borrow_mut();
    if let Some(entry) = cache_mut.entries[0].as_ref()
    {
        if entry.map_ptr == map_ptr
            && entry.version == map.version
            && entry.key.as_ref() == key.as_ref()
        {
            let value = entry.value.clone();
            cache_mut.hits += 1;
            return value;
        }
    }
    if let Some(entry) = cache_mut.entries[1].as_ref()
    {
        if entry.map_ptr == map_ptr
            && entry.version == map.version
            && entry.key.as_ref() == key.as_ref()
        {
            let value = entry.value.clone();
            cache_mut.hits += 1;
            if let Some(entry1) = cache_mut.entries[1].take()
            {
                cache_mut.entries[1] = cache_mut.entries[0].take();
                cache_mut.entries[0] = Some(entry1);
            }
            return value;
        }
    }
    cache_mut.misses += 1;
    let value = map.data.get(key).cloned().unwrap_or(Value::Nil);
    let new_entry = MapAccessCacheEntry {
        map_ptr,
        version: map.version,
        key: key.clone(),
        value: value.clone(),
    };
    if let Some(entry0) = cache_mut.entries[0].take()
    {
        cache_mut.entries[1] = Some(entry0);
    }
    cache_mut.entries[0] = Some(new_entry);
    value
}

fn resolve_method_value(
    target_val: Value,
    name: &Rc<String>,
    map_cache: &Rc<RefCell<MapAccessCache>>,
) -> EvalResult
{
    let result = match target_val
    {
        Value::StructInstance(inst) =>
        {
            if let Some(method) = inst.ty.methods.borrow().get(name).cloned()
            {
                Value::BoundMethod(Rc::new(BoundMethod {
                    receiver: Value::StructInstance(inst.clone()),
                    func: method,
                }))
            }
            else
            {
                return Err(err_index_unsupported());
            }
        }
        Value::StructType(ty) => ty.methods.borrow().get(name).cloned().unwrap_or(Value::Nil),
        Value::Map(map) =>
        {
            if name.as_str() == "keys"
            {
                map_keys_array(&map.borrow())
            }
            else if name.as_str() == "values"
            {
                map_values_array(&map.borrow())
            }
            else
            {
                let map_ptr = Rc::as_ptr(&map) as usize;
                let map_ref = map.borrow();
                eval_map_index_cached(&map_ref, map_ptr, name, map_cache)
            }
        }
        Value::Env(env) =>
        {
            if name.as_str() == "keys"
            {
                env_keys_array(env.as_ref())
            }
            else if name.as_str() == "values"
            {
                env_values_array(env.as_ref())
            }
            else
            {
                let map_ptr = Rc::as_ptr(&env) as usize;
                let mut cache_mut = map_cache.borrow_mut();
                    if let Some(entry) = cache_mut.entries[0].as_ref()
                    {
                        if entry.map_ptr == map_ptr && entry.key.as_ref() == name.as_ref()
                        {
                            let value = entry.value.clone();
                            cache_mut.hits += 1;
                            return Ok(value);
                        }
                    }
                    if let Some(entry) = cache_mut.entries[1].as_ref()
                    {
                        if entry.map_ptr == map_ptr && entry.key.as_ref() == name.as_ref()
                        {
                            let value = entry.value.clone();
                            cache_mut.hits += 1;
                            if let Some(entry1) = cache_mut.entries[1].take()
                            {
                                cache_mut.entries[1] = cache_mut.entries[0].take();
                                cache_mut.entries[0] = Some(entry1);
                            }
                            return Ok(value);
                        }
                    }
                cache_mut.misses += 1;
                let value = env
                    .data
                    .get(name)
                    .map(env_clone_value)
                    .unwrap_or(Value::Nil);
                let new_entry = MapAccessCacheEntry {
                    map_ptr,
                    version: env.version,
                    key: name.clone(),
                    value: value.clone(),
                };
                if let Some(entry0) = cache_mut.entries[0].take()
                {
                    cache_mut.entries[1] = Some(entry0);
                }
                cache_mut.entries[0] = Some(new_entry);
                value
            }
        }
        Value::Array(_)
        | Value::F32Array(_)
        | Value::F64Array(_)
        | Value::I32Array(_)
        | Value::I64Array(_) =>
        {
            return Err(err_index_requires_int());
        }
        _ => return Err(err_index_unsupported()),
    };
    Ok(result)
}

fn eval_index_cached_value(
    index_val: Value,
    target_val: Value,
    cache: &Rc<RefCell<IndexCache>>,
) -> EvalResult
{
    let result = match target_val
    {
        Value::Array(arr) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let arr_ptr = Rc::as_ptr(&arr) as usize;
                let mut cache_mut = cache.borrow_mut();
                if cache_mut.array_ptr == Some(arr_ptr) && cache_mut.index_usize == Some(i)
                {
                    cache_mut.hits += 1;
                }
                else
                {
                    cache_mut.array_ptr = Some(arr_ptr);
                    cache_mut.index_usize = Some(i);
                    cache_mut.misses += 1;
                }
                let vec = arr.borrow();
                if i < vec.len()
                {
                    vec[i].clone()
                }
                else
                {
                    Value::Nil
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::F64Array(arr) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let arr_ptr = Rc::as_ptr(&arr) as usize;
                let mut cache_mut = cache.borrow_mut();
                if cache_mut.array_ptr == Some(arr_ptr) && cache_mut.index_usize == Some(i)
                {
                    cache_mut.hits += 1;
                }
                else
                {
                    cache_mut.array_ptr = Some(arr_ptr);
                    cache_mut.index_usize = Some(i);
                    cache_mut.misses += 1;
                }
                let vec = arr.borrow();
                if i < vec.len()
                {
                    make_float(vec[i], FloatKind::F64)
                }
                else
                {
                    Value::Nil
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::F32Array(arr) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let arr_ptr = Rc::as_ptr(&arr) as usize;
                let mut cache_mut = cache.borrow_mut();
                if cache_mut.array_ptr == Some(arr_ptr) && cache_mut.index_usize == Some(i)
                {
                    cache_mut.hits += 1;
                }
                else
                {
                    cache_mut.array_ptr = Some(arr_ptr);
                    cache_mut.index_usize = Some(i);
                    cache_mut.misses += 1;
                }
                let vec = arr.borrow();
                if i < vec.len()
                {
                    make_float(vec[i] as f64, FloatKind::F32)
                }
                else
                {
                    Value::Nil
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::I64Array(arr) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let arr_ptr = Rc::as_ptr(&arr) as usize;
                let mut cache_mut = cache.borrow_mut();
                if cache_mut.array_ptr == Some(arr_ptr) && cache_mut.index_usize == Some(i)
                {
                    cache_mut.hits += 1;
                }
                else
                {
                    cache_mut.array_ptr = Some(arr_ptr);
                    cache_mut.index_usize = Some(i);
                    cache_mut.misses += 1;
                }
                let vec = arr.borrow();
                if i < vec.len()
                {
                    make_signed_int(vec[i] as i128, IntKind::I64)
                }
                else
                {
                    Value::Nil
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::I32Array(arr) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let arr_ptr = Rc::as_ptr(&arr) as usize;
                let mut cache_mut = cache.borrow_mut();
                if cache_mut.array_ptr == Some(arr_ptr) && cache_mut.index_usize == Some(i)
                {
                    cache_mut.hits += 1;
                }
                else
                {
                    cache_mut.array_ptr = Some(arr_ptr);
                    cache_mut.index_usize = Some(i);
                    cache_mut.misses += 1;
                }
                let vec = arr.borrow();
                if i < vec.len()
                {
                    make_signed_int(vec[i] as i128, IntKind::I32)
                }
                else
                {
                    Value::Nil
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::Bytes(bytes) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let arr_ptr = Rc::as_ptr(&bytes) as usize;
                let mut cache_mut = cache.borrow_mut();
                if cache_mut.array_ptr == Some(arr_ptr) && cache_mut.index_usize == Some(i)
                {
                    cache_mut.hits += 1;
                }
                else
                {
                    cache_mut.array_ptr = Some(arr_ptr);
                    cache_mut.index_usize = Some(i);
                    cache_mut.misses += 1;
                }
                if i < bytes.len()
                {
                    default_int(bytes[i] as i128)
                }
                else
                {
                    Value::Nil
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::ByteBuf(buf) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let buf_ptr = Rc::as_ptr(&buf) as usize;
                let mut cache_mut = cache.borrow_mut();
                if cache_mut.array_ptr == Some(buf_ptr) && cache_mut.index_usize == Some(i)
                {
                    cache_mut.hits += 1;
                }
                else
                {
                    cache_mut.array_ptr = Some(buf_ptr);
                    cache_mut.index_usize = Some(i);
                    cache_mut.misses += 1;
                }
                let bytes = buf.borrow();
                if i < bytes.len()
                {
                    default_int(bytes[i] as i128)
                }
                else
                {
                    Value::Nil
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::BytesView(view) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let view_ptr = Rc::as_ptr(&view) as usize;
                let mut cache_mut = cache.borrow_mut();
                if cache_mut.array_ptr == Some(view_ptr) && cache_mut.index_usize == Some(i)
                {
                    cache_mut.hits += 1;
                }
                else
                {
                    cache_mut.array_ptr = Some(view_ptr);
                    cache_mut.index_usize = Some(i);
                    cache_mut.misses += 1;
                }
                if i < view.len
                {
                    let idx = view.offset + i;
                    let byte = match &view.source
                    {
                        crate::value::BytesViewSource::Mmap(mmap) => mmap[idx],
                        crate::value::BytesViewSource::MmapMut(mmap) =>
                        {
                            let data = mmap.borrow();
                            data[idx]
                        }
                    };
                    default_int(byte as i128)
                }
                else
                {
                    Value::Nil
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::Mmap(mmap) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let map_ptr = Rc::as_ptr(&mmap) as usize;
                let mut cache_mut = cache.borrow_mut();
                if cache_mut.array_ptr == Some(map_ptr) && cache_mut.index_usize == Some(i)
                {
                    cache_mut.hits += 1;
                }
                else
                {
                    cache_mut.array_ptr = Some(map_ptr);
                    cache_mut.index_usize = Some(i);
                    cache_mut.misses += 1;
                }
                if i < mmap.len()
                {
                    default_int(mmap[i] as i128)
                }
                else
                {
                    Value::Nil
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::MmapMut(mmap) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let map_ptr = Rc::as_ptr(&mmap) as usize;
                let mut cache_mut = cache.borrow_mut();
                if cache_mut.array_ptr == Some(map_ptr) && cache_mut.index_usize == Some(i)
                {
                    cache_mut.hits += 1;
                }
                else
                {
                    cache_mut.array_ptr = Some(map_ptr);
                    cache_mut.index_usize = Some(i);
                    cache_mut.misses += 1;
                }
                let bytes = mmap.borrow();
                if i < bytes.len()
                {
                    default_int(bytes[i] as i128)
                }
                else
                {
                    Value::Nil
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::StructInstance(inst) =>
        {
            if let Value::String(s) = index_val
            {
                if let Some(idx) = inst.ty.field_map.get(&s)
                {
                    let fields = inst.fields.borrow();
                    fields.get(*idx).cloned().unwrap_or(Value::Nil)
                }
                else if let Some(method) = inst.ty.methods.borrow().get(&s).cloned()
                {
                    Value::BoundMethod(Rc::new(BoundMethod {
                        receiver: Value::StructInstance(inst.clone()),
                        func: method,
                    }))
                }
                else
                {
                    Value::Nil
                }
            }
            else
            {
                return Err(err_index_unsupported());
            }
        }
        Value::StructType(ty) =>
        {
            if let Value::String(s) = index_val
            {
                ty.methods.borrow().get(&s).cloned().unwrap_or(Value::Nil)
            }
            else
            {
                return Err(err_index_unsupported());
            }
        }
        Value::Map(map) =>
        {
            if let Value::String(s) = index_val
            {
                if s.as_str() == "keys"
                {
                    map_keys_array(&map.borrow())
                }
                else if s.as_str() == "values"
                {
                    map_values_array(&map.borrow())
                }
                else
                {
                    let map_ptr = Rc::as_ptr(&map) as usize;
                    let map_ref = map.borrow();
                    let mut cache_mut = cache.borrow_mut();
                    if cache_mut.map_ptr == Some(map_ptr)
                        && cache_mut.version == map_ref.version
                        && cache_mut.key.as_ref() == Some(&s)
                    {
                        cache_mut.hits += 1;
                        cache_mut.value.clone().unwrap_or(Value::Nil)
                    }
                    else
                    {
                        let value = map_ref.data.get(&s).cloned().unwrap_or(Value::Nil);
                        cache_mut.map_ptr = Some(map_ptr);
                        cache_mut.version = map_ref.version;
                        cache_mut.key = Some(s.clone());
                        cache_mut.value = Some(value.clone());
                        cache_mut.misses += 1;
                        value
                    }
                }
            }
            else
            {
                let key = intern::intern_owned(index_val.inspect());
                map.borrow().data.get(&key).cloned().unwrap_or(Value::Nil)
            }
        }
        Value::Env(env) =>
        {
            if let Value::String(s) = index_val
            {
                if s.as_str() == "keys"
                {
                    env_keys_array(env.as_ref())
                }
                else if s.as_str() == "values"
                {
                    env_values_array(env.as_ref())
                }
                else
                {
                    let map_ptr = Rc::as_ptr(&env) as usize;
                    let mut cache_mut = cache.borrow_mut();
                    if cache_mut.map_ptr == Some(map_ptr) && cache_mut.key.as_ref() == Some(&s)
                    {
                        cache_mut.hits += 1;
                        cache_mut.value.clone().unwrap_or(Value::Nil)
                    }
                    else
                    {
                        let value = env.data.get(&s).map(env_clone_value).unwrap_or(Value::Nil);
                        cache_mut.map_ptr = Some(map_ptr);
                        cache_mut.key = Some(s.clone());
                        cache_mut.value = Some(value.clone());
                        cache_mut.misses += 1;
                        value
                    }
                }
            }
            else
            {
                let key = intern::intern_owned(index_val.inspect());
                env.data
                    .get(&key)
                    .map(env_clone_value)
                    .unwrap_or(Value::Nil)
            }
        }
        _ => return Err(err_index_unsupported()),
    };
    Ok(result)
}

fn eval_index_assign_value(
    interpreter: &mut Interpreter,
    target_val: Value,
    index_val: Value,
    value: Value,
) -> Result<Value, RuntimeError>
{
    match target_val
    {
        Value::Array(arr) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let mut vec = arr.borrow_mut();
                if i < vec.len()
                {
                    vec[i] = value.clone();
                }
                else
                {
                    return Err(RuntimeError::simple("Array index out of bounds".to_string(), 0));
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::F64Array(arr) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let mut vec = arr.borrow_mut();
                if i < vec.len()
                {
                    match &value
                    {
                        Value::Float { value, .. } => vec[i] = *value,
                        v =>
                        {
                            if let Some(num) = int_value_as_f64(v)
                            {
                                vec[i] = num;
                            }
                            else
                            {
                                return Err(RuntimeError::simple("F64Array assignment requires a number".to_string(), 0));
                            }
                        }
                    }
                }
                else
                {
                    return Err(RuntimeError::simple("Array index out of bounds".to_string(), 0));
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::F32Array(arr) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let mut vec = arr.borrow_mut();
                if i < vec.len()
                {
                    match &value
                    {
                        Value::Float { value, .. } => vec[i] = *value as f32,
                        v =>
                        {
                            if let Some(num) = int_value_as_f64(v)
                            {
                                vec[i] = num as f32;
                            }
                            else
                            {
                                return Err(RuntimeError::simple("F32Array assignment requires a number".to_string(), 0));
                            }
                        }
                    }
                }
                else
                {
                    return Err(RuntimeError::simple("Array index out of bounds".to_string(), 0));
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::I64Array(arr) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let mut vec = arr.borrow_mut();
                if i < vec.len()
                {
                    match &value
                    {
                        Value::Integer { value, .. } => vec[i] = *value as i64,
                        Value::Unsigned { value, .. } => vec[i] = *value as i64,
                        _ =>
                        {
                            return Err(RuntimeError::simple("I64Array assignment requires an integer".to_string(), 0));
                        }
                    }
                }
                else
                {
                    return Err(RuntimeError::simple("Array index out of bounds".to_string(), 0));
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::I32Array(arr) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let mut vec = arr.borrow_mut();
                if i < vec.len()
                {
                    match &value
                    {
                        Value::Integer { value, .. } => vec[i] = *value as i32,
                        Value::Unsigned { value, .. } => vec[i] = *value as i32,
                        _ =>
                        {
                            return Err(RuntimeError::simple("I32Array assignment requires an integer".to_string(), 0));
                        }
                    }
                }
                else
                {
                    return Err(RuntimeError::simple("Array index out of bounds".to_string(), 0));
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::ByteBuf(buf) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let byte = match &value
                {
                    Value::Integer { value, .. } => *value,
                    Value::Unsigned { value, .. } => *value as i128,
                    _ =>
                    {
                        return Err(RuntimeError::simple("ByteBuf assignment requires an integer".to_string(), 0));
                    }
                };
                if byte < 0 || byte > 255
                {
                    return Err(RuntimeError::simple("ByteBuf assignment requires byte (0-255)".to_string(), 0));
                }
                let mut vec = buf.borrow_mut();
                if i < vec.len()
                {
                    vec[i] = byte as u8;
                }
                else
                {
                    return Err(RuntimeError::simple("ByteBuf index out of bounds".to_string(), 0));
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::MmapMut(mmap) =>
        {
            if let Some(i) = int_value_as_usize(&index_val)
            {
                let byte = match &value
                {
                    Value::Integer { value, .. } => *value,
                    Value::Unsigned { value, .. } => *value as i128,
                    _ =>
                    {
                        return Err(RuntimeError::simple("MmapMut assignment requires an integer".to_string(), 0));
                    }
                };
                if byte < 0 || byte > 255
                {
                    return Err(RuntimeError::simple("MmapMut assignment requires byte (0-255)".to_string(), 0));
                }
                let mut vec = mmap.borrow_mut();
                if i < vec.len()
                {
                    vec[i] = byte as u8;
                }
                else
                {
                    return Err(RuntimeError::simple("MmapMut index out of bounds".to_string(), 0));
                }
            }
            else
            {
                return Err(err_index_requires_int());
            }
        }
        Value::StructInstance(inst) =>
        {
            let key = match index_val
            {
                Value::String(s) => s,
                _ => return Err(err_index_unsupported()),
            };
            let idx = inst.ty.field_map.get(&key).ok_or_else(|| RuntimeError::simple(format!("Unknown field '{}'", key.as_str()), 0))?;
            let field = inst.ty.fields.get(*idx).ok_or_else(|| RuntimeError::simple("Struct field out of bounds".to_string(), 0))?;
            let resolved = resolve_type_ref(&interpreter.env, &field.type_ref, 0)?;
            let coerced = coerce_value_to_type(value.clone(), &resolved, 0, key.as_str())?;
            inst.fields.borrow_mut()[*idx] = coerced.clone();
            return Ok(coerced);
        }
        Value::Map(map) =>
        {
            let key = match index_val
            {
                Value::String(s) => s,
                _ => intern::intern_owned(index_val.inspect()),
            };
            let mut map_mut = map.borrow_mut();
            map_mut.data.insert(key, value.clone());
            map_mut.version = map_mut.version.wrapping_add(1);
        }
        Value::Env(_) =>
        {
            return Err(RuntimeError::simple("Env is immutable".to_string(), 0));
        }
        _ =>
        {
            return Err(RuntimeError::simple("Index assignment not supported on this type".to_string(), 0));
        }
    }
    Ok(value)
}

fn lookup_env_value_with_owner(
    env_rc: &Rc<RefCell<Environment>>,
    name: SymbolId,
) -> Option<(Value, usize, u64)>
{
    let idx = name as usize;
    let (parent, is_partial) = {
        let env = env_rc.borrow();
        if idx < env.values.len()
        {
            let v = env.values[idx].clone();
            let val = match v
            {
                Value::Reference(r) => r.borrow().clone(),
                _ => v,
            };
            let ptr = Rc::as_ptr(env_rc) as usize;
            return Some((val, ptr, env.version));
        }
        (env.parent.clone(), env.is_partial)
    };
    if let Some(parent_rc) = parent
    {
        if is_partial
        {
            return lookup_env_value_with_owner(&parent_rc, name);
        }
        return lookup_env_value_recursive_with_owner(&parent_rc, name);
    }
    None
}

fn lookup_env_value_recursive_with_owner(
    env_rc: &Rc<RefCell<Environment>>,
    name: SymbolId,
) -> Option<(Value, usize, u64)>
{
    let idx = name as usize;
    let parent = {
        let env = env_rc.borrow();
        if idx < env.values.len()
        {
            let v = env.values[idx].clone();
            return match v
            {
                Value::Reference(r) =>
                {
                    Some((r.borrow().clone(), Rc::as_ptr(env_rc) as usize, env.version))
                }
                Value::Function(_) => Some((v, Rc::as_ptr(env_rc) as usize, env.version)),
                _ =>
                {
                    if env.is_partial
                    {
                        Some((v, Rc::as_ptr(env_rc) as usize, env.version))
                    }
                    else
                    {
                        None
                    }
                }
            };
        }
        env.parent.clone()
    };
    if let Some(parent_rc) = parent
    {
        return lookup_env_value_recursive_with_owner(&parent_rc, name);
    }
    None
}

fn load_global_cached(
    interpreter: &mut Interpreter,
    name: SymbolId,
    cache: &Rc<RefCell<GlobalCache>>,
) -> EvalResult
{
    let cached = {
        let cache_ref = cache.borrow();
        if let Some(env_ptr) = cache_ref.env_ptr
        {
            let mut cached_val = None;
            let mut current = Some(interpreter.env.clone());
            while let Some(env_rc) = current
            {
                let env_ref = env_rc.borrow();
                if Rc::as_ptr(&env_rc) as usize == env_ptr
                {
                    if env_ref.version == cache_ref.version
                    {
                        cached_val = cache_ref.value.clone();
                    }
                    break;
                }
                current = env_ref.parent.clone();
            }
            cached_val
        }
        else
        {
            None
        }
    };
    if let Some(val) = cached
    {
        cache.borrow_mut().hits += 1;
        if let Value::Uninitialized = val
        {
            return Err(RuntimeError::simple(format!(
                    "Variable '{}' used before assignment",
                    symbol_name(name).as_str()
                ), 0));
        }
        return Ok(val);
    }

    let (val, env_ptr, version) =
        lookup_env_value_with_owner(&interpreter.env, name).ok_or_else(|| RuntimeError::simple(format!("Undefined variable: {}", symbol_name(name).as_str()), 0))?;
    cache.borrow_mut().misses += 1;
    {
        let mut cache_mut = cache.borrow_mut();
        cache_mut.env_ptr = Some(env_ptr);
        cache_mut.version = version;
        cache_mut.value = Some(val.clone());
    }
    if let Value::Uninitialized = val
    {
        return Err(RuntimeError::simple(format!("Variable '{}' used before assignment", symbol_name(name).as_str()), 0));
    }
    Ok(val)
}

fn eval_call_value_cached(
    interpreter: &mut Interpreter,
    func_val: Value,
    arg_vals: smallvec::SmallVec<[Value; 8]>,
    cache: &Rc<RefCell<CallSiteCache>>,
) -> EvalResult
{
    eval_call_value_cached_generic(interpreter, func_val, arg_vals, cache, None)
}

fn eval_call_value_cached_with_block(
    interpreter: &mut Interpreter,
    func_val: Value,
    arg_vals: smallvec::SmallVec<[Value; 8]>,
    cache: &Rc<RefCell<CallSiteCache>>,
    block: &Rc<Closure>,
) -> EvalResult
{
    eval_call_value_cached_generic(interpreter, func_val, arg_vals, cache, Some(block))
}

fn eval_call_value_cached_generic(
    interpreter: &mut Interpreter,
    func_val: Value,
    arg_vals: smallvec::SmallVec<[Value; 8]>,
    cache: &Rc<RefCell<CallSiteCache>>,
    block: Option<&Rc<Closure>>,
) -> EvalResult
{
    let block_owned = block.map(|b| b.clone());
    if let Value::Function(func_data) = &func_val
    {
        let func_ptr = Rc::as_ptr(func_data) as usize;
        let mut cache_mut = cache.borrow_mut();
        if cache_mut.func_ptr == Some(func_ptr)
        {
            cache_mut.hits += 1;
            let arg_len = arg_vals.len();
            if arg_len < func_data.params.len()
            {
                return interpreter.invoke_function(func_data.clone(), arg_vals, 0, block_owned);
            }
            if arg_len > func_data.params.len()
            {
                return Err(RuntimeError::simple("Too many arguments".to_string(), 0));
            }
            if block.is_some()
            {
                return interpreter.invoke_function(func_data.clone(), arg_vals, 0, block_owned);
            }
            let mut coerced_args: smallvec::SmallVec<[Value; 8]> = smallvec::SmallVec::new();
            for (param, val) in func_data.params.iter().zip(arg_vals.iter().cloned())
            {
                let coerced = coerce_param_value(&func_data.env, param, val, 0)?;
                coerced_args.push(coerced);
            }
            if let Some(fast) = &func_data.fast_reg_code
            {
                let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(
                    Value::Uninitialized,
                    func_data.declarations.len(),
                );
                interpreter.ensure_slot_capacity(
                    &mut new_slots,
                    func_data.param_offset,
                    func_data.params.len(),
                    &func_data.bound_args,
                );
                interpreter.apply_bound_args(&func_data.bound_args, &mut new_slots);
                for (i, val) in coerced_args.iter().cloned().enumerate()
                {
                    new_slots[i + func_data.param_offset] = val;
                }
                if let Some(result) = try_execute_fast_float_reg(fast, &mut new_slots)
                {
                    return Ok(result);
                }
            }
            if let Some(reg_code) = &func_data.reg_code
            {
                let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(
                    Value::Uninitialized,
                    func_data.declarations.len(),
                );
                interpreter.ensure_slot_capacity(
                    &mut new_slots,
                    func_data.param_offset,
                    func_data.params.len(),
                    &func_data.bound_args,
                );
                interpreter.apply_bound_args(&func_data.bound_args, &mut new_slots);
                for (i, val) in coerced_args.iter().cloned().enumerate()
                {
                    new_slots[i + func_data.param_offset] = val;
                }
                return execute_reg_instructions(interpreter, reg_code, &mut new_slots);
            }
            if let Some(code) = &func_data.code
            {
                let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(
                    Value::Uninitialized,
                    func_data.declarations.len(),
                );
                interpreter.ensure_slot_capacity(
                    &mut new_slots,
                    func_data.param_offset,
                    func_data.params.len(),
                    &func_data.bound_args,
                );
                interpreter.apply_bound_args(&func_data.bound_args, &mut new_slots);
                for (i, val) in coerced_args.iter().cloned().enumerate()
                {
                    new_slots[i + func_data.param_offset] = val;
                }
                let result = if func_data.uses_env
                {
                    let original_env = interpreter.env.clone();
                    interpreter.env = func_data.env.clone();
                    let result = execute_instructions(
                        interpreter,
                        code,
                        &func_data.const_pool,
                        &mut new_slots,
                    );
                    interpreter.env = original_env;
                    result?
                }
                else
                {
                    execute_instructions(interpreter, code, &func_data.const_pool, &mut new_slots)?
                };
                return Ok(result);
            }
        }
        else
        {
            cache_mut.func_ptr = Some(func_ptr);
            cache_mut.native_ptr = None;
            cache_mut.misses += 1;
        }
    }
    else if let Value::NativeFunction(func) = &func_val
    {
        let func_ptr = *func as usize;
        let mut cache_mut = cache.borrow_mut();
        if block.is_none() && cache_mut.native_ptr == Some(func_ptr)
        {
            cache_mut.hits += 1;
            return func(&arg_vals).map_err(|message| RuntimeError::simple(message, 0));
        }
        cache_mut.native_ptr = Some(func_ptr);
        cache_mut.func_ptr = None;
        cache_mut.misses += 1;
    }
    else
    {
        let mut cache_mut = cache.borrow_mut();
        cache_mut.func_ptr = None;
        cache_mut.native_ptr = None;
        cache_mut.misses += 1;
    }
    interpreter.call_value(func_val, arg_vals, 0, block_owned)
}

fn reg_binop_kind(op: RegBinOp) -> BinOpKind
{
    match op
    {
        RegBinOp::Add => BinOpKind::Add,
        RegBinOp::Sub => BinOpKind::Sub,
        RegBinOp::Mul => BinOpKind::Mul,
        RegBinOp::Div => BinOpKind::Div,
        RegBinOp::Pow => BinOpKind::Pow,
        RegBinOp::Eq => BinOpKind::Eq,
        RegBinOp::Gt => BinOpKind::Gt,
        RegBinOp::Lt => BinOpKind::Lt,
    }
}

fn eval_f64_index_cached_value(
    index_val: Value,
    target_val: Value,
    cache: &Rc<RefCell<IndexCache>>,
) -> EvalResult
{
    match target_val
    {
        Value::F64Array(arr) =>
        {
            let idx = match index_val
            {
                Value::Integer { value, .. } if value >= 0 => value as usize,
                Value::Unsigned { value, .. } => value as usize,
                _ =>
                {
                    return Err(err_index_requires_int());
                }
            };
            let arr_ptr = Rc::as_ptr(&arr) as usize;
            let mut cache_mut = cache.borrow_mut();
            if cache_mut.array_ptr == Some(arr_ptr) && cache_mut.index_usize == Some(idx)
            {
                cache_mut.hits += 1;
            }
            else
            {
                cache_mut.array_ptr = Some(arr_ptr);
                cache_mut.index_usize = Some(idx);
                cache_mut.misses += 1;
            }
            let vec = arr.borrow();
            if idx < vec.len()
            {
                Ok(make_float(vec[idx], FloatKind::F64))
            }
            else
            {
                Ok(Value::Nil)
            }
        }
        other => eval_index_cached_value(index_val, other, cache),
    }
}

fn eval_map_index_cached_value(
    index_val: Value,
    target_val: Value,
    cache: &Rc<RefCell<MapAccessCache>>,
) -> EvalResult
{
    let result = match target_val
    {
        Value::StructInstance(inst) =>
        {
            if let Value::String(s) = index_val
            {
                if let Some(idx) = inst.ty.field_map.get(&s)
                {
                    let fields = inst.fields.borrow();
                    fields.get(*idx).cloned().unwrap_or(Value::Nil)
                }
                else if let Some(method) = inst.ty.methods.borrow().get(&s).cloned()
                {
                    Value::BoundMethod(Rc::new(BoundMethod {
                        receiver: Value::StructInstance(inst.clone()),
                        func: method,
                    }))
                }
                else
                {
                    Value::Nil
                }
            }
            else
            {
                return Err(err_index_unsupported());
            }
        }
        Value::StructType(ty) =>
        {
            if let Value::String(s) = index_val
            {
                ty.methods.borrow().get(&s).cloned().unwrap_or(Value::Nil)
            }
            else
            {
                return Err(err_index_unsupported());
            }
        }
        Value::Map(map) =>
        {
            if let Value::String(s) = index_val
            {
                if s.as_str() == "keys"
                {
                    map_keys_array(&map.borrow())
                }
                else if s.as_str() == "values"
                {
                    map_values_array(&map.borrow())
                }
                else
                {
                    let map_ptr = Rc::as_ptr(&map) as usize;
                    let map_ref = map.borrow();
                    eval_map_index_cached(&map_ref, map_ptr, &s, cache)
                }
            }
            else
            {
                let key = intern::intern_owned(index_val.inspect());
                map.borrow().data.get(&key).cloned().unwrap_or(Value::Nil)
            }
        }
        Value::Env(env) =>
        {
            if let Value::String(s) = index_val
            {
                if s.as_str() == "keys"
                {
                    env_keys_array(env.as_ref())
                }
                else if s.as_str() == "values"
                {
                    env_values_array(env.as_ref())
                }
                else
                {
                    let map_ptr = Rc::as_ptr(&env) as usize;
                    let mut cache_mut = cache.borrow_mut();
                    if let Some(entry) = cache_mut.entries[0].clone()
                    {
                        if entry.map_ptr == map_ptr && entry.key.as_ref() == s.as_ref()
                        {
                            cache_mut.hits += 1;
                            return Ok(entry.value);
                        }
                    }
                    if let Some(entry) = cache_mut.entries[1].clone()
                    {
                        if entry.map_ptr == map_ptr && entry.key.as_ref() == s.as_ref()
                        {
                            cache_mut.hits += 1;
                            if let Some(entry1) = cache_mut.entries[1].take()
                            {
                                cache_mut.entries[1] = cache_mut.entries[0].take();
                                cache_mut.entries[0] = Some(entry1);
                            }
                            return Ok(entry.value);
                        }
                    }
                    cache_mut.misses += 1;
                    let value = env.data.get(&s).map(env_clone_value).unwrap_or(Value::Nil);
                    let new_entry = MapAccessCacheEntry {
                        map_ptr,
                        version: env.version,
                        key: s.clone(),
                        value: value.clone(),
                    };
                    if let Some(entry0) = cache_mut.entries[0].take()
                    {
                        cache_mut.entries[1] = Some(entry0);
                    }
                    cache_mut.entries[0] = Some(new_entry);
                    value
                }
            }
            else
            {
                let key = intern::intern_owned(index_val.inspect());
                env.data
                    .get(&key)
                    .map(env_clone_value)
                    .unwrap_or(Value::Nil)
            }
        }
        Value::Array(_)
        | Value::F32Array(_)
        | Value::F64Array(_)
        | Value::I32Array(_)
        | Value::I64Array(_) =>
        {
            return Err(err_index_requires_int());
        }
        _ => return Err(err_index_unsupported()),
    };
    Ok(result)
}

fn execute_reg_instructions(
    interpreter: &mut Interpreter,
    reg: &RegFunction,
    slots: &mut [Value],
) -> EvalResult
{
    let mut regs = interpreter.get_reg_buffer(reg.reg_count);
    let result = (|| {
        for inst in &reg.code
        {
            match inst
            {
                RegInstruction::LoadConst { dst, idx } =>
                {
                    let val = reg.const_pool.get(*idx).cloned().unwrap_or(Value::Nil);
                    regs[*dst] = val;
                }
                RegInstruction::LoadSlot { dst, slot } =>
                {
                    regs[*dst] = slots[*slot].clone();
                }
                RegInstruction::StoreSlot { slot, src } =>
                {
                    slots[*slot] = regs[*src].clone();
                }
                RegInstruction::CloneValue { dst, src } =>
                {
                    regs[*dst] = clone_value(&regs[*src]);
                }
                RegInstruction::BinOpCached {
                    dst,
                    op,
                    left,
                    right,
                    cache,
                } =>
                {
                    let l = regs[*left].clone();
                    let r = regs[*right].clone();
                    regs[*dst] = eval_cached_binop(reg_binop_kind(*op), cache, l, r)?;
                }
                RegInstruction::MapIndexCached {
                    dst,
                    target,
                    index,
                    cache,
                } =>
                {
                    let target_val = regs[*target].clone();
                    let index_val = regs[*index].clone();
                    let result = match target_val
                    {
                        Value::StructInstance(inst) =>
                        {
                            if let Value::String(s) = index_val
                            {
                                if let Some(idx) = inst.ty.field_map.get(&s)
                                {
                                    let fields = inst.fields.borrow();
                                    fields.get(*idx).cloned().unwrap_or(Value::Nil)
                                }
                                else if let Some(method) =
                                    inst.ty.methods.borrow().get(&s).cloned()
                                {
                                    Value::BoundMethod(Rc::new(BoundMethod {
                                        receiver: Value::StructInstance(inst.clone()),
                                        func: method,
                                    }))
                                }
                                else
                                {
                                    Value::Nil
                                }
                            }
                            else
                            {
                                return Err(err_index_unsupported());
                            }
                        }
                        Value::StructType(ty) =>
                        {
                            if let Value::String(s) = index_val
                            {
                                ty.methods.borrow().get(&s).cloned().unwrap_or(Value::Nil)
                            }
                            else
                            {
                                return Err(err_index_unsupported());
                            }
                        }
                        Value::Map(map) =>
                        {
                            if let Value::String(s) = index_val
                            {
                                if s.as_str() == "keys"
                                {
                                    map_keys_array(&map.borrow())
                                }
                                else if s.as_str() == "values"
                                {
                                    map_values_array(&map.borrow())
                                }
                                else
                                {
                                    let map_ptr = Rc::as_ptr(&map) as usize;
                                    let map_ref = map.borrow();
                                    eval_map_index_cached(&map_ref, map_ptr, &s, cache)
                                }
                            }
                            else
                            {
                                let key = intern::intern_owned(index_val.inspect());
                                map.borrow().data.get(&key).cloned().unwrap_or(Value::Nil)
                            }
                        }
                        Value::Array(_)
                        | Value::F32Array(_)
                        | Value::F64Array(_)
                        | Value::I32Array(_)
                        | Value::I64Array(_) =>
                        {
                            return Err(err_index_requires_int());
                        }
                        _ => return Err(err_index_unsupported()),
                    };
                    regs[*dst] = result;
                }
                RegInstruction::F64IndexCached {
                    dst,
                    target,
                    index,
                    cache,
                } =>
                {
                    let target_val = regs[*target].clone();
                    let index_val = regs[*index].clone();
                    let result = match target_val
                    {
                        Value::F64Array(arr) =>
                        {
                            let idx = match index_val
                            {
                                Value::Integer { value, .. } if value >= 0 => value as usize,
                                Value::Unsigned { value, .. } => value as usize,
                                _ =>
                                {
                                    return Err(err_index_requires_int());
                                }
                            };
                            let arr_ptr = Rc::as_ptr(&arr) as usize;
                            let mut cache_mut = cache.borrow_mut();
                            if cache_mut.array_ptr == Some(arr_ptr)
                                && cache_mut.index_usize == Some(idx)
                            {
                                cache_mut.hits += 1;
                            }
                            else
                            {
                                cache_mut.array_ptr = Some(arr_ptr);
                                cache_mut.index_usize = Some(idx);
                                cache_mut.misses += 1;
                            }
                            let vec = arr.borrow();
                            if idx < vec.len()
                            {
                                make_float(vec[idx], FloatKind::F64)
                            }
                            else
                            {
                                Value::Nil
                            }
                        }
                        other => eval_index_cached_value(index_val, other, cache)?,
                    };
                    regs[*dst] = result;
                }
                RegInstruction::F64IndexAssignCached {
                    dst,
                    target,
                    index,
                    value,
                    cache,
                } =>
                {
                    let target_val = regs[*target].clone();
                    let index_val = regs[*index].clone();
                    let value_val = regs[*value].clone();
                    let result = match target_val
                    {
                        Value::F64Array(arr) =>
                        {
                            let idx = match index_val
                            {
                                Value::Integer { value, .. } if value >= 0 => value as usize,
                                Value::Unsigned { value, .. } => value as usize,
                                _ =>
                                {
                                    let fallback = Value::F64Array(arr.clone());
                                    return eval_index_assign_value(
                                        interpreter,
                                        fallback,
                                        index_val,
                                        value_val,
                                    );
                                }
                            };
                            let num = match &value_val
                            {
                                Value::Float { value, .. } => *value,
                                Value::Integer { value, .. } => *value as f64,
                                Value::Unsigned { value, .. } => *value as f64,
                                other =>
                                {
                                    let fallback = Value::F64Array(arr.clone());
                                    return eval_index_assign_value(
                                        interpreter,
                                        fallback,
                                        index_val,
                                        other.clone(),
                                    );
                                }
                            };
                            let arr_ptr = Rc::as_ptr(&arr) as usize;
                            let mut cache_mut = cache.borrow_mut();
                            if cache_mut.array_ptr == Some(arr_ptr)
                                && cache_mut.index_usize == Some(idx)
                            {
                                cache_mut.hits += 1;
                            }
                            else
                            {
                                cache_mut.array_ptr = Some(arr_ptr);
                                cache_mut.index_usize = Some(idx);
                                cache_mut.misses += 1;
                            }
                            let mut vec = arr.borrow_mut();
                            if idx < vec.len()
                            {
                                vec[idx] = num;
                                value_val
                            }
                            else
                            {
                                return Err(RuntimeError::simple("Array index out of bounds".to_string(), 0));
                            }
                        }
                        other => eval_index_assign_value(interpreter, other, index_val, value_val)?,
                    };
                    regs[*dst] = result;
                }
                RegInstruction::CallValueCached0 { dst, func, cache } =>
                {
                    let func_val = regs[*func].clone();
                    let arg_vals: smallvec::SmallVec<[Value; 8]> = smallvec::SmallVec::new();
                    regs[*dst] = eval_call_value_cached(interpreter, func_val, arg_vals, cache)?;
                }
                RegInstruction::CallValueCached1 {
                    dst,
                    func,
                    arg0,
                    cache,
                } =>
                {
                    let func_val = regs[*func].clone();
                    let mut arg_vals: smallvec::SmallVec<[Value; 8]> = smallvec::SmallVec::new();
                    arg_vals.push(regs[*arg0].clone());
                    regs[*dst] = eval_call_value_cached(interpreter, func_val, arg_vals, cache)?;
                }
                RegInstruction::CallValueCached2 {
                    dst,
                    func,
                    arg0,
                    arg1,
                    cache,
                } =>
                {
                    let func_val = regs[*func].clone();
                    let mut arg_vals: smallvec::SmallVec<[Value; 8]> = smallvec::SmallVec::new();
                    arg_vals.push(regs[*arg0].clone());
                    arg_vals.push(regs[*arg1].clone());
                    regs[*dst] = eval_call_value_cached(interpreter, func_val, arg_vals, cache)?;
                }
                RegInstruction::CallValueCached3 {
                    dst,
                    func,
                    arg0,
                    arg1,
                    arg2,
                    cache,
                } =>
                {
                    let func_val = regs[*func].clone();
                    let mut arg_vals: smallvec::SmallVec<[Value; 8]> = smallvec::SmallVec::new();
                    arg_vals.push(regs[*arg0].clone());
                    arg_vals.push(regs[*arg1].clone());
                    arg_vals.push(regs[*arg2].clone());
                    regs[*dst] = eval_call_value_cached(interpreter, func_val, arg_vals, cache)?;
                }
                RegInstruction::CallValueCached {
                    dst,
                    func,
                    args,
                    cache,
                } =>
                {
                    let func_val = regs[*func].clone();
                    let mut arg_vals: smallvec::SmallVec<[Value; 8]> = smallvec::SmallVec::new();
                    for reg_idx in args
                    {
                        arg_vals.push(regs[*reg_idx].clone());
                    }
                    regs[*dst] = eval_call_value_cached(interpreter, func_val, arg_vals, cache)?;
                }
                RegInstruction::Len { dst, src } =>
                {
                    let val = regs[*src].clone();
                    regs[*dst] = match val
                    {
                        Value::String(s) => default_int(s.len() as i128),
                        Value::Array(arr) => default_int(arr.borrow().len() as i128),
                        Value::F32Array(arr) => default_int(arr.borrow().len() as i128),
                        Value::F64Array(arr) => default_int(arr.borrow().len() as i128),
                        Value::I32Array(arr) => default_int(arr.borrow().len() as i128),
                        Value::I64Array(arr) => default_int(arr.borrow().len() as i128),
                        Value::Bytes(bytes) => default_int(bytes.len() as i128),
                        Value::ByteBuf(buf) => default_int(buf.borrow().len() as i128),
                        Value::BytesView(view) => default_int(view.len as i128),
                        Value::Map(map) => default_int(map.borrow().data.len() as i128),
                        Value::Env(env) => default_int(env.data.len() as i128),
                        Value::Mmap(mmap) => default_int(mmap.len() as i128),
                        Value::MmapMut(mmap) => default_int(mmap.borrow().len() as i128),
                        _ => default_int(0),
                    };
                }
                RegInstruction::MapKeys { dst, src } =>
                {
                    let val = regs[*src].clone();
                    regs[*dst] = match val
                    {
                        Value::Map(map) => map_keys_array(&map.borrow()),
                        Value::Env(env) => env_keys_array(env.as_ref()),
                        _ => Value::Nil,
                    };
                }
                RegInstruction::MapValues { dst, src } =>
                {
                    let val = regs[*src].clone();
                    regs[*dst] = match val
                    {
                        Value::Map(map) => map_values_array(&map.borrow()),
                        Value::Env(env) => env_values_array(env.as_ref()),
                        _ => Value::Nil,
                    };
                }
            }
        }
        Ok(regs.get(reg.ret_reg).cloned().unwrap_or(Value::Nil))
    })();
    interpreter.recycle_reg_buffer(regs);
    result
}

fn try_execute_fast_float_reg(fast: &FastRegFunction, slots: &mut [Value]) -> Option<Value>
{
    let mut regs = vec![0.0f64; fast.reg_count];
    for inst in &fast.code
    {
        match inst
        {
            FastRegInstruction::LoadConst { dst, value } =>
            {
                regs[*dst] = *value;
            }
            FastRegInstruction::LoadSlot { dst, slot } =>
            {
                let val = slots.get(*slot)?.clone();
                match val
                {
                    Value::Float {
                        value,
                        kind: FloatKind::F64,
                    } =>
                    {
                        regs[*dst] = value;
                    }
                    _ => return None,
                }
            }
            FastRegInstruction::BinOp {
                dst,
                op,
                left,
                right,
            } =>
            {
                let l = regs[*left];
                let r = regs[*right];
                regs[*dst] = match op
                {
                    RegBinOp::Add => l + r,
                    RegBinOp::Sub => l - r,
                    RegBinOp::Mul => l * r,
                    RegBinOp::Div => l / r,
                    RegBinOp::Pow => l.powf(r),
                    _ => return None,
                };
            }
        }
    }
    let value = regs.get(fast.ret_reg)?;
    Some(make_float(*value, FloatKind::F64))
}

fn pop_args_from_stack(
    stack: &mut Vec<Value>,
    argc: usize,
) -> Result<smallvec::SmallVec<[Value; 8]>, RuntimeError>
{
    if argc > stack.len()
    {
        return Err(RuntimeError::simple("Invalid argument count".to_string(), 0));
    }
    let mut args = smallvec::SmallVec::<[Value; 8]>::with_capacity(argc);
    for _ in 0..argc
    {
        args.push(stack.pop().unwrap());
    }
    args.reverse();
    Ok(args)
}

fn pop_method_target_and_resolve(
    stack: &mut Vec<Value>,
    name: &Rc<String>,
    map_cache: &Rc<RefCell<MapAccessCache>>,
) -> EvalResult
{
    let target_val = stack.pop().ok_or_else(|| RuntimeError::simple("Missing target for method call".to_string(), 0))?;
    resolve_method_value(target_val, name, map_cache)
}

fn err_index_requires_int() -> RuntimeError
{
    RuntimeError::simple("Array index must be an integer".to_string(), 0)
}

fn err_index_unsupported() -> RuntimeError
{
    RuntimeError::simple("Index operator not supported on this type".to_string(), 0)
}

enum RangeEndNum
{
    Int(i64),
    Float(f64),
}

fn range_end_value(end: &RangeEnd, slots: &[Value], const_pool: &[Value]) -> Value
{
    match end
    {
        RangeEnd::Slot(s) => slots.get(*s).cloned().unwrap_or(Value::Nil),
        RangeEnd::Const(idx) => const_pool.get(*idx).cloned().unwrap_or(Value::Nil),
    }
}

fn range_end_f64(end: &RangeEnd, slots: &[Value], const_pool: &[Value])
-> Result<f64, RuntimeError>
{
    let end_val = range_end_value(end, slots, const_pool);
    match end_val
    {
        Value::Float { value, kind } => Ok(normalize_float_value(value, kind)),
        _ => int_value_as_f64(&end_val).ok_or_else(|| RuntimeError::simple("Range end must be a number".to_string(), 0)),
    }
}

fn range_end_num(
    end: &RangeEnd,
    slots: &[Value],
    const_pool: &[Value],
) -> Result<RangeEndNum, RuntimeError>
{
    let end_val = range_end_value(end, slots, const_pool);
    match end_val
    {
        Value::Float { value, kind } => Ok(RangeEndNum::Float(normalize_float_value(value, kind))),
        _ => int_value_as_i64(&end_val)
            .map(RangeEndNum::Int)
            .ok_or_else(|| RuntimeError::simple("Range end must be a number".to_string(), 0)),
    }
}

#[derive(Clone)]
enum HotInstr
{
    LoadConstIdx(usize),
    LoadSlot(usize),
    StoreSlot(usize),
    Pop,
    Dup,
    Not,
    CheckBool,
    JumpIfFalse(usize),
    Jump(usize),
    BinOp(BinOpKind),
    BinOpCached(BinOpKind, Rc<RefCell<BinaryOpCache>>),
    IndexCached(Rc<RefCell<IndexCache>>),
    F64IndexCached(Rc<RefCell<IndexCache>>),
    MapIndexCached(Rc<RefCell<MapAccessCache>>),
}

fn build_hot_cache(code: &[Instruction]) -> Rc<Vec<Option<HotInstr>>>
{
    let mut out = Vec::with_capacity(code.len());
    for inst in code
    {
        let hot = match inst
        {
            Instruction::LoadConstIdx(idx) => Some(HotInstr::LoadConstIdx(*idx)),
            Instruction::LoadSlot(slot) => Some(HotInstr::LoadSlot(*slot)),
            Instruction::StoreSlot(slot) => Some(HotInstr::StoreSlot(*slot)),
            Instruction::Pop => Some(HotInstr::Pop),
            Instruction::Dup => Some(HotInstr::Dup),
            Instruction::Not => Some(HotInstr::Not),
            Instruction::CheckBool => Some(HotInstr::CheckBool),
            Instruction::JumpIfFalse(target) => Some(HotInstr::JumpIfFalse(*target)),
            Instruction::Jump(target) => Some(HotInstr::Jump(*target)),
            Instruction::Add => Some(HotInstr::BinOp(BinOpKind::Add)),
            Instruction::Sub => Some(HotInstr::BinOp(BinOpKind::Sub)),
            Instruction::Mul => Some(HotInstr::BinOp(BinOpKind::Mul)),
            Instruction::Div => Some(HotInstr::BinOp(BinOpKind::Div)),
            Instruction::Pow => Some(HotInstr::BinOp(BinOpKind::Pow)),
            Instruction::Eq => Some(HotInstr::BinOp(BinOpKind::Eq)),
            Instruction::Gt => Some(HotInstr::BinOp(BinOpKind::Gt)),
            Instruction::Lt => Some(HotInstr::BinOp(BinOpKind::Lt)),
            Instruction::AddCached(cache) =>
            {
                Some(HotInstr::BinOpCached(BinOpKind::Add, cache.clone()))
            }
            Instruction::SubCached(cache) =>
            {
                Some(HotInstr::BinOpCached(BinOpKind::Sub, cache.clone()))
            }
            Instruction::MulCached(cache) =>
            {
                Some(HotInstr::BinOpCached(BinOpKind::Mul, cache.clone()))
            }
            Instruction::DivCached(cache) =>
            {
                Some(HotInstr::BinOpCached(BinOpKind::Div, cache.clone()))
            }
            Instruction::PowCached(cache) =>
            {
                Some(HotInstr::BinOpCached(BinOpKind::Pow, cache.clone()))
            }
            Instruction::IndexCached(cache) => Some(HotInstr::IndexCached(cache.clone())),
            Instruction::F64IndexCached(cache) => Some(HotInstr::F64IndexCached(cache.clone())),
            Instruction::MapIndexCached(cache) => Some(HotInstr::MapIndexCached(cache.clone())),
            _ => None,
        };
        out.push(hot);
    }
    Rc::new(out)
}

fn execute_instructions(
    interpreter: &mut Interpreter,
    code: &Rc<Vec<Instruction>>,
    const_pool: &[Value],
    slots: &mut [Value],
) -> EvalResult
{
    #[derive(Clone)]
    enum ForEachIter
    {
        Array
        {
            arr: Rc<RefCell<Vec<Value>>>,
            idx: usize,
            len: usize,
        },
        F64Array
        {
            arr: Rc<RefCell<Vec<f64>>>,
            idx: usize,
            len: usize,
        },
        F32Array
        {
            arr: Rc<RefCell<Vec<f32>>>,
            idx: usize,
            len: usize,
        },
        I64Array
        {
            arr: Rc<RefCell<Vec<i64>>>,
            idx: usize,
            len: usize,
        },
        I32Array
        {
            arr: Rc<RefCell<Vec<i32>>>,
            idx: usize,
            len: usize,
        },
        Map
        {
            keys: Vec<Rc<String>>, idx: usize
        },
    }

    struct ForEachState
    {
        var_slot: usize,
        body: Rc<Vec<Instruction>>,
        iter: ForEachIter,
        last: Value,
    }

    struct ForRangeState
    {
        index_slot: usize,
        end: RangeEnd,
        end_cached: Option<f64>,
        fast_until: Option<f64>,
        body: Rc<Vec<Instruction>>,
        current: i64,
        last: Value,
    }

    struct ForRangeIntState
    {
        index_slot: usize,
        end: RangeEnd,
        end_cached: Option<RangeEndNum>,
        fast_until: Option<i64>,
        step: i64,
        body: Rc<Vec<Instruction>>,
        current: i64,
        last: Value,
    }

    struct ForRangeFloatState
    {
        index_slot: usize,
        end: RangeEnd,
        end_cached: Option<f64>,
        fast_until: Option<f64>,
        step: f64,
        kind: FloatKind,
        body: Rc<Vec<Instruction>>,
        current: f64,
        last: Value,
    }

    enum Pending
    {
        ForEach(ForEachState),
        ForRange(ForRangeState),
        ForRangeInt(ForRangeIntState),
        ForRangeFloat(ForRangeFloatState),
    }

    struct Frame
    {
        code: Rc<Vec<Instruction>>,
        hot_cache: Rc<Vec<Option<HotInstr>>>,
        ip: usize,
        stack: Vec<Value>,
        pending: Option<Pending>,
    }

    fn is_self_alias(existing: &Value, val: &Value) -> bool
    {
        match (existing, val)
        {
            (Value::Reference(old_ref), Value::Reference(new_ref)) => Rc::ptr_eq(old_ref, new_ref),
            _ => false,
        }
    }

    fn next_foreach_value(iter: &mut ForEachIter) -> Option<Value>
    {
        match iter
        {
            ForEachIter::Array { arr, idx, len } =>
            {
                if *idx >= *len
                {
                    return None;
                }
                let val = arr.borrow().get(*idx).cloned();
                *idx += 1;
                val
            }
            ForEachIter::F64Array { arr, idx, len } =>
            {
                if *idx >= *len
                {
                    return None;
                }
                let val = arr
                    .borrow()
                    .get(*idx)
                    .cloned()
                    .map(|v| make_float(v, FloatKind::F64));
                *idx += 1;
                val
            }
            ForEachIter::F32Array { arr, idx, len } =>
            {
                if *idx >= *len
                {
                    return None;
                }
                let val = arr
                    .borrow()
                    .get(*idx)
                    .cloned()
                    .map(|v| make_float(v as f64, FloatKind::F32));
                *idx += 1;
                val
            }
            ForEachIter::I64Array { arr, idx, len } =>
            {
                if *idx >= *len
                {
                    return None;
                }
                let val = arr
                    .borrow()
                    .get(*idx)
                    .cloned()
                    .map(|v| make_signed_int(v as i128, IntKind::I64));
                *idx += 1;
                val
            }
            ForEachIter::I32Array { arr, idx, len } =>
            {
                if *idx >= *len
                {
                    return None;
                }
                let val = arr
                    .borrow()
                    .get(*idx)
                    .cloned()
                    .map(|v| make_signed_int(v as i128, IntKind::I32));
                *idx += 1;
                val
            }
            ForEachIter::Map { keys, idx } =>
            {
                if *idx >= keys.len()
                {
                    return None;
                }
                let val = keys.get(*idx).cloned().map(Value::String);
                *idx += 1;
                val
            }
        }
    }

    let mut frames = Vec::new();
    frames.push(Frame {
        code: code.clone(),
        hot_cache: build_hot_cache(code),
        ip: 0,
        stack: interpreter.take_stack(),
        pending: None,
    });
    let mut last_result = Value::Nil;

    loop
    {
        if frames.is_empty()
        {
            return Ok(last_result);
        }

        let done = {
            let frame = frames.last().unwrap();
            frame.ip >= frame.code.len()
        };
        if done
        {
            let mut frame = frames.pop().unwrap();
            let result = frame.stack.pop().unwrap_or(Value::Nil);
            interpreter.recycle_stack(frame.stack);
            last_result = result.clone();
            if frames.is_empty()
            {
                return Ok(result);
            }

            let mut pending_next: Option<Pending> = None;
            let mut next_frame: Option<Frame> = None;
            {
                let parent = frames.last_mut().unwrap();
                if let Some(pending) = parent.pending.take()
                {
                    match pending
                    {
                        Pending::ForEach(mut state) =>
                        {
                            state.last = result;
                            if let Some(item) = next_foreach_value(&mut state.iter)
                            {
                                if let Some(slot) = slots.get_mut(state.var_slot)
                                {
                                    *slot = item;
                                }
                                let body = state.body.clone();
                                pending_next = Some(Pending::ForEach(state));
                                next_frame = Some(Frame {
                                    code: body.clone(),
                                    hot_cache: build_hot_cache(&body),
                                    ip: 0,
                                    stack: interpreter.take_stack(),
                                    pending: None,
                                });
                            }
                            else
                            {
                                parent.stack.push(state.last.clone());
                            }
                        }
                        Pending::ForRange(mut state) =>
                        {
                            state.last = result;
                            state.current += 1;
                            let should_stop = if let Some(limit) = state.fast_until
                            {
                                if (state.current as f64) < limit
                                {
                                    false
                                }
                                else
                                {
                                    state.fast_until = None;
                                    let end_f = state
                                        .end_cached
                                        .unwrap_or(range_end_f64(&state.end, slots, const_pool)?);
                                    (state.current as f64) >= end_f
                                }
                            }
                            else
                            {
                                let end_f = state
                                    .end_cached
                                    .unwrap_or(range_end_f64(&state.end, slots, const_pool)?);
                                (state.current as f64) >= end_f
                            };
                            if should_stop
                            {
                                if let Some(slot) = slots.get_mut(state.index_slot)
                                {
                                    *slot = default_int(state.current as i128);
                                }
                                parent.stack.push(state.last.clone());
                            }
                            else
                            {
                                if let Some(slot) = slots.get_mut(state.index_slot)
                                {
                                    *slot = default_int(state.current as i128);
                                }
                                let body = state.body.clone();
                                pending_next = Some(Pending::ForRange(state));
                                next_frame = Some(Frame {
                                    code: body.clone(),
                                    hot_cache: build_hot_cache(&body),
                                    ip: 0,
                                    stack: interpreter.take_stack(),
                                    pending: None,
                                });
                            }
                        }
                        Pending::ForRangeInt(mut state) =>
                        {
                            state.last = result;
                            state.current = state.current + state.step;
                            let should_stop = if let Some(limit) = state.fast_until
                            {
                                if state.current < limit
                                {
                                    false
                                }
                                else
                                {
                                    state.fast_until = None;
                                    match state.end_cached
                                    {
                                        Some(RangeEndNum::Float(end_f)) =>
                                        {
                                            (state.current as f64) >= end_f
                                        }
                                        Some(RangeEndNum::Int(end_i)) => state.current >= end_i,
                                        None => match range_end_num(&state.end, slots, const_pool)?
                                        {
                                            RangeEndNum::Float(end_f) =>
                                            {
                                                (state.current as f64) >= end_f
                                            }
                                            RangeEndNum::Int(end_i) => state.current >= end_i,
                                        },
                                    }
                                }
                            }
                            else
                            {
                                match state.end_cached
                                {
                                    Some(RangeEndNum::Float(end_f)) =>
                                    {
                                        (state.current as f64) >= end_f
                                    }
                                    Some(RangeEndNum::Int(end_i)) => state.current >= end_i,
                                    None => match range_end_num(&state.end, slots, const_pool)?
                                    {
                                        RangeEndNum::Float(end_f) =>
                                        {
                                            (state.current as f64) >= end_f
                                        }
                                        RangeEndNum::Int(end_i) => state.current >= end_i,
                                    },
                                }
                            };
                            if should_stop
                            {
                                if let Some(slot) = slots.get_mut(state.index_slot)
                                {
                                    *slot = default_int(state.current as i128);
                                }
                                parent.stack.push(state.last.clone());
                            }
                            else
                            {
                                if let Some(slot) = slots.get_mut(state.index_slot)
                                {
                                    *slot = default_int(state.current as i128);
                                }
                                let body = state.body.clone();
                                pending_next = Some(Pending::ForRangeInt(state));
                                next_frame = Some(Frame {
                                    code: body.clone(),
                                    hot_cache: build_hot_cache(&body),
                                    ip: 0,
                                    stack: interpreter.take_stack(),
                                    pending: None,
                                });
                            }
                        }
                        Pending::ForRangeFloat(mut state) =>
                        {
                            state.last = result;
                            state.current += state.step;
                            let should_stop = if let Some(limit) = state.fast_until
                            {
                                if state.current < limit
                                {
                                    false
                                }
                                else
                                {
                                    state.fast_until = None;
                                    let end_f = state
                                        .end_cached
                                        .unwrap_or(range_end_f64(&state.end, slots, const_pool)?);
                                    state.current >= end_f
                                }
                            }
                            else
                            {
                                let end_f = state
                                    .end_cached
                                    .unwrap_or(range_end_f64(&state.end, slots, const_pool)?);
                                state.current >= end_f
                            };
                            if should_stop
                            {
                                if let Some(slot) = slots.get_mut(state.index_slot)
                                {
                                    *slot = make_float(state.current, state.kind);
                                }
                                parent.stack.push(state.last.clone());
                            }
                            else
                            {
                                if let Some(slot) = slots.get_mut(state.index_slot)
                                {
                                    *slot = make_float(state.current, state.kind);
                                }
                                let body = state.body.clone();
                                pending_next = Some(Pending::ForRangeFloat(state));
                                next_frame = Some(Frame {
                                    code: body.clone(),
                                    hot_cache: build_hot_cache(&body),
                                    ip: 0,
                                    stack: interpreter.take_stack(),
                                    pending: None,
                                });
                            }
                        }
                    }
                }
                else
                {
                    parent.stack.push(result);
                }

                if let Some(pending) = pending_next
                {
                    parent.pending = Some(pending);
                }
            }
            if let Some(frame) = next_frame
            {
                frames.push(frame);
            }
            continue;
        }

        let mut next_frame: Option<Frame> = None;

        {
            let frame = frames.last_mut().unwrap();
            if let Some(hot) = frame.hot_cache[frame.ip].clone()
            {
                match hot
                {
                    HotInstr::LoadConstIdx(idx) =>
                    {
                        let val = const_pool.get(idx).cloned().unwrap_or(Value::Nil);
                        frame.stack.push(val);
                        frame.ip += 1;
                        continue;
                    }
                    HotInstr::LoadSlot(slot) =>
                    {
                        frame.stack.push(slots[slot].clone());
                        frame.ip += 1;
                        continue;
                    }
                    HotInstr::StoreSlot(slot) =>
                    {
                        let val = frame.stack.pop().unwrap();
                        if let Some(existing) = slots.get(slot)
                        {
                            if is_self_alias(existing, &val)
                            {
                                return Err(RuntimeError::simple("cannot self alias".to_string(), 0));
                            }
                        }
                        if let Some(dst) = slots.get_mut(slot)
                        {
                            *dst = val.clone();
                        }
                        frame.stack.push(val);
                        frame.ip += 1;
                        continue;
                    }
                    HotInstr::Pop =>
                    {
                        frame.stack.pop();
                        frame.ip += 1;
                        continue;
                    }
                    HotInstr::Dup =>
                    {
                        let val = frame.stack.last().cloned().ok_or_else(|| RuntimeError::simple("Stack underflow on dup".to_string(), 0))?;
                        frame.stack.push(val);
                        frame.ip += 1;
                        continue;
                    }
                    HotInstr::Not =>
                    {
                        let val = frame.stack.pop().unwrap_or(Value::Nil);
                        let is_truthy = !matches!(val, Value::Boolean(false) | Value::Nil);
                        frame.stack.push(Value::Boolean(!is_truthy));
                        frame.ip += 1;
                        continue;
                    }
                    HotInstr::CheckBool =>
                    {
                        let val = frame.stack.last().cloned().unwrap_or(Value::Nil);
                        if !matches!(val, Value::Boolean(_))
                        {
                            return Err(RuntimeError::simple("&& expects boolean operands".to_string(), 0));
                        }
                        frame.ip += 1;
                        continue;
                    }
                    HotInstr::JumpIfFalse(target) =>
                    {
                        let cond = frame.stack.pop().unwrap_or(Value::Nil);
                        let is_false = matches!(cond, Value::Boolean(false) | Value::Nil);
                        if is_false
                        {
                            frame.ip = target;
                        }
                        else
                        {
                            frame.ip += 1;
                        }
                        continue;
                    }
                    HotInstr::Jump(target) =>
                    {
                        frame.ip = target;
                        continue;
                    }
                    HotInstr::BinOp(op) =>
                    {
                        let r = frame.stack.pop().unwrap();
                        let l = frame.stack.pop().unwrap();
                        let res = eval_binop(op, l, r)?;
                        frame.stack.push(res);
                        frame.ip += 1;
                        continue;
                    }
                    HotInstr::BinOpCached(op, cache) =>
                    {
                        let r = frame.stack.pop().unwrap();
                        let l = frame.stack.pop().unwrap();
                        let res = eval_cached_binop(op, &cache, l, r)?;
                        frame.stack.push(res);
                        frame.ip += 1;
                        continue;
                    }
                    HotInstr::IndexCached(cache) =>
                    {
                        let index_val = frame.stack.pop().unwrap();
                        let target_val = frame.stack.pop().unwrap();
                        let res = eval_index_cached_value(index_val, target_val, &cache)?;
                        frame.stack.push(res);
                        frame.ip += 1;
                        continue;
                    }
                    HotInstr::F64IndexCached(cache) =>
                    {
                        let index_val = frame.stack.pop().unwrap();
                        let target_val = frame.stack.pop().unwrap();
                        let res = eval_f64_index_cached_value(index_val, target_val, &cache)?;
                        frame.stack.push(res);
                        frame.ip += 1;
                        continue;
                    }
                    HotInstr::MapIndexCached(cache) =>
                    {
                        let index_val = frame.stack.pop().unwrap();
                        let target_val = frame.stack.pop().unwrap();
                        let res = eval_map_index_cached_value(index_val, target_val, &cache)?;
                        frame.stack.push(res);
                        frame.ip += 1;
                        continue;
                    }
                }
            }

            let mut advance_ip = true;

            match &frame.code[frame.ip]
            {
                Instruction::LoadConstIdx(idx) =>
                {
                    let val = const_pool.get(*idx).cloned().unwrap_or(Value::Nil);
                    frame.stack.push(val);
                    frame.ip += 1;
                    continue;
                }
                Instruction::LoadSlot(s) =>
                {
                    frame.stack.push(slots[*s].clone());
                    frame.ip += 1;
                    continue;
                }
                Instruction::StoreSlot(s) =>
                {
                    let val = frame.stack.pop().unwrap();
                    if let Some(existing) = slots.get(*s)
                    {
                        if is_self_alias(existing, &val)
                        {
                            return Err(RuntimeError::simple("cannot self alias".to_string(), 0));
                        }
                    }
                    if let Some(slot) = slots.get_mut(*s)
                    {
                        *slot = val.clone();
                    }
                    frame.stack.push(val);
                    frame.ip += 1;
                    continue;
                }
                Instruction::LoadGlobalCached(name, cache) =>
                {
                    let val = load_global_cached(interpreter, *name, cache)?;
                    frame.stack.push(val);
                    frame.ip += 1;
                    continue;
                }
                Instruction::LoadGlobal(name) =>
                {
                    let val = interpreter
                        .env
                        .borrow()
                        .get(*name)
                        .ok_or_else(|| RuntimeError::simple(format!("Undefined variable: {}", symbol_name(*name).as_str()), 0))?;
                    if let Value::Uninitialized = val
                    {
                        return Err(RuntimeError::simple(format!(
                                "Variable '{}' used before assignment",
                                symbol_name(*name).as_str()
                            ), 0));
                    }
                    frame.stack.push(val);
                    frame.ip += 1;
                    continue;
                }
                Instruction::StoreGlobal(name) =>
                {
                    let val = frame.stack.pop().unwrap();
                    if let Value::Reference(new_ref) = &val
                    {
                        let env_ref = interpreter.env.borrow();
                        let idx = *name as usize;
                        if idx < env_ref.values.len()
                        {
                            if let Value::Reference(old_ref) = &env_ref.values[idx]
                            {
                                if Rc::ptr_eq(old_ref, new_ref)
                                {
                                    return Err(RuntimeError::simple("cannot self alias".to_string(), 0));
                                }
                            }
                        }
                    }
                    interpreter.env.borrow_mut().set(*name, val.clone());
                    frame.stack.push(val);
                    frame.ip += 1;
                    continue;
                }
                Instruction::Pop =>
                {
                    frame.stack.pop();
                    frame.ip += 1;
                    continue;
                }
                Instruction::Dup =>
                {
                    let val = frame.stack.last().cloned().ok_or_else(|| RuntimeError::simple("Stack underflow on dup".to_string(), 0))?;
                    frame.stack.push(val);
                    frame.ip += 1;
                    continue;
                }
                Instruction::Not =>
                {
                    let val = frame.stack.pop().unwrap_or(Value::Nil);
                    let is_truthy = !matches!(val, Value::Boolean(false) | Value::Nil);
                    frame.stack.push(Value::Boolean(!is_truthy));
                    frame.ip += 1;
                    continue;
                }
                Instruction::JumpIfFalse(target) =>
                {
                    let cond = frame.stack.pop().unwrap_or(Value::Nil);
                    let is_false = matches!(cond, Value::Boolean(false) | Value::Nil);
                    if is_false
                    {
                        frame.ip = *target;
                    }
                    else
                    {
                        frame.ip += 1;
                    }
                    continue;
                }
                Instruction::Jump(target) =>
                {
                    frame.ip = *target;
                    continue;
                }
                Instruction::Add
                | Instruction::Sub
                | Instruction::Mul
                | Instruction::Div
                | Instruction::Pow
                | Instruction::Eq
                | Instruction::Gt
                | Instruction::Lt =>
                {
                    let r = frame.stack.pop().unwrap();
                    let l = frame.stack.pop().unwrap();
                    let op = match frame.code[frame.ip]
                    {
                        Instruction::Add => BinOpKind::Add,
                        Instruction::Sub => BinOpKind::Sub,
                        Instruction::Mul => BinOpKind::Mul,
                        Instruction::Div => BinOpKind::Div,
                        Instruction::Pow => BinOpKind::Pow,
                        Instruction::Eq => BinOpKind::Eq,
                        Instruction::Gt => BinOpKind::Gt,
                        Instruction::Lt => BinOpKind::Lt,
                        _ => unreachable!(),
                    };
                    let res = eval_binop(op, l, r)?;
                    frame.stack.push(res);
                    frame.ip += 1;
                    continue;
                }
                Instruction::AddCached(cache)
                | Instruction::SubCached(cache)
                | Instruction::MulCached(cache)
                | Instruction::DivCached(cache)
                | Instruction::PowCached(cache) =>
                {
                    let r = frame.stack.pop().unwrap();
                    let l = frame.stack.pop().unwrap();
                    let op = match frame.code[frame.ip]
                    {
                        Instruction::AddCached(_) => BinOpKind::Add,
                        Instruction::SubCached(_) => BinOpKind::Sub,
                        Instruction::MulCached(_) => BinOpKind::Mul,
                        Instruction::DivCached(_) => BinOpKind::Div,
                        Instruction::PowCached(_) => BinOpKind::Pow,
                        _ => unreachable!(),
                    };
                    let res = eval_cached_binop(op, cache, l, r)?;
                    frame.stack.push(res);
                    frame.ip += 1;
                    continue;
                }
                _ =>
                {}
            }

            let inst = frame.code[frame.ip].clone();
            match inst
            {
                Instruction::LoadConstIdx(idx) =>
                {
                    let val = const_pool.get(idx).cloned().unwrap_or(Value::Nil);
                    frame.stack.push(val);
                }
                Instruction::LoadSlot(s) => frame.stack.push(slots[s].clone()),
                Instruction::StoreSlot(s) =>
                {
                    let val = frame.stack.pop().unwrap();
                    if let Some(existing) = slots.get(s)
                    {
                        if is_self_alias(existing, &val)
                        {
                            return Err(RuntimeError::simple("cannot self alias".to_string(), 0));
                        }
                    }
                    if let Some(slot) = slots.get_mut(s)
                    {
                        *slot = val.clone();
                    }
                    frame.stack.push(val);
                }
                Instruction::Not =>
                {
                    let val = frame.stack.pop().unwrap_or(Value::Nil);
                    let is_truthy = !matches!(val, Value::Boolean(false) | Value::Nil);
                    frame.stack.push(Value::Boolean(!is_truthy));
                }
                Instruction::CheckBool =>
                {
                    let val = frame.stack.last().cloned().unwrap_or(Value::Nil);
                    if !matches!(val, Value::Boolean(_))
                    {
                        return Err(RuntimeError::simple("&& expects boolean operands".to_string(), 0));
                    }
                }
                Instruction::LoadGlobalCached(name, cache) =>
                {
                    let val = load_global_cached(interpreter, name, &cache)?;
                    frame.stack.push(val);
                }
                Instruction::LoadGlobal(name) =>
                {
                    let val = interpreter
                        .env
                        .borrow()
                        .get(name)
                        .ok_or_else(|| RuntimeError::simple(format!("Undefined variable: {}", symbol_name(name).as_str()), 0))?;
                    if let Value::Uninitialized = val
                    {
                        return Err(RuntimeError::simple(format!(
                                "Variable '{}' used before assignment",
                                symbol_name(name).as_str()
                            ), 0));
                    }
                    frame.stack.push(val);
                }
                Instruction::StoreGlobal(name) =>
                {
                    let val = frame.stack.pop().unwrap();
                    if let Value::Reference(new_ref) = &val
                    {
                        let env_ref = interpreter.env.borrow();
                        let idx = name as usize;
                        if idx < env_ref.values.len()
                        {
                            if let Value::Reference(old_ref) = &env_ref.values[idx]
                            {
                                if Rc::ptr_eq(old_ref, new_ref)
                                {
                                    return Err(RuntimeError::simple("cannot self alias".to_string(), 0));
                                }
                            }
                        }
                    }
                    interpreter.env.borrow_mut().set(name, val.clone());
                    frame.stack.push(val);
                }
                Instruction::Pop =>
                {
                    frame.stack.pop();
                }
                Instruction::Dup =>
                {
                    let val = frame.stack.last().cloned().ok_or_else(|| RuntimeError::simple("Stack underflow on dup".to_string(), 0))?;
                    frame.stack.push(val);
                }
                Instruction::CloneValue =>
                {
                    let val = frame.stack.pop().unwrap_or(Value::Nil);
                    frame.stack.push(clone_value(&val));
                }
                Instruction::AddCached(cache) =>
                {
                    let r = frame.stack.pop().unwrap();
                    let l = frame.stack.pop().unwrap();
                    let res = eval_cached_binop(BinOpKind::Add, &cache, l, r)?;
                    frame.stack.push(res);
                }
                Instruction::SubCached(cache) =>
                {
                    let r = frame.stack.pop().unwrap();
                    let l = frame.stack.pop().unwrap();
                    let res = eval_cached_binop(BinOpKind::Sub, &cache, l, r)?;
                    frame.stack.push(res);
                }
                Instruction::MulCached(cache) =>
                {
                    let r = frame.stack.pop().unwrap();
                    let l = frame.stack.pop().unwrap();
                    let res = eval_cached_binop(BinOpKind::Mul, &cache, l, r)?;
                    frame.stack.push(res);
                }
                Instruction::DivCached(cache) =>
                {
                    let r = frame.stack.pop().unwrap();
                    let l = frame.stack.pop().unwrap();
                    let res = eval_cached_binop(BinOpKind::Div, &cache, l, r)?;
                    frame.stack.push(res);
                }
                Instruction::PowCached(cache) =>
                {
                    let r = frame.stack.pop().unwrap();
                    let l = frame.stack.pop().unwrap();
                    let res = eval_cached_binop(BinOpKind::Pow, &cache, l, r)?;
                    frame.stack.push(res);
                }
                Instruction::JumpIfFalse(target) =>
                {
                    let cond = frame.stack.pop().unwrap_or(Value::Nil);
                    let is_false = matches!(cond, Value::Boolean(false) | Value::Nil);
                    if is_false
                    {
                        frame.ip = target;
                        advance_ip = false;
                    }
                }
                Instruction::Jump(target) =>
                {
                    frame.ip = target;
                    advance_ip = false;
                }
                Instruction::CallBuiltin(builtin, argc) =>
                {
                    let args = pop_args_from_stack(&mut frame.stack, argc)?;
                    let result = interpreter.call_builtin(&builtin, &args)?;
                    frame.stack.push(result);
                }
                Instruction::Len =>
                {
                    let val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing argument for len".to_string(), 0))?;
                    let result = match val
                    {
                        Value::String(s) => default_int(s.len() as i128),
                        Value::Array(arr) => default_int(arr.borrow().len() as i128),
                        Value::F32Array(arr) => default_int(arr.borrow().len() as i128),
                        Value::F64Array(arr) => default_int(arr.borrow().len() as i128),
                        Value::I32Array(arr) => default_int(arr.borrow().len() as i128),
                        Value::I64Array(arr) => default_int(arr.borrow().len() as i128),
                        Value::Bytes(bytes) => default_int(bytes.len() as i128),
                        Value::ByteBuf(buf) => default_int(buf.borrow().len() as i128),
                        Value::BytesView(view) => default_int(view.len as i128),
                        Value::Map(map) => default_int(map.borrow().data.len() as i128),
                        Value::Env(env) => default_int(env.data.len() as i128),
                        Value::Mmap(mmap) => default_int(mmap.len() as i128),
                        Value::MmapMut(mmap) => default_int(mmap.borrow().len() as i128),
                        _ => default_int(0),
                    };
                    frame.stack.push(result);
                }
                Instruction::CallValue(argc) =>
                {
                    let args = pop_args_from_stack(&mut frame.stack, argc)?;
                    let func_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing function value for call".to_string(), 0))?;
                    let result = interpreter.call_value(func_val, args, 0, None)?;
                    frame.stack.push(result);
                }
                Instruction::MapKeys =>
                {
                    let target_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing target for map keys".to_string(), 0))?;
                    let result = match target_val
                    {
                        Value::Map(map) => map_keys_array(&map.borrow()),
                        Value::Env(env) => env_keys_array(env.as_ref()),
                        _ => Value::Nil,
                    };
                    frame.stack.push(result);
                }
                Instruction::MapValues =>
                {
                    let target_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing target for map values".to_string(), 0))?;
                    let result = match target_val
                    {
                        Value::Map(map) => map_values_array(&map.borrow()),
                        Value::Env(env) => env_values_array(env.as_ref()),
                        _ => Value::Nil,
                    };
                    frame.stack.push(result);
                }
                Instruction::CallValueCached(cache, argc) =>
                {
                    let args = pop_args_from_stack(&mut frame.stack, argc)?;
                    let func_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing function value for call".to_string(), 0))?;
                    let result = eval_call_value_cached(interpreter, func_val, args, &cache)?;
                    frame.stack.push(result);
                }
                Instruction::CallValueCached0(cache) =>
                {
                    let func_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing function value for call".to_string(), 0))?;
                    let args = smallvec::SmallVec::<[Value; 8]>::new();
                    let result = eval_call_value_cached(interpreter, func_val, args, &cache)?;
                    frame.stack.push(result);
                }
                Instruction::CallValueCached1(cache) =>
                {
                    let arg = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing argument for call".to_string(), 0))?;
                    let func_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing function value for call".to_string(), 0))?;
                    let mut args = smallvec::SmallVec::<[Value; 8]>::new();
                    args.push(arg);
                    let result = eval_call_value_cached(interpreter, func_val, args, &cache)?;
                    frame.stack.push(result);
                }
                Instruction::CallValueWithBlock(block, argc) =>
                {
                    let args = pop_args_from_stack(&mut frame.stack, argc)?;
                    let func_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing function value for call".to_string(), 0))?;
                    let result = interpreter.call_value(func_val, args, 0, Some(block.clone()))?;
                    frame.stack.push(result);
                }
                Instruction::CallValueWithBlockCached(cache, block, argc) =>
                {
                    let args = pop_args_from_stack(&mut frame.stack, argc)?;
                    let func_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing function value for call".to_string(), 0))?;
                    let result = eval_call_value_cached_with_block(
                        interpreter,
                        func_val,
                        args,
                        &cache,
                        &block,
                    )?;
                    frame.stack.push(result);
                }
                Instruction::CallValueWithBlockCached0(cache, block) =>
                {
                    let func_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing function value for call".to_string(), 0))?;
                    let args = smallvec::SmallVec::<[Value; 8]>::new();
                    let result = eval_call_value_cached_with_block(
                        interpreter,
                        func_val,
                        args,
                        &cache,
                        &block,
                    )?;
                    frame.stack.push(result);
                }
                Instruction::CallValueWithBlockCached1(cache, block) =>
                {
                    let arg = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing argument for call".to_string(), 0))?;
                    let func_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing function value for call".to_string(), 0))?;
                    let mut args = smallvec::SmallVec::<[Value; 8]>::new();
                    args.push(arg);
                    let result = eval_call_value_cached_with_block(
                        interpreter,
                        func_val,
                        args,
                        &cache,
                        &block,
                    )?;
                    frame.stack.push(result);
                }
                Instruction::CallGlobalCached(name, global_cache, call_cache, argc) =>
                {
                    let args = pop_args_from_stack(&mut frame.stack, argc)?;
                    let func_val = load_global_cached(interpreter, name, &global_cache)?;
                    let result = eval_call_value_cached(interpreter, func_val, args, &call_cache)?;
                    frame.stack.push(result);
                }
                Instruction::CallGlobalCached0(name, global_cache, call_cache) =>
                {
                    let func_val = load_global_cached(interpreter, name, &global_cache)?;
                    let args = smallvec::SmallVec::<[Value; 8]>::new();
                    let result = eval_call_value_cached(interpreter, func_val, args, &call_cache)?;
                    frame.stack.push(result);
                }
                Instruction::CallGlobalCached1(name, global_cache, call_cache) =>
                {
                    let arg = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing argument for call".to_string(), 0))?;
                    let func_val = load_global_cached(interpreter, name, &global_cache)?;
                    let mut args = smallvec::SmallVec::<[Value; 8]>::new();
                    args.push(arg);
                    let result = eval_call_value_cached(interpreter, func_val, args, &call_cache)?;
                    frame.stack.push(result);
                }
                Instruction::CallMethodCached(name, map_cache, call_cache, argc) =>
                {
                    let args = pop_args_from_stack(&mut frame.stack, argc)?;
                    let func_val =
                        pop_method_target_and_resolve(&mut frame.stack, &name, &map_cache)?;
                    let result = eval_call_value_cached(interpreter, func_val, args, &call_cache)?;
                    frame.stack.push(result);
                }
                Instruction::CallMethodCached0(name, map_cache, call_cache) =>
                {
                    let func_val =
                        pop_method_target_and_resolve(&mut frame.stack, &name, &map_cache)?;
                    let args = smallvec::SmallVec::<[Value; 8]>::new();
                    let result = eval_call_value_cached(interpreter, func_val, args, &call_cache)?;
                    frame.stack.push(result);
                }
                Instruction::CallMethodCached1(name, map_cache, call_cache) =>
                {
                    let arg = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing argument for call".to_string(), 0))?;
                    let func_val =
                        pop_method_target_and_resolve(&mut frame.stack, &name, &map_cache)?;
                    let mut args = smallvec::SmallVec::<[Value; 8]>::new();
                    args.push(arg);
                    let result = eval_call_value_cached(interpreter, func_val, args, &call_cache)?;
                    frame.stack.push(result);
                }
                Instruction::CallMethodWithBlockCached(
                    name,
                    map_cache,
                    call_cache,
                    block,
                    argc,
                ) =>
                {
                    let args = pop_args_from_stack(&mut frame.stack, argc)?;
                    let func_val =
                        pop_method_target_and_resolve(&mut frame.stack, &name, &map_cache)?;
                    let result = eval_call_value_cached_with_block(
                        interpreter,
                        func_val,
                        args,
                        &call_cache,
                        &block,
                    )?;
                    frame.stack.push(result);
                }
                Instruction::CallMethodWithBlockCached0(name, map_cache, call_cache, block) =>
                {
                    let func_val =
                        pop_method_target_and_resolve(&mut frame.stack, &name, &map_cache)?;
                    let args = smallvec::SmallVec::<[Value; 8]>::new();
                    let result = eval_call_value_cached_with_block(
                        interpreter,
                        func_val,
                        args,
                        &call_cache,
                        &block,
                    )?;
                    frame.stack.push(result);
                }
                Instruction::CallMethodWithBlockCached1(name, map_cache, call_cache, block) =>
                {
                    let arg = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing argument for call".to_string(), 0))?;
                    let func_val =
                        pop_method_target_and_resolve(&mut frame.stack, &name, &map_cache)?;
                    let mut args = smallvec::SmallVec::<[Value; 8]>::new();
                    args.push(arg);
                    let result = eval_call_value_cached_with_block(
                        interpreter,
                        func_val,
                        args,
                        &call_cache,
                        &block,
                    )?;
                    frame.stack.push(result);
                }
                Instruction::ForEach { var_slot, body } =>
                {
                    let iter_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing iterable for for-loop".to_string(), 0))?;
                    let iter = match iter_val
                    {
                        Value::Array(arr) =>
                        {
                            let len = arr.borrow().len();
                            ForEachIter::Array { arr, idx: 0, len }
                        }
                        Value::F32Array(arr) =>
                        {
                            let len = arr.borrow().len();
                            ForEachIter::F32Array { arr, idx: 0, len }
                        }
                        Value::F64Array(arr) =>
                        {
                            let len = arr.borrow().len();
                            ForEachIter::F64Array { arr, idx: 0, len }
                        }
                        Value::I32Array(arr) =>
                        {
                            let len = arr.borrow().len();
                            ForEachIter::I32Array { arr, idx: 0, len }
                        }
                        Value::I64Array(arr) =>
                        {
                            let len = arr.borrow().len();
                            ForEachIter::I64Array { arr, idx: 0, len }
                        }
                        Value::Map(map) =>
                        {
                            let map_ref = map.borrow();
                            let mut keys = Vec::with_capacity(map_ref.data.len());
                            keys.extend(map_ref.data.keys().cloned());
                            ForEachIter::Map { keys, idx: 0 }
                        }
                        Value::Env(env) =>
                        {
                            let mut keys = Vec::with_capacity(env.data.len());
                            keys.extend(env.data.keys().cloned());
                            ForEachIter::Map { keys, idx: 0 }
                        }
                        _ =>
                        {
                            return Err(RuntimeError::simple("Type is not iterable".to_string(), 0));
                        }
                    };
                    let mut state = ForEachState {
                        var_slot,
                        body: body.clone(),
                        iter,
                        last: Value::Nil,
                    };
                    if let Some(item) = next_foreach_value(&mut state.iter)
                    {
                        if let Some(slot) = slots.get_mut(state.var_slot)
                        {
                            *slot = item;
                        }
                        frame.pending = Some(Pending::ForEach(state));
                        next_frame = Some(Frame {
                            code: body.clone(),
                            hot_cache: build_hot_cache(&body),
                            ip: 0,
                            stack: interpreter.take_stack(),
                            pending: None,
                        });
                    }
                    else
                    {
                        frame.stack.push(state.last);
                    }
                }
                Instruction::ForEachArray { var_slot, body } =>
                {
                    let iter_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing iterable for for-loop".to_string(), 0))?;
                    let arr = match iter_val
                    {
                        Value::Array(arr) => arr,
                        _ =>
                        {
                            return Err(RuntimeError::simple("Type is not iterable".to_string(), 0));
                        }
                    };
                    let len = arr.borrow().len();
                    let mut state = ForEachState {
                        var_slot,
                        body: body.clone(),
                        iter: ForEachIter::Array { arr, idx: 0, len },
                        last: Value::Nil,
                    };
                    if let Some(item) = next_foreach_value(&mut state.iter)
                    {
                        if let Some(slot) = slots.get_mut(state.var_slot)
                        {
                            *slot = item;
                        }
                        frame.pending = Some(Pending::ForEach(state));
                        next_frame = Some(Frame {
                            code: body.clone(),
                            hot_cache: build_hot_cache(&body),
                            ip: 0,
                            stack: interpreter.take_stack(),
                            pending: None,
                        });
                    }
                    else
                    {
                        frame.stack.push(state.last);
                    }
                }
                Instruction::ForEachF64Array { var_slot, body } =>
                {
                    let iter_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing iterable for for-loop".to_string(), 0))?;
                    let arr = match iter_val
                    {
                        Value::F64Array(arr) => arr,
                        _ =>
                        {
                            return Err(RuntimeError::simple("Type is not iterable".to_string(), 0));
                        }
                    };
                    let len = arr.borrow().len();
                    let mut state = ForEachState {
                        var_slot,
                        body: body.clone(),
                        iter: ForEachIter::F64Array { arr, idx: 0, len },
                        last: Value::Nil,
                    };
                    if let Some(item) = next_foreach_value(&mut state.iter)
                    {
                        if let Some(slot) = slots.get_mut(state.var_slot)
                        {
                            *slot = item;
                        }
                        frame.pending = Some(Pending::ForEach(state));
                        next_frame = Some(Frame {
                            code: body.clone(),
                            hot_cache: build_hot_cache(&body),
                            ip: 0,
                            stack: interpreter.take_stack(),
                            pending: None,
                        });
                    }
                    else
                    {
                        frame.stack.push(state.last);
                    }
                }
                Instruction::ForRange {
                    index_slot,
                    end,
                    body,
                } =>
                {
                    let current = match slots.get(index_slot)
                    {
                        Some(v) => int_value_as_i64(v).ok_or_else(|| RuntimeError::simple("Range index must be a number".to_string(), 0))?,
                        None =>
                        {
                            return Err(RuntimeError::simple("Range index must be a number".to_string(), 0));
                        }
                    };
                    let end_cached = match end
                    {
                        RangeEnd::Const(_) => Some(range_end_f64(&end, slots, const_pool)?),
                        _ => None,
                    };
                    let end_f = end_cached.unwrap_or(range_end_f64(&end, slots, const_pool)?);
                    let fast_until = end_cached.map(|end| end - 3.0);
                    if (current as f64) >= end_f
                    {
                        if let Some(slot) = slots.get_mut(index_slot)
                        {
                            *slot = default_int(current as i128);
                        }
                        frame.stack.push(Value::Nil);
                    }
                    else
                    {
                        if let Some(slot) = slots.get_mut(index_slot)
                        {
                            *slot = default_int(current as i128);
                        }
                        frame.pending = Some(Pending::ForRange(ForRangeState {
                            index_slot,
                            end,
                            end_cached,
                            fast_until,
                            body: body.clone(),
                            current,
                            last: Value::Nil,
                        }));
                        next_frame = Some(Frame {
                            code: body.clone(),
                            hot_cache: build_hot_cache(&body),
                            ip: 0,
                            stack: interpreter.take_stack(),
                            pending: None,
                        });
                    }
                }
                Instruction::ForRangeInt {
                    index_slot,
                    end,
                    step,
                    body,
                } =>
                {
                    let current_val =
                        slots.get(index_slot).cloned().ok_or_else(|| RuntimeError::simple("Range index must be a number".to_string(), 0))?;
                    if let Value::Float {
                        value: current,
                        kind,
                    } = current_val
                    {
                        let step_f = step as f64;
                        let end_cached = match end
                        {
                            RangeEnd::Const(_) => Some(range_end_f64(&end, slots, const_pool)?),
                            _ => None,
                        };
                        let end_f = end_cached.unwrap_or(range_end_f64(&end, slots, const_pool)?);
                        let fast_until = end_cached.map(|end| end - (step_f * 3.0));
                        if current >= end_f
                        {
                            if let Some(slot) = slots.get_mut(index_slot)
                            {
                                *slot = make_float(current, kind);
                            }
                            frame.stack.push(Value::Nil);
                        }
                        else
                        {
                            if let Some(slot) = slots.get_mut(index_slot)
                            {
                                *slot = make_float(current, kind);
                            }
                            frame.pending = Some(Pending::ForRangeFloat(ForRangeFloatState {
                                index_slot,
                                end,
                                end_cached,
                                fast_until,
                                step: step_f,
                                kind,
                                body: body.clone(),
                                current,
                                last: Value::Nil,
                            }));
                            next_frame = Some(Frame {
                                code: body.clone(),
                                hot_cache: build_hot_cache(&body),
                                ip: 0,
                                stack: interpreter.take_stack(),
                                pending: None,
                            });
                        }
                    }
                    else
                    {
                        let current =
                            int_value_as_i64(&current_val).ok_or_else(|| RuntimeError::simple("Range index must be a number".to_string(), 0))?;
                        let end_cached = match end
                        {
                            RangeEnd::Const(_) => Some(range_end_num(&end, slots, const_pool)?),
                            _ => None,
                        };
                        let fast_until = match (&end_cached, step)
                        {
                            (Some(RangeEndNum::Int(end_i)), 1) => Some(end_i.saturating_sub(3)),
                            _ => None,
                        };
                        let should_stop = match end_cached
                        {
                            Some(RangeEndNum::Float(end_f)) => (current as f64) >= end_f,
                            Some(RangeEndNum::Int(end_i)) => current >= end_i,
                            None => match range_end_num(&end, slots, const_pool)?
                            {
                                RangeEndNum::Float(end_f) => (current as f64) >= end_f,
                                RangeEndNum::Int(end_i) => current >= end_i,
                            },
                        };
                        if should_stop
                        {
                            if let Some(slot) = slots.get_mut(index_slot)
                            {
                                *slot = default_int(current as i128);
                            }
                            frame.stack.push(Value::Nil);
                        }
                        else
                        {
                            if let Some(slot) = slots.get_mut(index_slot)
                            {
                                *slot = default_int(current as i128);
                            }
                            frame.pending = Some(Pending::ForRangeInt(ForRangeIntState {
                                index_slot,
                                end,
                                end_cached,
                                fast_until,
                                step,
                                body: body.clone(),
                                current,
                                last: Value::Nil,
                            }));
                            next_frame = Some(Frame {
                                code: body.clone(),
                                hot_cache: build_hot_cache(&body),
                                ip: 0,
                                stack: interpreter.take_stack(),
                                pending: None,
                            });
                        }
                    }
                }
                Instruction::ForRangeFloat {
                    index_slot,
                    end,
                    step,
                    kind,
                    body,
                } =>
                {
                    let current_val =
                        slots.get(index_slot).cloned().ok_or_else(|| RuntimeError::simple("Range index must be a number".to_string(), 0))?;
                    let (current, kind) = match current_val
                    {
                        Value::Float {
                            value,
                            kind: current_kind,
                        } => (value, promote_float_kind(current_kind, kind)),
                        _ =>
                        {
                            let current =
                                int_value_as_f64(&current_val).ok_or_else(|| RuntimeError::simple("Range index must be a number".to_string(), 0))?;
                            (current, kind)
                        }
                    };
                    let step_f = normalize_float_value(step, kind);
                    let end_cached = match end
                    {
                        RangeEnd::Const(_) => Some(range_end_f64(&end, slots, const_pool)?),
                        _ => None,
                    };
                    let end_f = end_cached.unwrap_or(range_end_f64(&end, slots, const_pool)?);
                    let fast_until = end_cached.map(|end| end - (step_f * 3.0));
                    if current >= end_f
                    {
                        if let Some(slot) = slots.get_mut(index_slot)
                        {
                            *slot = make_float(current, kind);
                        }
                        frame.stack.push(Value::Nil);
                    }
                    else
                    {
                        if let Some(slot) = slots.get_mut(index_slot)
                        {
                            *slot = make_float(current, kind);
                        }
                        frame.pending = Some(Pending::ForRangeFloat(ForRangeFloatState {
                            index_slot,
                            end,
                            end_cached,
                            fast_until,
                            step: step_f,
                            kind,
                            body: body.clone(),
                            current,
                            last: Value::Nil,
                        }));
                        next_frame = Some(Frame {
                            code: body.clone(),
                            hot_cache: build_hot_cache(&body),
                            ip: 0,
                            stack: interpreter.take_stack(),
                            pending: None,
                        });
                    }
                }
                Instruction::MakeArray(count) =>
                {
                    if count > frame.stack.len()
                    {
                        return Err(RuntimeError::simple("Invalid array length".to_string(), 0));
                    }
                    let mut elems = Vec::with_capacity(count);
                    for _ in 0..count
                    {
                        elems.push(frame.stack.pop().unwrap());
                    }
                    elems.reverse();
                    if elems.is_empty()
                    {
                        frame
                            .stack
                            .push(Value::Array(Rc::new(RefCell::new(Vec::new()))));
                        continue;
                    }
                    let mut i32_vals: Vec<i32> = Vec::new();
                    let mut i64_vals: Vec<i64> = Vec::new();
                    let mut f32_vals: Vec<f32> = Vec::new();
                    let mut f64_vals: Vec<f64> = Vec::new();
                    let mut all_i32 = true;
                    let mut all_i64 = true;
                    let mut all_f32 = true;
                    let mut all_f64 = true;
                    for v in &elems
                    {
                        if all_i32
                        {
                            match v
                            {
                                Value::Integer {
                                    value,
                                    kind: IntKind::I32,
                                } => i32_vals.push(*value as i32),
                                Value::Unsigned {
                                    value,
                                    kind: IntKind::U32,
                                } => i32_vals.push(*value as i32),
                                _ => all_i32 = false,
                            }
                        }
                        if all_f32
                        {
                            match v
                            {
                                Value::Float {
                                    value,
                                    kind: FloatKind::F32,
                                } => f32_vals.push(*value as f32),
                                _ => all_f32 = false,
                            }
                        }
                        if all_i64
                        {
                            match v
                            {
                                Value::Integer { value, .. } =>
                                {
                                    i64_vals.push(*value as i64);
                                    if all_f64
                                    {
                                        f64_vals.push(*value as f64);
                                    }
                                    continue;
                                }
                                Value::Unsigned { value, .. } =>
                                {
                                    i64_vals.push(*value as i64);
                                    if all_f64
                                    {
                                        f64_vals.push(*value as f64);
                                    }
                                    continue;
                                }
                                _ =>
                                {
                                    all_i64 = false;
                                }
                            }
                        }
                        if all_f64
                        {
                            match v
                            {
                                Value::Float { value, .. } =>
                                {
                                    f64_vals.push(*value);
                                    continue;
                                }
                                _ =>
                                {
                                    if let Some(num) = int_value_as_f64(v)
                                    {
                                        f64_vals.push(num);
                                        continue;
                                    }
                                    all_f64 = false;
                                }
                            }
                        }
                    }
                    if all_i32
                    {
                        frame
                            .stack
                            .push(Value::I32Array(Rc::new(RefCell::new(i32_vals))));
                    }
                    else if all_i64
                    {
                        frame
                            .stack
                            .push(Value::I64Array(Rc::new(RefCell::new(i64_vals))));
                    }
                    else if all_f32
                    {
                        frame
                            .stack
                            .push(Value::F32Array(Rc::new(RefCell::new(f32_vals))));
                    }
                    else if all_f64
                    {
                        frame
                            .stack
                            .push(Value::F64Array(Rc::new(RefCell::new(f64_vals))));
                    }
                    else
                    {
                        frame.stack.push(Value::Array(Rc::new(RefCell::new(elems))));
                    }
                }
                Instruction::MakeMap(count) =>
                {
                    let pair_count = count.saturating_mul(2);
                    if pair_count > frame.stack.len()
                    {
                        return Err(RuntimeError::simple("Invalid map length".to_string(), 0));
                    }
                    let mut entries = Vec::with_capacity(count);
                    for _ in 0..count
                    {
                        let val = frame.stack.pop().unwrap();
                        let key_val = frame.stack.pop().unwrap();
                        entries.push((key_val, val));
                    }
                    entries.reverse();
                    let mut map = FxHashMap::default();
                    for (k_val, v_val) in entries
                    {
                        let key = match k_val
                        {
                            Value::String(s) => s,
                            _ => intern::intern_owned(k_val.inspect()),
                        };
                        map.insert(key, v_val);
                    }
                    frame
                        .stack
                        .push(Value::Map(Rc::new(RefCell::new(MapValue::new(map)))));
                }
                Instruction::Index =>
                {
                    let index_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing index for index expression".to_string(), 0))?;
                    let target_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing target for index expression".to_string(), 0))?;
                    let result = match target_val
                    {
                        Value::Array(arr) =>
                        {
                            if let Some(i) = int_value_as_usize(&index_val)
                            {
                                let vec = arr.borrow();
                                if i < vec.len()
                                {
                                    vec[i].clone()
                                }
                                else
                                {
                                    Value::Nil
                                }
                            }
                            else
                            {
                                return Err(err_index_requires_int());
                            }
                        }
                        Value::F64Array(arr) =>
                        {
                            if let Some(i) = int_value_as_usize(&index_val)
                            {
                                let vec = arr.borrow();
                                if i < vec.len()
                                {
                                    make_float(vec[i], FloatKind::F64)
                                }
                                else
                                {
                                    Value::Nil
                                }
                            }
                            else
                            {
                                return Err(err_index_requires_int());
                            }
                        }
                        Value::F32Array(arr) =>
                        {
                            if let Some(i) = int_value_as_usize(&index_val)
                            {
                                let vec = arr.borrow();
                                if i < vec.len()
                                {
                                    make_float(vec[i] as f64, FloatKind::F32)
                                }
                                else
                                {
                                    Value::Nil
                                }
                            }
                            else
                            {
                                return Err(err_index_requires_int());
                            }
                        }
                        Value::I64Array(arr) =>
                        {
                            if let Some(i) = int_value_as_usize(&index_val)
                            {
                                let vec = arr.borrow();
                                if i < vec.len()
                                {
                                    make_signed_int(vec[i] as i128, IntKind::I64)
                                }
                                else
                                {
                                    Value::Nil
                                }
                            }
                            else
                            {
                                return Err(err_index_requires_int());
                            }
                        }
                        Value::I32Array(arr) =>
                        {
                            if let Some(i) = int_value_as_usize(&index_val)
                            {
                                let vec = arr.borrow();
                                if i < vec.len()
                                {
                                    make_signed_int(vec[i] as i128, IntKind::I32)
                                }
                                else
                                {
                                    Value::Nil
                                }
                            }
                            else
                            {
                                return Err(err_index_requires_int());
                            }
                        }
                        Value::Bytes(bytes) =>
                        {
                            if let Some(i) = int_value_as_usize(&index_val)
                            {
                                if i < bytes.len()
                                {
                                    default_int(bytes[i] as i128)
                                }
                                else
                                {
                                    Value::Nil
                                }
                            }
                            else
                            {
                                return Err(err_index_requires_int());
                            }
                        }
                        Value::ByteBuf(buf) =>
                        {
                            if let Some(i) = int_value_as_usize(&index_val)
                            {
                                let bytes = buf.borrow();
                                if i < bytes.len()
                                {
                                    default_int(bytes[i] as i128)
                                }
                                else
                                {
                                    Value::Nil
                                }
                            }
                            else
                            {
                                return Err(err_index_requires_int());
                            }
                        }
                        Value::BytesView(view) =>
                        {
                            if let Some(i) = int_value_as_usize(&index_val)
                            {
                                if i < view.len
                                {
                                    let idx = view.offset + i;
                                    let byte = match &view.source
                                    {
                                        crate::value::BytesViewSource::Mmap(mmap) => mmap[idx],
                                        crate::value::BytesViewSource::MmapMut(mmap) =>
                                        {
                                            let data = mmap.borrow();
                                            data[idx]
                                        }
                                    };
                                    default_int(byte as i128)
                                }
                                else
                                {
                                    Value::Nil
                                }
                            }
                            else
                            {
                                return Err(err_index_requires_int());
                            }
                        }
                        Value::Mmap(mmap) =>
                        {
                            if let Some(i) = int_value_as_usize(&index_val)
                            {
                                if i < mmap.len()
                                {
                                    default_int(mmap[i] as i128)
                                }
                                else
                                {
                                    Value::Nil
                                }
                            }
                            else
                            {
                                return Err(err_index_requires_int());
                            }
                        }
                        Value::MmapMut(mmap) =>
                        {
                            if let Some(i) = int_value_as_usize(&index_val)
                            {
                                let bytes = mmap.borrow();
                                if i < bytes.len()
                                {
                                    default_int(bytes[i] as i128)
                                }
                                else
                                {
                                    Value::Nil
                                }
                            }
                            else
                            {
                                return Err(err_index_requires_int());
                            }
                        }
                        Value::StructInstance(inst) =>
                        {
                            if let Value::String(s) = index_val
                            {
                                if let Some(idx) = inst.ty.field_map.get(&s)
                                {
                                    let fields = inst.fields.borrow();
                                    fields.get(*idx).cloned().unwrap_or(Value::Nil)
                                }
                                else if let Some(method) =
                                    inst.ty.methods.borrow().get(&s).cloned()
                                {
                                    Value::BoundMethod(Rc::new(BoundMethod {
                                        receiver: Value::StructInstance(inst.clone()),
                                        func: method,
                                    }))
                                }
                                else
                                {
                                    Value::Nil
                                }
                            }
                            else
                            {
                                return Err(err_index_unsupported());
                            }
                        }
                        Value::StructType(ty) =>
                        {
                            if let Value::String(s) = index_val
                            {
                                ty.methods.borrow().get(&s).cloned().unwrap_or(Value::Nil)
                            }
                            else
                            {
                                return Err(err_index_unsupported());
                            }
                        }
                        Value::Map(map) =>
                        {
                            let map_ref = map.borrow();
                            if let Value::String(s) = index_val
                            {
                                if s.as_str() == "keys"
                                {
                                    map_keys_array(&map_ref)
                                }
                                else if s.as_str() == "values"
                                {
                                    map_values_array(&map_ref)
                                }
                                else
                                {
                                    map_ref.data.get(&s).cloned().unwrap_or(Value::Nil)
                                }
                            }
                            else
                            {
                                let key = intern::intern_owned(index_val.inspect());
                                map_ref.data.get(&key).cloned().unwrap_or(Value::Nil)
                            }
                        }
                        Value::Env(env) =>
                        {
                            if let Value::String(s) = index_val
                            {
                                if s.as_str() == "keys"
                                {
                                    env_keys_array(env.as_ref())
                                }
                                else if s.as_str() == "values"
                                {
                                    env_values_array(env.as_ref())
                                }
                                else
                                {
                                    env.data.get(&s).map(env_clone_value).unwrap_or(Value::Nil)
                                }
                            }
                            else
                            {
                                let key = intern::intern_owned(index_val.inspect());
                                env.data
                                    .get(&key)
                                    .map(env_clone_value)
                                    .unwrap_or(Value::Nil)
                            }
                        }
                        _ => return Err(err_index_unsupported()),
                    };
                    frame.stack.push(result);
                }
                Instruction::Slice =>
                {
                    let end_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing end index for slice".to_string(), 0))?;
                    let start_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing start index for slice".to_string(), 0))?;
                    let target_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing target for slice".to_string(), 0))?;

                    let start_idx = int_value_as_usize(&start_val).ok_or_else(|| RuntimeError::simple("Slice start index must be an integer".to_string(), 0))?;
                    let end_idx = int_value_as_usize(&end_val).ok_or_else(|| RuntimeError::simple("Slice end index must be an integer".to_string(), 0))?;

                    let result = match target_val
                    {
                        Value::String(s) =>
                        {
                            let chars: Vec<char> = s.chars().collect();
                            let len = chars.len();
                            let start_clamped = start_idx.min(len);
                            let end_clamped = end_idx.min(len);
                            if start_clamped >= end_clamped
                            {
                                Value::String(intern::intern(""))
                            }
                            else
                            {
                                let slice: String =
                                    chars[start_clamped..end_clamped].iter().collect();
                                Value::String(intern::intern_owned(slice))
                            }
                        }
                        Value::Array(arr) =>
                        {
                            let vec = arr.borrow();
                            let len = vec.len();
                            let start_clamped = start_idx.min(len);
                            let end_clamped = end_idx.min(len);
                            if start_clamped >= end_clamped
                            {
                                Value::Array(Rc::new(RefCell::new(Vec::new())))
                            }
                            else
                            {
                                let slice: Vec<Value> = vec[start_clamped..end_clamped].to_vec();
                                Value::Array(Rc::new(RefCell::new(slice)))
                            }
                        }
                        Value::F64Array(arr) =>
                        {
                            let vec = arr.borrow();
                            let len = vec.len();
                            let start_clamped = start_idx.min(len);
                            let end_clamped = end_idx.min(len);
                            if start_clamped >= end_clamped
                            {
                                Value::F64Array(Rc::new(RefCell::new(Vec::new())))
                            }
                            else
                            {
                                let slice: Vec<f64> = vec[start_clamped..end_clamped].to_vec();
                                Value::F64Array(Rc::new(RefCell::new(slice)))
                            }
                        }
                        Value::F32Array(arr) =>
                        {
                            let vec = arr.borrow();
                            let len = vec.len();
                            let start_clamped = start_idx.min(len);
                            let end_clamped = end_idx.min(len);
                            if start_clamped >= end_clamped
                            {
                                Value::F32Array(Rc::new(RefCell::new(Vec::new())))
                            }
                            else
                            {
                                let slice: Vec<f32> = vec[start_clamped..end_clamped].to_vec();
                                Value::F32Array(Rc::new(RefCell::new(slice)))
                            }
                        }
                        Value::I64Array(arr) =>
                        {
                            let vec = arr.borrow();
                            let len = vec.len();
                            let start_clamped = start_idx.min(len);
                            let end_clamped = end_idx.min(len);
                            if start_clamped >= end_clamped
                            {
                                Value::I64Array(Rc::new(RefCell::new(Vec::new())))
                            }
                            else
                            {
                                let slice: Vec<i64> = vec[start_clamped..end_clamped].to_vec();
                                Value::I64Array(Rc::new(RefCell::new(slice)))
                            }
                        }
                        Value::I32Array(arr) =>
                        {
                            let vec = arr.borrow();
                            let len = vec.len();
                            let start_clamped = start_idx.min(len);
                            let end_clamped = end_idx.min(len);
                            if start_clamped >= end_clamped
                            {
                                Value::I32Array(Rc::new(RefCell::new(Vec::new())))
                            }
                            else
                            {
                                let slice: Vec<i32> = vec[start_clamped..end_clamped].to_vec();
                                Value::I32Array(Rc::new(RefCell::new(slice)))
                            }
                        }
                        _ =>
                        {
                            return Err(RuntimeError::simple("Slice is only supported for strings and arrays"
                                    .to_string(), 0));
                        }
                    };
                    frame.stack.push(result);
                }
                Instruction::IndexCached(cache) =>
                {
                    let index_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing index for index expression".to_string(), 0))?;
                    let target_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing target for index expression".to_string(), 0))?;
                    let result = eval_index_cached_value(index_val, target_val, &cache)?;
                    frame.stack.push(result);
                }
                Instruction::MapIndexCached(cache) =>
                {
                    let index_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing index for index expression".to_string(), 0))?;
                    let target_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing target for index expression".to_string(), 0))?;
                    let result = eval_map_index_cached_value(index_val, target_val, &cache)?;
                    frame.stack.push(result);
                }
                Instruction::F64IndexCached(cache) =>
                {
                    let index_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing index for index expression".to_string(), 0))?;
                    let target_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing target for index expression".to_string(), 0))?;
                    let result = eval_f64_index_cached_value(index_val, target_val, &cache)?;
                    frame.stack.push(result);
                }
                Instruction::IndexAssign =>
                {
                    let value = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing value for index assignment".to_string(), 0))?;
                    let index_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing index for index assignment".to_string(), 0))?;
                    let target_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing target for index assignment".to_string(), 0))?;
                    let result =
                        eval_index_assign_value(interpreter, target_val, index_val, value)?;
                    frame.stack.push(result);
                }
                Instruction::F64IndexAssignCached(cache) =>
                {
                    let value = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing value for index assignment".to_string(), 0))?;
                    let index_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing index for index assignment".to_string(), 0))?;
                    let target_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing target for index assignment".to_string(), 0))?;
                    let result = match target_val
                    {
                        Value::F64Array(arr) =>
                        {
                            let out_value = value;
                            let idx = match index_val
                            {
                                Value::Integer { value, .. } if value >= 0 => value as usize,
                                Value::Unsigned { value, .. } => value as usize,
                                _ =>
                                {
                                    let fallback = Value::F64Array(arr.clone());
                                    return eval_index_assign_value(
                                        interpreter,
                                        fallback,
                                        index_val,
                                        out_value,
                                    );
                                }
                            };
                            let num = match &out_value
                            {
                                Value::Float { value, .. } => *value,
                                Value::Integer { value, .. } => *value as f64,
                                Value::Unsigned { value, .. } => *value as f64,
                                other =>
                                {
                                    let fallback = Value::F64Array(arr.clone());
                                    return eval_index_assign_value(
                                        interpreter,
                                        fallback,
                                        index_val,
                                        other.clone(),
                                    );
                                }
                            };
                            let arr_ptr = Rc::as_ptr(&arr) as usize;
                            let mut cache_mut = cache.borrow_mut();
                            if cache_mut.array_ptr == Some(arr_ptr)
                                && cache_mut.index_usize == Some(idx)
                            {
                                cache_mut.hits += 1;
                            }
                            else
                            {
                                cache_mut.array_ptr = Some(arr_ptr);
                                cache_mut.index_usize = Some(idx);
                                cache_mut.misses += 1;
                            }
                            let mut vec = arr.borrow_mut();
                            if idx < vec.len()
                            {
                                vec[idx] = num;
                                out_value
                            }
                            else
                            {
                                return Err(RuntimeError::simple("Array index out of bounds".to_string(), 0));
                            }
                        }
                        other => eval_index_assign_value(interpreter, other, index_val, value)?,
                    };
                    frame.stack.push(result);
                }
                Instruction::F64ArrayGen { count } =>
                {
                    let n = if let Some(count) = count
                    {
                        count
                    }
                    else
                    {
                        let size_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing size for array generator".to_string(), 0))?;
                        int_value_as_usize(&size_val).ok_or_else(|| RuntimeError::simple("Array size must be a non-negative integer".to_string(), 0))?
                    };
                    if let Some(count) = count
                    {
                        if count > frame.stack.len()
                        {
                            return Err(RuntimeError::simple("Invalid array length".to_string(), 0));
                        }
                        let mut raw_vals = Vec::with_capacity(count);
                        for _ in 0..count
                        {
                            raw_vals.push(frame.stack.pop().unwrap());
                        }
                        raw_vals.reverse();
                        let mut i32_vals: Vec<i32> = Vec::new();
                        let mut i64_vals: Vec<i64> = Vec::new();
                        let mut f32_vals: Vec<f32> = Vec::new();
                        let mut f64_vals: Vec<f64> = Vec::new();
                        let mut all_i32 = true;
                        let mut all_i64 = true;
                        let mut all_f32 = true;
                        for val in &raw_vals
                        {
                            if all_i32
                            {
                                match val
                                {
                                    Value::Integer {
                                        value,
                                        kind: IntKind::I32,
                                    } => i32_vals.push(*value as i32),
                                    Value::Unsigned {
                                        value,
                                        kind: IntKind::U32,
                                    } => i32_vals.push(*value as i32),
                                    _ => all_i32 = false,
                                }
                            }
                            if all_f32
                            {
                                match val
                                {
                                    Value::Float {
                                        value,
                                        kind: FloatKind::F32,
                                    } => f32_vals.push(*value as f32),
                                    _ => all_f32 = false,
                                }
                            }
                            if all_i64
                            {
                                match val
                                {
                                    Value::Integer { value, .. } =>
                                    {
                                        i64_vals.push(*value as i64);
                                        f64_vals.push(*value as f64);
                                        continue;
                                    }
                                    Value::Unsigned { value, .. } =>
                                    {
                                        i64_vals.push(*value as i64);
                                        f64_vals.push(*value as f64);
                                        continue;
                                    }
                                    _ =>
                                    {
                                        all_i64 = false;
                                    }
                                }
                            }
                            match val
                            {
                                Value::Float { value, .. } =>
                                {
                                    f64_vals.push(*value);
                                    continue;
                                }
                                _ =>
                                {
                                    if let Some(num) = int_value_as_f64(val)
                                    {
                                        f64_vals.push(num);
                                        continue;
                                    }
                                    return Err(RuntimeError::simple("Numeric array literal requires numeric elements"
                                            .to_string(), 0));
                                }
                            }
                        }
                        if all_i32
                        {
                            frame
                                .stack
                                .push(Value::I32Array(Rc::new(RefCell::new(i32_vals))));
                        }
                        else if all_i64
                        {
                            frame
                                .stack
                                .push(Value::I64Array(Rc::new(RefCell::new(i64_vals))));
                        }
                        else if all_f32
                        {
                            frame
                                .stack
                                .push(Value::F32Array(Rc::new(RefCell::new(f32_vals))));
                        }
                        else
                        {
                            frame
                                .stack
                                .push(Value::F64Array(Rc::new(RefCell::new(f64_vals))));
                        }
                    }
                    else
                    {
                        let gen_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing generator for array".to_string(), 0))?;
                        match gen_val
                        {
                            Value::Float {
                                value,
                                kind: FloatKind::F32,
                            } =>
                            {
                                let vals = vec![value as f32; n];
                                frame
                                    .stack
                                    .push(Value::F32Array(Rc::new(RefCell::new(vals))));
                            }
                            Value::Float { value, .. } =>
                            {
                                let vals = vec![value; n];
                                frame
                                    .stack
                                    .push(Value::F64Array(Rc::new(RefCell::new(vals))));
                            }
                            Value::Integer {
                                value,
                                kind: IntKind::I32,
                            } =>
                            {
                                let vals = vec![value as i32; n];
                                frame
                                    .stack
                                    .push(Value::I32Array(Rc::new(RefCell::new(vals))));
                            }
                            Value::Unsigned {
                                value,
                                kind: IntKind::U32,
                            } =>
                            {
                                let vals = vec![value as i32; n];
                                frame
                                    .stack
                                    .push(Value::I32Array(Rc::new(RefCell::new(vals))));
                            }
                            Value::Integer { value, .. } =>
                            {
                                let vals = vec![value as i64; n];
                                frame
                                    .stack
                                    .push(Value::I64Array(Rc::new(RefCell::new(vals))));
                            }
                            Value::Unsigned { value, .. } =>
                            {
                                let vals = vec![value as i64; n];
                                frame
                                    .stack
                                    .push(Value::I64Array(Rc::new(RefCell::new(vals))));
                            }
                            _ =>
                            {
                                return Err(RuntimeError::simple("Numeric array generator requires numeric value"
                                        .to_string(), 0));
                            }
                        };
                    }
                }
                Instruction::ArrayGen =>
                {
                    let size_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing size for array generator".to_string(), 0))?;
                    let gen_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing generator for array".to_string(), 0))?;
                    let n = int_value_as_usize(&size_val).ok_or_else(|| RuntimeError::simple("Array size must be a non-negative integer".to_string(), 0))?;
                    let mut vals: Vec<Value> = Vec::with_capacity(n);
                    if let Value::Function(data) = gen_val
                    {
                        for i in 0..n
                        {
                            let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(
                                Value::Uninitialized,
                                data.declarations.len(),
                            );
                            interpreter.ensure_slot_capacity(
                                &mut new_slots,
                                data.param_offset,
                                data.params.len(),
                                &data.bound_args,
                            );
                            interpreter.apply_bound_args(&data.bound_args, &mut new_slots);
                            if !data.params.is_empty()
                            {
                                new_slots[data.param_offset] = default_int(i as i128);
                            }
                            let result = if let Some(code) = &data.code
                            {
                                execute_instructions(interpreter, code, const_pool, &mut new_slots)?
                            }
                            else if data.uses_env
                            {
                                let new_env = interpreter.get_env(Some(data.env.clone()), false);
                                let original_env = interpreter.env.clone();
                                interpreter.env = new_env.clone();
                                let result = interpreter.eval(&data.body, &mut new_slots)?;
                                interpreter.env = original_env;
                                interpreter.recycle_env(new_env);
                                result
                            }
                            else
                            {
                                interpreter.eval(&data.body, &mut new_slots)?
                            };
                            vals.push(result);
                        }
                    }
                    else
                    {
                        for _ in 0..n
                        {
                            vals.push(gen_val.clone());
                        }
                    }
                    if vals.is_empty()
                    {
                        frame
                            .stack
                            .push(Value::Array(Rc::new(RefCell::new(Vec::new()))));
                        continue;
                    }
                    let mut i32_vals: Vec<i32> = Vec::new();
                    let mut i64_vals: Vec<i64> = Vec::new();
                    let mut f32_vals: Vec<f32> = Vec::new();
                    let mut f64_vals: Vec<f64> = Vec::new();
                    let mut all_i32 = true;
                    let mut all_i64 = true;
                    let mut all_f32 = true;
                    let mut all_f64 = true;
                    for v in &vals
                    {
                        if all_i32
                        {
                            match v
                            {
                                Value::Integer {
                                    value,
                                    kind: IntKind::I32,
                                } => i32_vals.push(*value as i32),
                                Value::Unsigned {
                                    value,
                                    kind: IntKind::U32,
                                } => i32_vals.push(*value as i32),
                                _ => all_i32 = false,
                            }
                        }
                        if all_f32
                        {
                            match v
                            {
                                Value::Float {
                                    value,
                                    kind: FloatKind::F32,
                                } => f32_vals.push(*value as f32),
                                _ => all_f32 = false,
                            }
                        }
                        if all_i64
                        {
                            match v
                            {
                                Value::Integer { value, .. } =>
                                {
                                    i64_vals.push(*value as i64);
                                    if all_f64
                                    {
                                        f64_vals.push(*value as f64);
                                    }
                                    continue;
                                }
                                Value::Unsigned { value, .. } =>
                                {
                                    i64_vals.push(*value as i64);
                                    if all_f64
                                    {
                                        f64_vals.push(*value as f64);
                                    }
                                    continue;
                                }
                                _ =>
                                {
                                    all_i64 = false;
                                }
                            }
                        }
                        if all_f64
                        {
                            match v
                            {
                                Value::Float { value, .. } =>
                                {
                                    f64_vals.push(*value);
                                    continue;
                                }
                                _ =>
                                {
                                    if let Some(num) = int_value_as_f64(v)
                                    {
                                        f64_vals.push(num);
                                        continue;
                                    }
                                    all_f64 = false;
                                }
                            }
                        }
                    }
                    if all_i32
                    {
                        frame
                            .stack
                            .push(Value::I32Array(Rc::new(RefCell::new(i32_vals))));
                    }
                    else if all_i64
                    {
                        frame
                            .stack
                            .push(Value::I64Array(Rc::new(RefCell::new(i64_vals))));
                    }
                    else if all_f32
                    {
                        frame
                            .stack
                            .push(Value::F32Array(Rc::new(RefCell::new(f32_vals))));
                    }
                    else if all_f64
                    {
                        frame
                            .stack
                            .push(Value::F64Array(Rc::new(RefCell::new(f64_vals))));
                    }
                    else
                    {
                        frame.stack.push(Value::Array(Rc::new(RefCell::new(vals))));
                    }
                }
                Instruction::F64Axpy {
                    dst_slot,
                    dst_index_slot,
                    src_slot,
                    src_index_slot,
                } =>
                {
                    let scalar_val = frame.stack.pop().ok_or_else(|| RuntimeError::simple("Missing scalar for F64Axpy".to_string(), 0))?;
                    let scalar = match scalar_val
                    {
                        Value::Float { value, .. } => value,
                        v => int_value_as_f64(&v).ok_or_else(|| RuntimeError::simple("F64Axpy requires numeric scalar".to_string(), 0))?,
                    };
                    let dst_idx = match slots.get(dst_index_slot)
                    {
                        Some(v) => number_to_usize(v).ok_or_else(|| RuntimeError::simple("F64Axpy dst index must be numeric".to_string(), 0))?,
                        None =>
                        {
                            return Err(RuntimeError::simple("F64Axpy dst index must be numeric".to_string(), 0));
                        }
                    };
                    let src_idx = match slots.get(src_index_slot)
                    {
                        Some(v) => number_to_usize(v).ok_or_else(|| RuntimeError::simple("F64Axpy src index must be numeric".to_string(), 0))?,
                        None =>
                        {
                            return Err(RuntimeError::simple("F64Axpy src index must be numeric".to_string(), 0));
                        }
                    };
                    let dst = match slots.get_mut(dst_slot)
                    {
                        Some(Value::F64Array(arr)) => arr.clone(),
                        _ =>
                        {
                            return Err(RuntimeError::simple("F64Axpy requires F64Array dst".to_string(), 0));
                        }
                    };
                    let src = match slots.get(src_slot)
                    {
                        Some(Value::F64Array(arr)) => arr.clone(),
                        _ =>
                        {
                            return Err(RuntimeError::simple("F64Axpy requires F64Array src".to_string(), 0));
                        }
                    };
                    let mut dst_vec = dst.borrow_mut();
                    let src_vec = src.borrow();
                    if dst_idx >= dst_vec.len() || src_idx >= src_vec.len()
                    {
                        return Err(RuntimeError::simple("F64Axpy index out of bounds".to_string(), 0));
                    }
                    let result = dst_vec[dst_idx] + scalar * src_vec[src_idx];
                    dst_vec[dst_idx] = result;
                    frame.stack.push(make_float(result, FloatKind::F64));
                }
                Instruction::F64DotRange {
                    acc_slot,
                    a_slot,
                    b_slot,
                    index_slot,
                    end,
                } =>
                {
                    let start = match slots.get(index_slot)
                    {
                        Some(v) => number_to_usize(v).unwrap_or(0),
                        None => 0,
                    };
                    let end_val = match end
                    {
                        RangeEnd::Slot(s) => slots.get(s).cloned().unwrap_or(Value::Nil),
                        RangeEnd::Const(idx) => const_pool.get(idx).cloned().unwrap_or(Value::Nil),
                    };
                    let end_idx = number_to_usize(&end_val).ok_or_else(|| RuntimeError::simple("Range end must be a non-negative number".to_string(), 0))?;
                    let acc = match slots.get(acc_slot)
                    {
                        Some(Value::Float { value, .. }) => *value,
                        Some(v) => int_value_as_f64(v).unwrap_or(0.0),
                        _ => 0.0,
                    };
                    let a = match slots.get(a_slot)
                    {
                        Some(Value::F64Array(arr)) => arr.clone(),
                        _ =>
                        {
                            return Err(RuntimeError::simple("F64DotRange requires F64Array a".to_string(), 0));
                        }
                    };
                    let b = match slots.get(b_slot)
                    {
                        Some(Value::F64Array(arr)) => arr.clone(),
                        _ =>
                        {
                            return Err(RuntimeError::simple("F64DotRange requires F64Array b".to_string(), 0));
                        }
                    };
                    let a_vec = a.borrow();
                    let b_vec = b.borrow();
                    if end_idx > a_vec.len() || end_idx > b_vec.len()
                    {
                        return Err(RuntimeError::simple("F64DotRange index out of bounds".to_string(), 0));
                    }
                    let mut i = start;
                    let mut sum = Simd::<f64, 4>::splat(0.0);
                    while i + 4 <= end_idx
                    {
                        let av = Simd::from_slice(&a_vec[i..i + 4]);
                        let bv = Simd::from_slice(&b_vec[i..i + 4]);
                        sum += av * bv;
                        i += 4;
                    }
                    let mut total = acc + sum.reduce_sum();
                    while i < end_idx
                    {
                        total += a_vec[i] * b_vec[i];
                        i += 1;
                    }
                    if let Some(slot) = slots.get_mut(acc_slot)
                    {
                        *slot = make_float(total, FloatKind::F64);
                    }
                    if let Some(slot) = slots.get_mut(index_slot)
                    {
                        *slot = default_int(end_idx as i128);
                    }
                    frame.stack.push(make_float(total, FloatKind::F64));
                }
                Instruction::F64Dot2Range {
                    acc1_slot,
                    a1_slot,
                    b1_slot,
                    acc2_slot,
                    a2_slot,
                    b2_slot,
                    index_slot,
                    end,
                } =>
                {
                    let start = match slots.get(index_slot)
                    {
                        Some(v) => number_to_usize(v).unwrap_or(0),
                        None => 0,
                    };
                    let end_val = match end
                    {
                        RangeEnd::Slot(s) => slots.get(s).cloned().unwrap_or(Value::Nil),
                        RangeEnd::Const(idx) => const_pool.get(idx).cloned().unwrap_or(Value::Nil),
                    };
                    let end_idx = number_to_usize(&end_val).ok_or_else(|| RuntimeError::simple("Range end must be a non-negative number".to_string(), 0))?;
                    let acc1 = match slots.get(acc1_slot)
                    {
                        Some(Value::Float { value, .. }) => *value,
                        Some(v) => int_value_as_f64(v).unwrap_or(0.0),
                        _ => 0.0,
                    };
                    let acc2 = match slots.get(acc2_slot)
                    {
                        Some(Value::Float { value, .. }) => *value,
                        Some(v) => int_value_as_f64(v).unwrap_or(0.0),
                        _ => 0.0,
                    };
                    let a1 = match slots.get(a1_slot)
                    {
                        Some(Value::F64Array(arr)) => arr.clone(),
                        _ =>
                        {
                            return Err(RuntimeError::simple("F64Dot2Range requires F64Array a1".to_string(), 0));
                        }
                    };
                    let b1 = match slots.get(b1_slot)
                    {
                        Some(Value::F64Array(arr)) => arr.clone(),
                        _ =>
                        {
                            return Err(RuntimeError::simple("F64Dot2Range requires F64Array b1".to_string(), 0));
                        }
                    };
                    let a2 = match slots.get(a2_slot)
                    {
                        Some(Value::F64Array(arr)) => arr.clone(),
                        _ =>
                        {
                            return Err(RuntimeError::simple("F64Dot2Range requires F64Array a2".to_string(), 0));
                        }
                    };
                    let b2 = match slots.get(b2_slot)
                    {
                        Some(Value::F64Array(arr)) => arr.clone(),
                        _ =>
                        {
                            return Err(RuntimeError::simple("F64Dot2Range requires F64Array b2".to_string(), 0));
                        }
                    };
                    let a1_vec = a1.borrow();
                    let b1_vec = b1.borrow();
                    let a2_vec = a2.borrow();
                    let b2_vec = b2.borrow();
                    if end_idx > a1_vec.len()
                        || end_idx > b1_vec.len()
                        || end_idx > a2_vec.len()
                        || end_idx > b2_vec.len()
                    {
                        return Err(RuntimeError::simple("F64Dot2Range index out of bounds".to_string(), 0));
                    }
                    let mut i = start;
                    let mut sum1 = Simd::<f64, 4>::splat(0.0);
                    let mut sum2 = Simd::<f64, 4>::splat(0.0);
                    while i + 4 <= end_idx
                    {
                        let a1v = Simd::from_slice(&a1_vec[i..i + 4]);
                        let b1v = Simd::from_slice(&b1_vec[i..i + 4]);
                        let a2v = Simd::from_slice(&a2_vec[i..i + 4]);
                        let b2v = Simd::from_slice(&b2_vec[i..i + 4]);
                        sum1 += a1v * b1v;
                        sum2 += a2v * b2v;
                        i += 4;
                    }
                    let mut total1 = acc1 + sum1.reduce_sum();
                    let mut total2 = acc2 + sum2.reduce_sum();
                    while i < end_idx
                    {
                        total1 += a1_vec[i] * b1_vec[i];
                        total2 += a2_vec[i] * b2_vec[i];
                        i += 1;
                    }
                    if let Some(slot) = slots.get_mut(acc1_slot)
                    {
                        *slot = make_float(total1, FloatKind::F64);
                    }
                    if let Some(slot) = slots.get_mut(acc2_slot)
                    {
                        *slot = make_float(total2, FloatKind::F64);
                    }
                    if let Some(slot) = slots.get_mut(index_slot)
                    {
                        *slot = default_int(end_idx as i128);
                    }
                    frame.stack.push(make_float(total2, FloatKind::F64));
                }
                Instruction::Add
                | Instruction::Sub
                | Instruction::Mul
                | Instruction::Div
                | Instruction::Pow
                | Instruction::Eq
                | Instruction::Gt
                | Instruction::Lt =>
                {
                    let r = frame.stack.pop().unwrap();
                    let l = frame.stack.pop().unwrap();
                    let op = match inst
                    {
                        Instruction::Add => BinOpKind::Add,
                        Instruction::Sub => BinOpKind::Sub,
                        Instruction::Mul => BinOpKind::Mul,
                        Instruction::Div => BinOpKind::Div,
                        Instruction::Pow => BinOpKind::Pow,
                        Instruction::Eq => BinOpKind::Eq,
                        Instruction::Gt => BinOpKind::Gt,
                        Instruction::Lt => BinOpKind::Lt,
                        _ => unreachable!(),
                    };
                    let res = eval_binop(op, l, r)?;
                    frame.stack.push(res);
                }
            }

            if advance_ip
            {
                frame.ip += 1;
            }
        }

        if let Some(frame) = next_frame
        {
            frames.push(frame);
        }
    }
}

fn is_simple(expr: &Expr) -> bool
{
    match &expr.kind
    {
        ExprKind::Yield(_)
        | ExprKind::FunctionDef { .. }
        | ExprKind::MethodDef { .. }
        | ExprKind::AnonymousFunction { .. }
        | ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. }
        | ExprKind::FilePublic(_)
        | ExprKind::FunctionPublic(_)
        | ExprKind::StructDef { .. } => false,
        ExprKind::Block(stmts) => stmts.iter().all(is_simple),
        ExprKind::FormatString(parts) => parts.iter().all(|part| {
            if let crate::ast::FormatPart::Expr { expr, .. } = part
            {
                is_simple(expr)
            }
            else
            {
                true
            }
        }),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            is_simple(condition)
                && is_simple(then_branch)
                && else_branch.as_ref().map_or(true, |eb| is_simple(eb))
        }
        ExprKind::While { condition, body } => is_simple(condition) && is_simple(body),
        ExprKind::For { iterable, body, .. } => is_simple(iterable) && is_simple(body),
        ExprKind::Loop { count, body, .. } => is_simple(count) && is_simple(body),
        ExprKind::Collect { count, into, body, .. } =>
        {
            let into_simple = into.as_ref().map_or(true, |expr| is_simple(expr));
            is_simple(count) && into_simple && is_simple(body)
        }
        ExprKind::Result { .. } => false,
        ExprKind::BinaryOp { left, right, .. } => is_simple(left) && is_simple(right),
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            if block.is_some()
            {
                return false;
            }
            is_simple(function) && args.iter().all(is_simple)
        }
        ExprKind::Array(elements) => elements.iter().all(is_simple),
        ExprKind::StructLiteral { fields, .. } => fields.iter().all(|(_, v)| is_simple(v)),
        ExprKind::ArrayGenerator { generator, size } => is_simple(generator) && is_simple(size),
        ExprKind::Map(entries) => entries.iter().all(|(k, v)| is_simple(k) && is_simple(v)),
        ExprKind::Index { target, index } => is_simple(target) && is_simple(index),
        ExprKind::Slice { target, start, end } =>
        {
            is_simple(target) && is_simple(start) && is_simple(end)
        }
        ExprKind::EnvFreeze(_) => false,
        ExprKind::ErrorRaise(_) => false,
        ExprKind::Not(expr) => is_simple(expr),
        ExprKind::And { left, right } | ExprKind::AndBool { left, right } =>
        {
            is_simple(left) && is_simple(right)
        }
        ExprKind::Or { left, right } | ExprKind::OrBool { left, right } =>
        {
            is_simple(left) && is_simple(right)
        }
        ExprKind::Clone(expr) => is_simple(expr),
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } => is_simple(target) && is_simple(index) && is_simple(value),
        _ => true,
    }
}

fn should_compile(simple: bool, _uses_env: bool, mode: BytecodeMode) -> bool
{
    match mode
    {
        BytecodeMode::Off => false,
        BytecodeMode::Simple => simple && !_uses_env,
        BytecodeMode::Advanced => simple && !_uses_env,
    }
}

fn resolve(expr: &mut Expr, slot_map: &FxHashMap<SymbolId, usize>)
{
    match &mut expr.kind
    {
        ExprKind::Identifier { name, slot } =>
        {
            if let Some(s) = slot_map.get(name)
            {
                *slot = Some(*s);
            }
        }
        ExprKind::Assignment { name, value, slot } =>
        {
            if let Some(s) = slot_map.get(name)
            {
                *slot = Some(*s);
            }
            resolve(value, slot_map);
        }
        ExprKind::FilePublic(expr) =>
        {
            resolve(expr, slot_map);
        }
        ExprKind::FunctionPublic(expr) =>
        {
            resolve(expr, slot_map);
        }
        ExprKind::BinaryOp { left, right, .. } =>
        {
            resolve(left, slot_map);
            resolve(right, slot_map);
        }
        ExprKind::Block(stmts) =>
        {
            for stmt in stmts
            {
                resolve(stmt, slot_map);
            }
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            resolve(condition, slot_map);
            resolve(then_branch, slot_map);
            if let Some(eb) = else_branch
            {
                resolve(eb, slot_map);
            }
        }
        ExprKind::Result {
            body,
            else_expr,
            else_binding,
            else_slot,
        } =>
        {
            if let Some(name) = else_binding
            {
                if let Some(s) = slot_map.get(name)
                {
                    *else_slot = Some(*s);
                }
            }
            resolve(body, slot_map);
            resolve(else_expr, slot_map);
        }
        ExprKind::While { condition, body } =>
        {
            resolve(condition, slot_map);
            resolve(body, slot_map);
        }
        ExprKind::For {
            var,
            var_slot,
            iterable,
            body,
            ..
        } =>
        {
            if let Some(s) = slot_map.get(var)
            {
                *var_slot = Some(*s);
            }
            resolve(iterable, slot_map);
            resolve(body, slot_map);
        }
        ExprKind::Loop {
            var,
            var_slot,
            count,
            body,
        } =>
        {
            if let Some(name) = var
            {
                if let Some(s) = slot_map.get(name)
                {
                    *var_slot = Some(*s);
                }
            }
            resolve(count, slot_map);
            resolve(body, slot_map);
        }
        ExprKind::Collect {
            var,
            var_slot,
            count,
            into,
            body,
            ..
        } =>
        {
            if let Some(name) = var
            {
                if let Some(s) = slot_map.get(name)
                {
                    *var_slot = Some(*s);
                }
            }
            resolve(count, slot_map);
            if let Some(into) = into
            {
                resolve(into, slot_map);
            }
            resolve(body, slot_map);
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            resolve(function, slot_map);
            for arg in args
            {
                resolve(arg, slot_map);
            }
            if let Some(c) = block
            {
                resolve(&mut c.body, slot_map);
            }
        }
        ExprKind::Array(elements) =>
        {
            for e in elements
            {
                resolve(e, slot_map);
            }
        }
        ExprKind::StructLiteral { fields, .. } =>
        {
            for (_, expr) in fields
            {
                resolve(expr, slot_map);
            }
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            resolve(generator, slot_map);
            resolve(size, slot_map);
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                resolve(k, slot_map);
                resolve(v, slot_map);
            }
        }
        ExprKind::Index { target, index } =>
        {
            resolve(target, slot_map);
            resolve(index, slot_map);
        }
        ExprKind::Slice { target, start, end } =>
        {
            resolve(target, slot_map);
            resolve(start, slot_map);
            resolve(end, slot_map);
        }
        ExprKind::Not(expr) =>
        {
            resolve(expr, slot_map);
        }
        ExprKind::And { left, right } | ExprKind::AndBool { left, right } =>
        {
            resolve(left, slot_map);
            resolve(right, slot_map);
        }
        ExprKind::Or { left, right } | ExprKind::OrBool { left, right } =>
        {
            resolve(left, slot_map);
            resolve(right, slot_map);
        }
        ExprKind::Clone(expr) =>
        {
            resolve(expr, slot_map);
        }
        ExprKind::ErrorRaise(expr) =>
        {
            resolve(expr, slot_map);
        }
        ExprKind::EnvFreeze(expr) =>
        {
            resolve(expr, slot_map);
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            resolve(target, slot_map);
            resolve(index, slot_map);
            resolve(value, slot_map);
        }
        ExprKind::FormatString(parts) =>
        {
            for part in parts
            {
                if let crate::ast::FormatPart::Expr { expr, .. } = part
                {
                    resolve(expr, slot_map);
                }
            }
        }
        ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. } =>
        {}
        ExprKind::Yield(args) =>
        {
            for a in args
            {
                resolve(a, slot_map);
            }
        }
        _ =>
        {}
    }
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

        let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let lexer = crate::lexer::Lexer::new(&source);
            let mut parser = crate::parser::Parser::new(lexer);
            parser.parse()
        }));

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

    fn call_builtin(&mut self, builtin: &Builtin, args: &[Value]) -> EvalResult
    {
        match builtin
        {
            Builtin::Puts =>
            {
                let mut last = Value::Nil;
                for arg in args
                {
                    println!("{}", arg);
                    last = arg.clone();
                }
                Ok(last)
            }
            Builtin::Print =>
            {
                let mut last = Value::Nil;
                for arg in args
                {
                    print!("{}", arg);
                    io::stdout().flush().unwrap();
                    last = arg.clone();
                }
                Ok(last)
            }
            Builtin::Eputs =>
            {
                let mut last = Value::Nil;
                let mut stderr = io::stderr();
                for arg in args
                {
                    writeln!(stderr, "{}", arg).unwrap();
                    last = arg.clone();
                }
                Ok(last)
            }
            Builtin::Eprint =>
            {
                let mut last = Value::Nil;
                let mut stderr = io::stderr();
                for arg in args
                {
                    write!(stderr, "{}", arg).unwrap();
                    stderr.flush().unwrap();
                    last = arg.clone();
                }
                Ok(last)
            }
            Builtin::Log =>
            {
                let mut last = Value::Nil;
                for arg in args
                {
                    let msg = arg.to_string();
                    self.write_log(LogLevel::Info, &msg)?;
                    last = arg.clone();
                }
                Ok(last)
            }
            Builtin::Assert =>
            {
                let condition = args.get(0).cloned().unwrap_or(Value::Nil);
                let passed = !matches!(condition, Value::Boolean(false) | Value::Nil);
                if passed
                {
                    Ok(condition)
                }
                else
                {
                    let msg = match args.get(1)
                    {
                        Some(val) => val.to_string(),
                        None => "assertion failed".to_string(),
                    };
                    Err(RuntimeError::simple(msg, 0))
                }
            }
            Builtin::AssertEq =>
            {
                let left = args.get(0).cloned().unwrap_or(Value::Nil);
                let right = args.get(1).cloned().unwrap_or(Value::Nil);
                if left == right
                {
                    Ok(left)
                }
                else
                {
                    let msg = match args.get(2)
                    {
                        Some(val) => val.to_string(),
                        None => format!("assert_eq failed: {} != {}", left, right),
                    };
                    Err(RuntimeError::simple(msg, 0))
                }
            }
            Builtin::Len =>
            {
                let val = args.get(0).cloned().unwrap_or(Value::Nil);
                match val
                {
                    Value::String(s) => Ok(default_int(s.len() as i128)),
                    Value::Array(arr) => Ok(default_int(arr.borrow().len() as i128)),
                    Value::F32Array(arr) => Ok(default_int(arr.borrow().len() as i128)),
                    Value::F64Array(arr) => Ok(default_int(arr.borrow().len() as i128)),
                    Value::I32Array(arr) => Ok(default_int(arr.borrow().len() as i128)),
                    Value::I64Array(arr) => Ok(default_int(arr.borrow().len() as i128)),
                    Value::Bytes(bytes) => Ok(default_int(bytes.len() as i128)),
                    Value::ByteBuf(buf) => Ok(default_int(buf.borrow().len() as i128)),
                    Value::BytesView(view) => Ok(default_int(view.len as i128)),
                    Value::Map(map) => Ok(default_int(map.borrow().data.len() as i128)),
                    Value::Env(env) => Ok(default_int(env.data.len() as i128)),
                    Value::Mmap(mmap) => Ok(default_int(mmap.len() as i128)),
                    Value::MmapMut(mmap) => Ok(default_int(mmap.borrow().len() as i128)),
                    _ => Ok(default_int(0)),
                }
            }
            Builtin::ReadFile =>
            {
                let path = args.get(0).cloned().unwrap_or(Value::Nil).to_string();
                match fs::read_to_string(&path)
                {
                    Ok(content) => Ok(Value::String(intern::intern_owned(content))),
                    Err(_) => Ok(Value::Nil),
                }
            }
            Builtin::WriteFile =>
            {
                let path = args.get(0).cloned().unwrap_or(Value::Nil).to_string();
                let content = args.get(1).cloned().unwrap_or(Value::Nil).to_string();
                match fs::File::create(&path)
                {
                    Ok(mut file) =>
                    {
                        write!(file, "{}", content).unwrap();
                        Ok(Value::Boolean(true))
                    }
                    Err(_) => Ok(Value::Boolean(false)),
                }
            }
            Builtin::Typeof =>
            {
                let val = args.get(0).cloned().unwrap_or(Value::Nil);
                let type_name = match &val
                {
                    Value::Integer { kind, .. } => match kind
                    {
                        IntKind::I8 => "Int8",
                        IntKind::I16 => "Int16",
                        IntKind::I32 => "Int32",
                        IntKind::I64 => "Int64",
                        IntKind::I128 => "Int128",
                        _ => "Integer",
                    },
                    Value::Unsigned { kind, .. } => match kind
                    {
                        IntKind::U8 => "Uint8",
                        IntKind::U16 => "Uint16",
                        IntKind::U32 => "Uint32",
                        IntKind::U64 => "Uint64",
                        IntKind::U128 => "Uint128",
                        _ => "Unsigned",
                    },
                    Value::Float { kind, .. } => match kind
                    {
                        FloatKind::F32 => "Float32",
                        FloatKind::F64 => "Float64",
                        FloatKind::F128 => "Float128",
                    },
                    Value::String(_) => "String",
                    Value::Boolean(_) => "Boolean",
                    Value::Array(_) => "Array",
                    Value::F32Array(_) => "F32Array",
                    Value::F64Array(_) => "F64Array",
                    Value::I32Array(_) => "I32Array",
                    Value::I64Array(_) => "I64Array",
                    Value::Bytes(_) => "Bytes",
                    Value::ByteBuf(_) => "ByteBuf",
                    Value::BytesView(_) => "BytesView",
                    Value::StructType(ty) =>
                    {
                        return Ok(Value::String(intern::intern_owned(format!(
                            "StructType({})",
                            ty.name
                        ))));
                    }
                    Value::StructInstance(inst) => return Ok(Value::String(inst.ty.name.clone())),
                    Value::BoundMethod(_) => "BoundMethod",
                    Value::Map(_) => "Map",
                    Value::Env(_) => "Env",
                    Value::Ast(_) => "Ast",
                    Value::DataFrame(_) => "DataFrame",
                    Value::Sqlite(_) => "Sqlite",
                    Value::Mmap(_) => "Mmap",
                    Value::MmapMut(_) => "MmapMut",
                    #[cfg(feature = "lib-net")]
                    Value::NetStream(_) => "NetStream",
                    Value::Nil => "Nil",
                    Value::Function(_) => "Function",
                    Value::NativeFunction(_) => "NativeFunction",
                    Value::HostFunction(_) => "HostFunction",
                    Value::WasmFunction(_) => "WasmFunction",
                    Value::Reference(r) =>
                    {
                        // Recursively get type of referenced value
                        let inner = r.borrow().clone();
                        return self.call_builtin(&Builtin::Typeof, &[inner]);
                    }
                    Value::Uninitialized => "Uninitialized",
                };
                Ok(Value::String(intern::intern(type_name)))
            }
            Builtin::F64 =>
            {
                let val = args.get(0).cloned().unwrap_or(Value::Nil);
                cast_f64_value(&val).map_err(|message| RuntimeError::simple(message, 0))
            }
            Builtin::F32 =>
            {
                let val = args.get(0).cloned().unwrap_or(Value::Nil);
                cast_f32_value(&val).map_err(|message| RuntimeError::simple(message, 0))
            }
            Builtin::I64 =>
            {
                let val = args.get(0).cloned().unwrap_or(Value::Nil);
                cast_i64_value(&val).map_err(|message| RuntimeError::simple(message, 0))
            }
            Builtin::I32 =>
            {
                let val = args.get(0).cloned().unwrap_or(Value::Nil);
                cast_i32_value(&val).map_err(|message| RuntimeError::simple(message, 0))
            }
            Builtin::U64 =>
            {
                let val = args.get(0).cloned().unwrap_or(Value::Nil);
                cast_u64_value(&val).map_err(|message| RuntimeError::simple(message, 0))
            }
            Builtin::U32 =>
            {
                let val = args.get(0).cloned().unwrap_or(Value::Nil);
                cast_u32_value(&val).map_err(|message| RuntimeError::simple(message, 0))
            }
        }
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

    pub fn eval(&mut self, expr: &Expr, slots: &mut [Value]) -> EvalResult
    {
        let line = expr.line;
        let prev_span = CURRENT_SPAN.with(|span| span.borrow().clone());
        CURRENT_SPAN.with(|span| {
            *span.borrow_mut() = Some((expr.line, expr.column, expr.source.clone()));
        });
        let result = match &expr.kind
        {
            ExprKind::Integer { value, kind } => Ok(make_signed_int(*value, *kind)),
            ExprKind::Unsigned { value, kind } => Ok(make_unsigned_int(*value, *kind)),
            ExprKind::Float { value, kind } => Ok(make_float(*value, *kind)),
            ExprKind::String(s) => Ok(Value::String(s.clone())),
            ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
            ExprKind::Nil => Ok(Value::Nil),
            ExprKind::Use(path) =>
            {
                self.import_path(path, line)?;
                Ok(Value::Nil)
            }
            ExprKind::Import { path, alias } =>
            {
                self.import_module(path, *alias, line, false)?;
                Ok(Value::Nil)
            }
            ExprKind::FilePublic(expr) =>
            {
                match &expr.kind
                {
                    ExprKind::Use(path) =>
                    {
                        self.import_path(path, line)?;
                        if let Some(root) = path.first()
                        {
                            self.env.borrow_mut().mark_public(*root);
                        }
                        Ok(Value::Nil)
                    }
                    ExprKind::Import { path, alias } =>
                    {
                        self.import_module(path, *alias, line, true)?;
                        Ok(Value::Nil)
                    }
                    ExprKind::Load(path) =>
                    {
                        self.load_wasm_module(path, line)?;
                        let wasm_sym = intern::intern_symbol("wasm");
                        self.env.borrow_mut().mark_public(wasm_sym);
                        Ok(Value::Nil)
                    }
                    ExprKind::Assignment { name, value, slot } =>
                    {
                        if slot.is_some()
                        {
                            return Err(RuntimeError::simple("@file assignment requires a global variable".to_string(), line));
                        }
                        let val = self.eval(value, slots)?;
                        self.env.borrow_mut().set(*name, val.clone());
                        self.env.borrow_mut().mark_public(*name);
                        Ok(val)
                    }
                    ExprKind::FunctionDef {
                        name,
                        params,
                        body,
                        slots,
                    } =>
                    {
                        let func_env = self.env.clone();
                        let (resolved_body, slot_names) = if let Some(slot_names) = slots
                        {
                            (body.clone(), slot_names.clone())
                        }
                        else
                        {
                            let mut locals = HashSet::new();
                            collect_declarations(body, &mut locals);
                            let (slot_map, slot_names) = build_slot_map(params, locals);
                            let mut resolved = body.clone();
                            resolve(resolved.as_mut(), &slot_map);
                            (resolved, Rc::new(slot_names))
                        };
                        let simple = is_simple(&resolved_body);
                        let uses_env = uses_environment(&resolved_body);
                        let reg_simple = is_reg_simple(&resolved_body);

                        let mut code = Vec::new();
                        let mut const_pool = Vec::new();
                        let compiled = if should_compile(simple, uses_env, self.bytecode_mode)
                        {
                            let use_caches = self.bytecode_mode == BytecodeMode::Advanced;
                            with_compile_use_caches(use_caches, || {
                                compile_expr(&resolved_body, &mut code, &mut const_pool, true)
                            })
                        }
                        else
                        {
                            false
                        };

                        let reg_code =
                            if reg_simple && !uses_env && self.bytecode_mode != BytecodeMode::Off
                            {
                                compile_reg_function(&resolved_body).map(Rc::new)
                            }
                            else
                            {
                                None
                            };
                        let fast_reg_code = if reg_simple
                            && !uses_env
                            && self.bytecode_mode != BytecodeMode::Off
                        {
                            compile_fast_float_function(&resolved_body).map(Rc::new)
                        }
                        else
                        {
                            None
                        };
                        let func = Value::Function(Rc::new(crate::value::FunctionData {
                            params: params.clone(),
                            body: *resolved_body,
                            declarations: slot_names,
                            param_offset: 0,
                            is_simple: simple,
                            uses_env,
                            code: if compiled { Some(Rc::new(code)) } else { None },
                            reg_code,
                            fast_reg_code,
                            const_pool: Rc::new(const_pool),
                            bound_args: Rc::new(Vec::new()),
                            env: func_env,
                        }));
                        self.env.borrow_mut().define(*name, func.clone());
                        self.env.borrow_mut().mark_public(*name);
                        Ok(func)
                    }
                    _ => Err(RuntimeError::simple("@file can only be used with use/import/load, assignments, or function definitions"
                            .to_string(), line)),
                }
            }
            ExprKind::FunctionPublic(expr) =>
            {
                match &expr.kind
                {
                    ExprKind::Use(path) =>
                    {
                        self.import_path(path, line)?;
                        if let Some(root) = path.first()
                        {
                            self.env.borrow_mut().mark_function_public(*root);
                        }
                        Ok(Value::Nil)
                    }
                    ExprKind::Import { path, alias } =>
                    {
                        let namespace = self.import_module(path, *alias, line, false)?;
                        if let Some(alias) = alias
                        {
                            self.env.borrow_mut().mark_function_public(*alias);
                        }
                        else if let Some(root) = namespace.first()
                        {
                            self.env.borrow_mut().mark_function_public(*root);
                        }
                        Ok(Value::Nil)
                    }
                    ExprKind::Load(path) =>
                    {
                        self.load_wasm_module(path, line)?;
                        let wasm_sym = intern::intern_symbol("wasm");
                        self.env.borrow_mut().mark_function_public(wasm_sym);
                        Ok(Value::Nil)
                    }
                    ExprKind::Assignment { name, value, slot } =>
                    {
                        let val = self.eval(value, slots)?;
                        if let Some(s) = slot
                        {
                            if let Some(slot_val) = slots.get_mut(*s)
                            {
                                *slot_val = val.clone();
                            }
                        }
                        self.env.borrow_mut().set(*name, val.clone());
                        self.env.borrow_mut().mark_function_public(*name);
                        Ok(val)
                    }
                    ExprKind::FunctionDef {
                        name,
                        params,
                        body,
                        slots,
                    } =>
                    {
                        let func_env = self.env.clone();
                        let (resolved_body, slot_names) = if let Some(slot_names) = slots
                        {
                            (body.clone(), slot_names.clone())
                        }
                        else
                        {
                            let mut locals = HashSet::new();
                            collect_declarations(body, &mut locals);
                            let (slot_map, slot_names) = build_slot_map(params, locals);
                            let mut resolved = body.clone();
                            resolve(resolved.as_mut(), &slot_map);
                            (resolved, Rc::new(slot_names))
                        };
                        let simple = is_simple(&resolved_body);
                        let uses_env = uses_environment(&resolved_body);
                        let reg_simple = is_reg_simple(&resolved_body);

                        let mut code = Vec::new();
                        let mut const_pool = Vec::new();
                        let compiled = if should_compile(simple, uses_env, self.bytecode_mode)
                        {
                            let use_caches = self.bytecode_mode == BytecodeMode::Advanced;
                            with_compile_use_caches(use_caches, || {
                                compile_expr(&resolved_body, &mut code, &mut const_pool, true)
                            })
                        }
                        else
                        {
                            false
                        };

                        let reg_code =
                            if reg_simple && !uses_env && self.bytecode_mode != BytecodeMode::Off
                            {
                                compile_reg_function(&resolved_body).map(Rc::new)
                            }
                            else
                            {
                                None
                            };
                        let fast_reg_code = if reg_simple
                            && !uses_env
                            && self.bytecode_mode != BytecodeMode::Off
                        {
                            compile_fast_float_function(&resolved_body).map(Rc::new)
                        }
                        else
                        {
                            None
                        };
                        let func = Value::Function(Rc::new(crate::value::FunctionData {
                            params: params.clone(),
                            body: *resolved_body,
                            declarations: slot_names,
                            param_offset: 0,
                            is_simple: simple,
                            uses_env,
                            code: if compiled { Some(Rc::new(code)) } else { None },
                            reg_code,
                            fast_reg_code,
                            const_pool: Rc::new(const_pool),
                            bound_args: Rc::new(Vec::new()),
                            env: func_env,
                        }));
                        self.env.borrow_mut().define(*name, func.clone());
                        self.env.borrow_mut().mark_function_public(*name);
                        Ok(func)
                    }
                    _ => Err(RuntimeError::simple("@function can only be used with use/import/load, assignments, or function definitions"
                            .to_string(), line)),
                }
            }
            ExprKind::Export { .. } => Err(RuntimeError::simple("export declarations are only valid at the top of module files"
                    .to_string(), line)),
            ExprKind::Load(path) =>
            {
                self.load_wasm_module(path, line)?;
                Ok(Value::Nil)
            }
            ExprKind::Clone(expr) =>
            {
                let val = self.eval(expr, slots)?;
                Ok(clone_value(&val))
            }
            ExprKind::EnvFreeze(expr) =>
            {
                let val = self.eval(expr, slots)?;
                let env = freeze_to_env(&val).map_err(|message| RuntimeError::simple(message, line))?;
                Ok(Value::Env(env))
            }
            ExprKind::Not(expr) =>
            {
                let val = self.eval(expr, slots)?;
                let is_truthy = !matches!(val, Value::Boolean(false) | Value::Nil);
                Ok(Value::Boolean(!is_truthy))
            }
            ExprKind::And { left, right } =>
            {
                let left_val = self.eval(left, slots)?;
                let left_truthy = !matches!(left_val, Value::Boolean(false) | Value::Nil);
                if !left_truthy
                {
                    return Ok(Value::Boolean(false));
                }
                let right_val = self.eval(right, slots)?;
                let right_truthy = !matches!(right_val, Value::Boolean(false) | Value::Nil);
                Ok(Value::Boolean(right_truthy))
            }
            ExprKind::AndBool { left, right } =>
            {
                let left_val = self.eval(left, slots)?;
                let left_bool = match left_val
                {
                    Value::Boolean(value) => value,
                    _ =>
                    {
                        return Err(RuntimeError::simple("&& expects boolean operands".to_string(), line))
                    }
                };
                if !left_bool
                {
                    return Ok(Value::Boolean(false));
                }
                let right_val = self.eval(right, slots)?;
                match right_val
                {
                    Value::Boolean(value) => Ok(Value::Boolean(value)),
                    _ => Err(RuntimeError::simple("&& expects boolean operands".to_string(), line)),
                }
            }
            ExprKind::Or { left, right } =>
            {
                let left_val = self.eval(left, slots)?;
                let left_truthy = !matches!(left_val, Value::Boolean(false) | Value::Nil);
                if left_truthy
                {
                    return Ok(Value::Boolean(true));
                }
                let right_val = self.eval(right, slots)?;
                let right_truthy = !matches!(right_val, Value::Boolean(false) | Value::Nil);
                Ok(Value::Boolean(right_truthy))
            }
            ExprKind::OrBool { left, right } =>
            {
                let left_val = self.eval(left, slots)?;
                let left_bool = match left_val
                {
                    Value::Boolean(value) => value,
                    _ =>
                    {
                        return Err(RuntimeError::simple("|| expects boolean operands".to_string(), line))
                    }
                };
                if left_bool
                {
                    return Ok(Value::Boolean(true));
                }
                let right_val = self.eval(right, slots)?;
                match right_val
                {
                    Value::Boolean(value) => Ok(Value::Boolean(value)),
                    _ => Err(RuntimeError::simple("|| expects boolean operands".to_string(), line)),
                }
            }
            ExprKind::FormatString(parts) =>
            {
                let out = eval_format_parts(self, parts, slots, line)?;
                Ok(Value::String(intern::intern_owned(out)))
            }
            ExprKind::Shell(cmd_str) =>
            {
                let parts = parse_format_parts(cmd_str.as_str(), line)?;
                let cmd = eval_format_parts(self, &parts, slots, line)?;
                let output = if cfg!(target_os = "windows")
                {
                    Command::new("cmd").args(&["/C", cmd.as_str()]).output()
                }
                else
                {
                    Command::new("sh").arg("-c").arg(cmd.as_str()).output()
                };

                match output
                {
                    Ok(o) =>
                    {
                        let res = String::from_utf8_lossy(&o.stdout).to_string();
                        Ok(Value::String(intern::intern_owned(res.trim().to_string())))
                    }
                    Err(_) => Ok(Value::String(intern::intern_owned("".to_string()))),
                }
            }
            ExprKind::Identifier { name, slot } =>
            {
                if let Some(s) = slot
                {
                    if let Some(val) = slots.get(*s)
                    {
                        if let Value::Uninitialized = val
                        {
                            // Fallback to name-based lookup (e.g., for currying)
                        }
                        else
                        {
                            return Ok(val.clone());
                        }
                    }
                }
                let val = self.env.borrow().get(*name).ok_or_else(|| RuntimeError::simple(format!("Undefined variable: {}", symbol_name(*name).as_str()), line))?;
                if let Value::Uninitialized = val
                {
                    return Err(RuntimeError::simple(format!(
                            "Variable '{}' used before assignment",
                            symbol_name(*name).as_str()
                        ), line));
                }
                Ok(val)
            }
            ExprKind::Reference(name) =>
            {
                let val = self
                    .env
                    .borrow_mut()
                    .promote(*name)
                    .ok_or_else(|| RuntimeError::simple(format!(
                            "Undefined variable referenced: {}",
                            symbol_name(*name).as_str()
                        ), line))?;
                Ok(val)
            }
            ExprKind::Assignment { name, value, slot } =>
            {
                let val = self.eval(value, slots)?;
                if let Some(s) = slot
                {
                    if let Value::Reference(new_ref) = &val
                    {
                        if let Some(existing) = slots.get(*s)
                        {
                            if let Value::Reference(old_ref) = existing
                            {
                                if Rc::ptr_eq(old_ref, new_ref)
                                {
                                    return Err(RuntimeError::simple("cannot self alias".to_string(), line));
                                }
                            }
                        }
                    }
                    if let Some(slot_val) = slots.get_mut(*s)
                    {
                        *slot_val = val.clone();
                    }
                }
                else
                {
                    if let Value::Reference(new_ref) = &val
                    {
                        let env_ref = self.env.borrow();
                        let idx = *name as usize;
                        if idx < env_ref.values.len()
                        {
                            if let Value::Reference(old_ref) = &env_ref.values[idx]
                            {
                                if Rc::ptr_eq(old_ref, new_ref)
                                {
                                    return Err(RuntimeError::simple("cannot self alias".to_string(), line));
                                }
                            }
                        }
                    }
                    self.env.borrow_mut().set(*name, val.clone());
                }
                Ok(val)
            }
            ExprKind::IndexAssignment {
                target,
                index,
                value,
            } =>
            {
                let target_val = self.eval(target, slots)?;
                let index_val = self.eval(index, slots)?;
                let val = self.eval(value, slots)?;

                match target_val
                {
                    Value::Array(arr) =>
                    {
                        if let Some(i) = int_value_as_usize(&index_val)
                        {
                            let mut vec = arr.borrow_mut();
                            if i < vec.len()
                            {
                                vec[i] = val.clone();
                            }
                            else
                            {
                                return Err(RuntimeError::simple("Array index out of bounds".to_string(), line));
                            }
                        }
                        else
                        {
                            return Err(RuntimeError::simple(err_index_requires_int().message, line));
                        }
                    }
                    Value::F64Array(arr) =>
                    {
                        if let Some(i) = int_value_as_usize(&index_val)
                        {
                            let mut vec = arr.borrow_mut();
                            if i < vec.len()
                            {
                                match &val
                                {
                                    Value::Float { value, .. } => vec[i] = *value,
                                    v =>
                                    {
                                        if let Some(num) = int_value_as_f64(v)
                                        {
                                            vec[i] = num;
                                        }
                                        else
                                        {
                                            return Err(RuntimeError::simple("F64Array assignment requires a number"
                                                    .to_string(), line));
                                        }
                                    }
                                }
                            }
                            else
                            {
                                return Err(RuntimeError::simple("Array index out of bounds".to_string(), line));
                            }
                        }
                        else
                        {
                            return Err(RuntimeError::simple(err_index_requires_int().message, line));
                        }
                    }
                    Value::F32Array(arr) =>
                    {
                        if let Some(i) = int_value_as_usize(&index_val)
                        {
                            let mut vec = arr.borrow_mut();
                            if i < vec.len()
                            {
                                match &val
                                {
                                    Value::Float { value, .. } => vec[i] = *value as f32,
                                    v =>
                                    {
                                        if let Some(num) = int_value_as_f64(v)
                                        {
                                            vec[i] = num as f32;
                                        }
                                        else
                                        {
                                            return Err(RuntimeError::simple("F32Array assignment requires a number"
                                                    .to_string(), line));
                                        }
                                    }
                                }
                            }
                            else
                            {
                                return Err(RuntimeError::simple("Array index out of bounds".to_string(), line));
                            }
                        }
                        else
                        {
                            return Err(RuntimeError::simple(err_index_requires_int().message, line));
                        }
                    }
                    Value::I64Array(arr) =>
                    {
                        if let Some(i) = int_value_as_usize(&index_val)
                        {
                            let mut vec = arr.borrow_mut();
                            if i < vec.len()
                            {
                                match &val
                                {
                                    Value::Integer { value, .. } => vec[i] = *value as i64,
                                    Value::Unsigned { value, .. } => vec[i] = *value as i64,
                                    _ =>
                                    {
                                        return Err(RuntimeError::simple("I64Array assignment requires an integer"
                                                .to_string(), line));
                                    }
                                }
                            }
                            else
                            {
                                return Err(RuntimeError::simple("Array index out of bounds".to_string(), line));
                            }
                        }
                        else
                        {
                            return Err(RuntimeError::simple(err_index_requires_int().message, line));
                        }
                    }
                    Value::I32Array(arr) =>
                    {
                        if let Some(i) = int_value_as_usize(&index_val)
                        {
                            let mut vec = arr.borrow_mut();
                            if i < vec.len()
                            {
                                match &val
                                {
                                    Value::Integer { value, .. } => vec[i] = *value as i32,
                                    Value::Unsigned { value, .. } => vec[i] = *value as i32,
                                    _ =>
                                    {
                                        return Err(RuntimeError::simple("I32Array assignment requires an integer"
                                                .to_string(), line));
                                    }
                                }
                            }
                            else
                            {
                                return Err(RuntimeError::simple("Array index out of bounds".to_string(), line));
                            }
                        }
                        else
                        {
                            return Err(RuntimeError::simple(err_index_requires_int().message, line));
                        }
                    }
                    Value::Map(map) =>
                    {
                        let key = match index_val
                        {
                            Value::String(s) => s,
                            _ => intern::intern_owned(index_val.inspect()),
                        };
                        let mut map_mut = map.borrow_mut();
                        map_mut.data.insert(key, val.clone());
                        map_mut.version = map_mut.version.wrapping_add(1);
                    }
                    Value::Env(_) =>
                    {
                        return Err(RuntimeError::simple("Env is immutable".to_string(), line));
                    }
                    Value::StructInstance(inst) =>
                    {
                        let key = match index_val
                        {
                            Value::String(s) => s,
                            _ =>
                            {
                                return Err(RuntimeError::simple(err_index_unsupported().message, line));
                            }
                        };
                        let idx = inst.ty.field_map.get(&key).ok_or_else(|| RuntimeError::simple(format!("Unknown field '{}'", key.as_str()), line))?;
                        let field = inst.ty.fields.get(*idx).ok_or_else(|| RuntimeError::simple("Struct field out of bounds".to_string(), line))?;
                        let resolved = resolve_type_ref(&self.env, &field.type_ref, line)?;
                        let coerced =
                            coerce_value_to_type(val.clone(), &resolved, line, key.as_str())?;
                        inst.fields.borrow_mut()[*idx] = coerced.clone();
                    }
                    _ =>
                    {
                        return Err(RuntimeError::simple("Index assignment not supported on this type".to_string(), line));
                    }
                }
                Ok(val)
            }
            ExprKind::FunctionDef {
                name,
                params,
                body,
                slots,
            } =>
            {
                let func_env = self.env.clone();
                let (resolved_body, slot_names) = if let Some(slot_names) = slots
                {
                    (body.clone(), slot_names.clone())
                }
                else
                {
                    let mut locals = HashSet::new();
                    collect_declarations(body, &mut locals);
                    let (slot_map, slot_names) = build_slot_map(params, locals);
                    let mut resolved = body.clone();
                    resolve(resolved.as_mut(), &slot_map);
                    (resolved, Rc::new(slot_names))
                };
                let simple = is_simple(&resolved_body);
                let uses_env = uses_environment(&resolved_body);
                let reg_simple = is_reg_simple(&resolved_body);

                let mut code = Vec::new();
                let mut const_pool = Vec::new();
                let compiled = if should_compile(simple, uses_env, self.bytecode_mode)
                {
                    let use_caches = self.bytecode_mode == BytecodeMode::Advanced;
                    with_compile_use_caches(use_caches, || {
                        compile_expr(&resolved_body, &mut code, &mut const_pool, true)
                    })
                }
                else
                {
                    false
                };

                let reg_code = if reg_simple && !uses_env && self.bytecode_mode != BytecodeMode::Off
                {
                    compile_reg_function(&resolved_body).map(Rc::new)
                }
                else
                {
                    None
                };
                let fast_reg_code =
                    if reg_simple && !uses_env && self.bytecode_mode != BytecodeMode::Off
                    {
                        compile_fast_float_function(&resolved_body).map(Rc::new)
                    }
                    else
                    {
                        None
                    };
                let func = Value::Function(Rc::new(crate::value::FunctionData {
                    params: params.clone(),
                    body: *resolved_body,
                    declarations: slot_names,
                    param_offset: 0,
                    is_simple: simple,
                    uses_env,
                    code: if compiled { Some(Rc::new(code)) } else { None },
                    reg_code,
                    fast_reg_code,
                    const_pool: Rc::new(const_pool),
                    bound_args: Rc::new(Vec::new()),
                    env: func_env,
                }));
                self.env.borrow_mut().define(*name, func.clone());
                Ok(func)
            }
            ExprKind::AnonymousFunction {
                params,
                body,
                slots,
            } =>
            {
                let func_env = self.env.clone();
                let (resolved_body, slot_names) = if let Some(slot_names) = slots
                {
                    (body.clone(), slot_names.clone())
                }
                else
                {
                    let mut locals = HashSet::new();
                    collect_declarations(body, &mut locals);
                    let (slot_map, slot_names) = build_slot_map(params, locals);
                    let mut resolved = body.clone();
                    resolve(resolved.as_mut(), &slot_map);
                    (resolved, Rc::new(slot_names))
                };
                let simple = is_simple(&resolved_body);
                let uses_env = uses_environment(&resolved_body);
                let reg_simple = is_reg_simple(&resolved_body);

                let mut code = Vec::new();
                let mut const_pool = Vec::new();
                let compiled = if should_compile(simple, uses_env, self.bytecode_mode)
                {
                    let use_caches = self.bytecode_mode == BytecodeMode::Advanced;
                    with_compile_use_caches(use_caches, || {
                        compile_expr(&resolved_body, &mut code, &mut const_pool, true)
                    })
                }
                else
                {
                    false
                };

                let reg_code = if reg_simple && !uses_env && self.bytecode_mode != BytecodeMode::Off
                {
                    compile_reg_function(&resolved_body).map(Rc::new)
                }
                else
                {
                    None
                };
                let fast_reg_code =
                    if reg_simple && !uses_env && self.bytecode_mode != BytecodeMode::Off
                    {
                        compile_fast_float_function(&resolved_body).map(Rc::new)
                    }
                    else
                    {
                        None
                    };
                Ok(Value::Function(Rc::new(crate::value::FunctionData {
                    params: params.clone(),
                    body: *resolved_body,
                    declarations: slot_names,
                    param_offset: 0,
                    is_simple: simple,
                    uses_env,
                    code: if compiled { Some(Rc::new(code)) } else { None },
                    reg_code,
                    fast_reg_code,
                    const_pool: Rc::new(const_pool),
                    bound_args: Rc::new(Vec::new()),
                    env: func_env,
                })))
            }
            ExprKind::MethodDef {
                type_name,
                name,
                params,
                body,
                slots,
            } =>
            {
                if params.is_empty()
                {
                    return Err(RuntimeError::simple("Method must take self as first parameter".to_string(), line));
                }
                let self_name = symbol_name(params[0].name);
                if self_name.as_str() != "self"
                {
                    return Err(RuntimeError::simple("Method must take self as first parameter".to_string(), line));
                }
                let type_val = self
                    .env
                    .borrow()
                    .get(*type_name)
                    .ok_or_else(|| RuntimeError::simple(format!("Unknown struct '{}'", symbol_name(*type_name).as_str()), line))?;
                let ty = match type_val
                {
                    Value::StructType(ty) => ty,
                    _ =>
                    {
                        return Err(RuntimeError::simple(format!(
                                "'{}' is not a struct type",
                                symbol_name(*type_name).as_str()
                            ), line));
                    }
                };
                let method_key = symbol_name(*name);
                if ty.field_map.contains_key(&method_key)
                {
                    return Err(RuntimeError::simple(format!(
                            "Method '{}' conflicts with field on {}",
                            method_key.as_str(),
                            ty.name
                        ), line));
                }
                let func_env = self.env.clone();
                let (resolved_body, slot_names) = if let Some(slot_names) = slots
                {
                    (body.clone(), slot_names.clone())
                }
                else
                {
                    let mut locals = HashSet::new();
                    collect_declarations(body, &mut locals);
                    let (slot_map, slot_names) = build_slot_map(params, locals);
                    let mut resolved = body.clone();
                    resolve(resolved.as_mut(), &slot_map);
                    (resolved, Rc::new(slot_names))
                };
                let simple = is_simple(&resolved_body);
                let uses_env = uses_environment(&resolved_body);
                let reg_simple = is_reg_simple(&resolved_body);

                let mut code = Vec::new();
                let mut const_pool = Vec::new();
                let compiled = if should_compile(simple, uses_env, self.bytecode_mode)
                {
                    let use_caches = self.bytecode_mode == BytecodeMode::Advanced;
                    with_compile_use_caches(use_caches, || {
                        compile_expr(&resolved_body, &mut code, &mut const_pool, true)
                    })
                }
                else
                {
                    false
                };

                let reg_code = if reg_simple && !uses_env && self.bytecode_mode != BytecodeMode::Off
                {
                    compile_reg_function(&resolved_body).map(Rc::new)
                }
                else
                {
                    None
                };
                let fast_reg_code =
                    if reg_simple && !uses_env && self.bytecode_mode != BytecodeMode::Off
                    {
                        compile_fast_float_function(&resolved_body).map(Rc::new)
                    }
                    else
                    {
                        None
                    };
                let func = Value::Function(Rc::new(crate::value::FunctionData {
                    params: params.clone(),
                    body: *resolved_body,
                    declarations: slot_names,
                    param_offset: 0,
                    is_simple: simple,
                    uses_env,
                    code: if compiled { Some(Rc::new(code)) } else { None },
                    reg_code,
                    fast_reg_code,
                    const_pool: Rc::new(const_pool),
                    bound_args: Rc::new(Vec::new()),
                    env: func_env,
                }));
                ty.methods.borrow_mut().insert(method_key, func.clone());
                Ok(func)
            }
            ExprKind::Yield(args) =>
            {
                let block_data = self.block_stack.last().cloned();

                if let Some(Some((closure, saved_env))) = block_data
                {
                    let mut arg_vals = Vec::new();
                    for a in args
                    {
                        arg_vals.push(self.eval(a, slots)?);
                    }
                    self.call_block_with_args(&closure, saved_env, &arg_vals, line)
                }
                else
                {
                    Err(RuntimeError::simple("No block given for yield".to_string(), line))
                }
            }
            ExprKind::Return(expr) =>
            {
                let value = if let Some(e) = expr
                {
                    self.eval(e, slots)?
                }
                else
                {
                    Value::Nil
                };
                Err(make_early_return_error(value))
            }
            ExprKind::ErrorRaise(inner) =>
            {
                let value = self.eval(inner, slots)?;
                if let Some(err) = runtime_error_from_value(&value)
                {
                    Err(RuntimeError::wrap(
                        err,
                        expr.line,
                        expr.column,
                        expr.source.clone(),
                    ))
                }
                else
                {
                    Err(RuntimeError::from_expr(value.to_string(), expr))
                }
            }
            ExprKind::Result {
                body,
                else_expr,
                else_binding,
                else_slot,
            } =>
            {
                match self.eval(body, slots)
                {
                    Ok(value) => Ok(value),
                    Err(err) if is_early_return(&err) => Err(err),
                    Err(err) =>
                    {
                        let err_val = runtime_error_to_value(&err);
                        let mut saved_slot = None;
                        let mut saved_local = None;

                        if let Some(slot) = else_slot
                        {
                            if let Some(slot_val) = slots.get_mut(*slot)
                            {
                                saved_slot = Some(slot_val.clone());
                                *slot_val = err_val.clone();
                            }
                        }
                        else if let Some(name) = else_binding
                        {
                            let idx = *name as usize;
                            let mut env = self.env.borrow_mut();
                            if idx < env.values.len()
                            {
                                if !matches!(env.values[idx], Value::Uninitialized)
                                {
                                    saved_local = Some(env.values[idx].clone());
                                }
                            }
                            if idx >= env.values.len()
                            {
                                env.values.resize(idx + 1, Value::Uninitialized);
                            }
                            env.values[idx] = err_val.clone();
                        }

                        let result = self.eval(else_expr, slots);

                        if let Some(slot) = else_slot
                        {
                            if let Some(slot_val) = slots.get_mut(*slot)
                            {
                                if let Some(saved) = saved_slot
                                {
                                    *slot_val = saved;
                                }
                            }
                        }
                        else if let Some(name) = else_binding
                        {
                            let idx = *name as usize;
                            let mut env = self.env.borrow_mut();
                            if idx >= env.values.len()
                            {
                                env.values.resize(idx + 1, Value::Uninitialized);
                            }
                            if let Some(saved) = saved_local
                            {
                                env.values[idx] = saved;
                            }
                            else
                            {
                                env.values[idx] = Value::Uninitialized;
                            }
                        }

                        result
                    }
                }
            }
            ExprKind::Array(elements) =>
            {
                // Empty array is always Array type
                if elements.is_empty()
                {
                    return Ok(Value::Array(Rc::new(RefCell::new(Vec::new()))));
                }

                let mut vals = Vec::new();
                let mut i32_vals: Vec<i32> = Vec::new();
                let mut i64_vals: Vec<i64> = Vec::new();
                let mut f32_vals: Vec<f32> = Vec::new();
                let mut f64_vals: Vec<f64> = Vec::new();
                let mut all_i32 = true;
                let mut all_i64 = true;
                let mut all_f32 = true;
                let mut all_f64 = true;

                for e in elements
                {
                    let v = self.eval(e, slots)?;
                    if all_i32
                    {
                        match &v
                        {
                            Value::Integer {
                                value,
                                kind: IntKind::I32,
                            } =>
                            {
                                i32_vals.push(*value as i32);
                            }
                            Value::Unsigned {
                                value,
                                kind: IntKind::U32,
                            } =>
                            {
                                i32_vals.push(*value as i32);
                            }
                            _ =>
                            {
                                all_i32 = false;
                            }
                        }
                    }

                    if all_f32
                    {
                        match &v
                        {
                            Value::Float {
                                value,
                                kind: FloatKind::F32,
                            } =>
                            {
                                f32_vals.push(*value as f32);
                            }
                            _ =>
                            {
                                all_f32 = false;
                            }
                        }
                    }

                    if all_i64
                    {
                        match &v
                        {
                            Value::Integer { value, .. } =>
                            {
                                i64_vals.push(*value as i64);
                                if all_f64
                                {
                                    f64_vals.push(*value as f64);
                                }
                                continue;
                            }
                            Value::Unsigned { value, .. } =>
                            {
                                i64_vals.push(*value as i64);
                                if all_f64
                                {
                                    f64_vals.push(*value as f64);
                                }
                                continue;
                            }
                            _ =>
                            {
                                all_i64 = false;
                                // Convert i64_vals to general vals
                                vals.extend(
                                    i64_vals
                                        .drain(..)
                                        .map(|value| make_signed_int(value as i128, IntKind::I64)),
                                );
                            }
                        }
                    }

                    if all_f64
                    {
                        match &v
                        {
                            Value::Float { value, .. } =>
                            {
                                f64_vals.push(*value);
                                continue;
                            }
                            _ =>
                            {
                                if let Some(num) = int_value_as_f64(&v)
                                {
                                    f64_vals.push(num);
                                    vals.push(v);
                                    continue;
                                }
                                else
                                {
                                    all_f64 = false;
                                    f64_vals.clear();
                                    vals.push(v);
                                }
                            }
                        }
                    }
                    else
                    {
                        vals.push(v);
                    }
                }

                if all_i32
                {
                    Ok(Value::I32Array(Rc::new(RefCell::new(i32_vals))))
                }
                else if all_i64
                {
                    Ok(Value::I64Array(Rc::new(RefCell::new(i64_vals))))
                }
                else if all_f32
                {
                    Ok(Value::F32Array(Rc::new(RefCell::new(f32_vals))))
                }
                else if all_f64
                {
                    Ok(Value::F64Array(Rc::new(RefCell::new(f64_vals))))
                }
                else
                {
                    Ok(Value::Array(Rc::new(RefCell::new(vals))))
                }
            }
            ExprKind::StructDef { name, fields } =>
            {
                let struct_name = symbol_name(*name);
                let mut field_map = FxHashMap::default();
                let mut field_defs = Vec::new();
                for (idx, (field_name, type_ref)) in fields.iter().enumerate()
                {
                    let field_str = symbol_name(*field_name);
                    if field_map.contains_key(&field_str)
                    {
                        return Err(RuntimeError::simple(format!("Duplicate field '{}'", field_str.as_str()), line));
                    }
                    field_map.insert(field_str.clone(), idx);
                    field_defs.push(crate::value::StructField {
                        name: field_str,
                        type_ref: type_ref.clone(),
                    });
                }
                let ty = StructType {
                    name: struct_name.clone(),
                    fields: field_defs,
                    field_map,
                    methods: RefCell::new(FxHashMap::default()),
                };
                let value = Value::StructType(Rc::new(ty));
                self.env.borrow_mut().define(*name, value.clone());
                Ok(value)
            }
            ExprKind::StructLiteral { name, fields } =>
            {
                let type_val = self.env.borrow().get(*name).ok_or_else(|| RuntimeError::simple(format!("Unknown struct '{}'", symbol_name(*name).as_str()), line))?;
                let ty = match type_val
                {
                    Value::StructType(ty) => ty,
                    _ =>
                    {
                        return Err(RuntimeError::simple(format!(
                                "'{}' is not a struct type",
                                symbol_name(*name).as_str()
                            ), line));
                    }
                };
                let mut field_values = FxHashMap::default();
                for (field_name, expr) in fields
                {
                    let val = self.eval(expr, slots)?;
                    let field_str = symbol_name(*field_name);
                    if !ty.field_map.contains_key(&field_str)
                    {
                        return Err(RuntimeError::simple(format!(
                                "Unknown field '{}' for {}",
                                field_str.as_str(),
                                ty.name
                            ), line));
                    }
                    field_values.insert(field_str, val);
                }
                let mut values = Vec::with_capacity(ty.fields.len());
                for field in &ty.fields
                {
                    let val = field_values
                        .remove(&field.name)
                        .ok_or_else(|| RuntimeError::simple(format!(
                                "Missing field '{}' for {}",
                                field.name.as_str(),
                                ty.name
                            ), line))?;
                    let resolved = resolve_type_ref(&self.env, &field.type_ref, line)?;
                    let coerced = coerce_value_to_type(val, &resolved, line, field.name.as_str())?;
                    values.push(coerced);
                }
                let inst = crate::value::StructInstance {
                    ty: ty.clone(),
                    fields: RefCell::new(values),
                };
                Ok(Value::StructInstance(Rc::new(inst)))
            }
            ExprKind::ArrayGenerator { generator, size } =>
            {
                let gen_val = self.eval(generator, slots)?;
                let size_val = self.eval(size, slots)?;
                let n = int_value_as_usize(&size_val).ok_or_else(|| RuntimeError::simple("Array size must be a non-negative integer".to_string(), line))?;
                let mut vals: Vec<Value> = Vec::with_capacity(n);
                if let Value::Function(data) = gen_val
                {
                    for i in 0..n
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
                        if !data.params.is_empty()
                        {
                            new_slots[data.param_offset] = default_int(i as i128);
                        }

                        let result = if let Some(code) = &data.code
                        {
                            execute_instructions(self, code, &data.const_pool, &mut new_slots)?
                        }
                        else if data.uses_env
                        {
                            let new_env = self.get_env(Some(data.env.clone()), false);
                            let original_env = self.env.clone();
                            self.env = new_env.clone();
                            let result = self.eval(&data.body, &mut new_slots)?;
                            self.env = original_env;
                            self.recycle_env(new_env);
                            result
                        }
                        else
                        {
                            self.eval(&data.body, &mut new_slots)?
                        };
                        vals.push(result);
                    }
                }
                else
                {
                    for _ in 0..n
                    {
                        vals.push(gen_val.clone());
                    }
                }
                if vals.is_empty()
                {
                    return Ok(Value::Array(Rc::new(RefCell::new(Vec::new()))));
                }
                let mut i32_vals: Vec<i32> = Vec::new();
                let mut i64_vals: Vec<i64> = Vec::new();
                let mut f32_vals: Vec<f32> = Vec::new();
                let mut f64_vals: Vec<f64> = Vec::new();
                let mut all_i32 = true;
                let mut all_i64 = true;
                let mut all_f32 = true;
                let mut all_f64 = true;
                for v in &vals
                {
                    if all_i32
                    {
                        match v
                        {
                            Value::Integer {
                                value,
                                kind: IntKind::I32,
                            } =>
                            {
                                i32_vals.push(*value as i32);
                            }
                            Value::Unsigned {
                                value,
                                kind: IntKind::U32,
                            } =>
                            {
                                i32_vals.push(*value as i32);
                            }
                            _ =>
                            {
                                all_i32 = false;
                            }
                        }
                    }
                    if all_f32
                    {
                        match v
                        {
                            Value::Float {
                                value,
                                kind: FloatKind::F32,
                            } =>
                            {
                                f32_vals.push(*value as f32);
                            }
                            _ =>
                            {
                                all_f32 = false;
                            }
                        }
                    }
                    if all_i64
                    {
                        match v
                        {
                            Value::Integer { value, .. } =>
                            {
                                i64_vals.push(*value as i64);
                                if all_f64
                                {
                                    f64_vals.push(*value as f64);
                                }
                                continue;
                            }
                            Value::Unsigned { value, .. } =>
                            {
                                i64_vals.push(*value as i64);
                                if all_f64
                                {
                                    f64_vals.push(*value as f64);
                                }
                                continue;
                            }
                            _ =>
                            {
                                all_i64 = false;
                            }
                        }
                    }
                    if all_f64
                    {
                        match v
                        {
                            Value::Float { value, .. } =>
                            {
                                f64_vals.push(*value);
                                continue;
                            }
                            _ =>
                            {
                                if let Some(num) = int_value_as_f64(v)
                                {
                                    f64_vals.push(num);
                                    continue;
                                }
                                all_f64 = false;
                            }
                        }
                    }
                }
                if all_i32
                {
                    Ok(Value::I32Array(Rc::new(RefCell::new(i32_vals))))
                }
                else if all_i64
                {
                    Ok(Value::I64Array(Rc::new(RefCell::new(i64_vals))))
                }
                else if all_f32
                {
                    Ok(Value::F32Array(Rc::new(RefCell::new(f32_vals))))
                }
                else if all_f64
                {
                    Ok(Value::F64Array(Rc::new(RefCell::new(f64_vals))))
                }
                else
                {
                    Ok(Value::Array(Rc::new(RefCell::new(vals))))
                }
            }
            ExprKind::Map(entries) =>
            {
                let mut map = FxHashMap::default();
                for (k_expr, v_expr) in entries
                {
                    let k_val = self.eval(k_expr, slots)?;
                    let v_val = self.eval(v_expr, slots)?;
                    let k_str = match k_val
                    {
                        Value::String(s) => s,
                        _ => intern::intern_owned(k_val.inspect()),
                    };
                    map.insert(k_str, v_val);
                }
                Ok(Value::Map(Rc::new(RefCell::new(MapValue::new(map)))))
            }
            ExprKind::Index { target, index } =>
            {
                let target_val = self.eval(target, slots)?;
                let index_val = self.eval(index, slots)?;
                match target_val
                {
                    Value::StructInstance(inst) =>
                    {
                        if let Value::String(s) = index_val
                        {
                            if let Some(idx) = inst.ty.field_map.get(&s)
                            {
                                let fields = inst.fields.borrow();
                                Ok(fields.get(*idx).cloned().unwrap_or(Value::Nil))
                            }
                            else if let Some(method) = inst.ty.methods.borrow().get(&s).cloned()
                            {
                                Ok(Value::BoundMethod(Rc::new(BoundMethod {
                                    receiver: Value::StructInstance(inst.clone()),
                                    func: method,
                                })))
                            }
                            else
                            {
                                Ok(Value::Nil)
                            }
                        }
                        else
                        {
                            Err(RuntimeError::simple(err_index_unsupported().message, line))
                        }
                    }
                    Value::StructType(ty) =>
                    {
                        if let Value::String(s) = index_val
                        {
                            Ok(ty.methods.borrow().get(&s).cloned().unwrap_or(Value::Nil))
                        }
                        else
                        {
                            Err(RuntimeError::simple(err_index_unsupported().message, line))
                        }
                    }
                    Value::Array(arr) =>
                    {
                        if let Some(i) = int_value_as_usize(&index_val)
                        {
                            let vec = arr.borrow();
                            if i < vec.len()
                            {
                                Ok(vec[i].clone())
                            }
                            else
                            {
                                Ok(Value::Nil)
                            }
                        }
                        else
                        {
                            Err(RuntimeError::simple(err_index_requires_int().message, line))
                        }
                    }
                    Value::F64Array(arr) =>
                    {
                        if let Some(i) = int_value_as_usize(&index_val)
                        {
                            let vec = arr.borrow();
                            if i < vec.len()
                            {
                                Ok(make_float(vec[i], FloatKind::F64))
                            }
                            else
                            {
                                Ok(Value::Nil)
                            }
                        }
                        else
                        {
                            Err(RuntimeError::simple(err_index_requires_int().message, line))
                        }
                    }
                    Value::F32Array(arr) =>
                    {
                        if let Some(i) = int_value_as_usize(&index_val)
                        {
                            let vec = arr.borrow();
                            if i < vec.len()
                            {
                                Ok(make_float(vec[i] as f64, FloatKind::F32))
                            }
                            else
                            {
                                Ok(Value::Nil)
                            }
                        }
                        else
                        {
                            Err(RuntimeError::simple(err_index_requires_int().message, line))
                        }
                    }
                    Value::I64Array(arr) =>
                    {
                        if let Some(i) = int_value_as_usize(&index_val)
                        {
                            let vec = arr.borrow();
                            if i < vec.len()
                            {
                                Ok(make_signed_int(vec[i] as i128, IntKind::I64))
                            }
                            else
                            {
                                Ok(Value::Nil)
                            }
                        }
                        else
                        {
                            Err(RuntimeError::simple(err_index_requires_int().message, line))
                        }
                    }
                    Value::I32Array(arr) =>
                    {
                        if let Some(i) = int_value_as_usize(&index_val)
                        {
                            let vec = arr.borrow();
                            if i < vec.len()
                            {
                                Ok(make_signed_int(vec[i] as i128, IntKind::I32))
                            }
                            else
                            {
                                Ok(Value::Nil)
                            }
                        }
                        else
                        {
                            Err(RuntimeError::simple(err_index_requires_int().message, line))
                        }
                    }
                    Value::Map(map) =>
                    {
                        if let Value::String(s) = index_val
                        {
                            if s.as_str() == "keys"
                            {
                                Ok(map_keys_array(&map.borrow()))
                            }
                            else if s.as_str() == "values"
                            {
                                Ok(map_values_array(&map.borrow()))
                            }
                            else
                            {
                                Ok(map.borrow().data.get(&s).cloned().unwrap_or(Value::Nil))
                            }
                        }
                        else
                        {
                            let key = intern::intern_owned(index_val.inspect());
                            Ok(map.borrow().data.get(&key).cloned().unwrap_or(Value::Nil))
                        }
                    }
                    Value::Env(env) =>
                    {
                        if let Value::String(s) = index_val
                        {
                            if s.as_str() == "keys"
                            {
                                Ok(env_keys_array(env.as_ref()))
                            }
                            else if s.as_str() == "values"
                            {
                                Ok(env_values_array(env.as_ref()))
                            }
                            else
                            {
                                Ok(env.data.get(&s).map(env_clone_value).unwrap_or(Value::Nil))
                            }
                        }
                        else
                        {
                            let key = intern::intern_owned(index_val.inspect());
                            Ok(env.data.get(&key).map(env_clone_value).unwrap_or(Value::Nil))
                        }
                    }
                    _ => Err(RuntimeError::simple(err_index_unsupported().message, line)),
                }
            }
            ExprKind::Slice { target, start, end } =>
            {
                let target_val = self.eval(target, slots)?;
                let start_val = self.eval(start, slots)?;
                let end_val = self.eval(end, slots)?;

                let start_idx = int_value_as_usize(&start_val).ok_or_else(|| RuntimeError::simple("Slice start index must be an integer".to_string(), line))?;
                let end_idx = int_value_as_usize(&end_val).ok_or_else(|| RuntimeError::simple("Slice end index must be an integer".to_string(), line))?;

                match target_val
                {
                    Value::String(s) =>
                    {
                        let chars: Vec<char> = s.chars().collect();
                        let len = chars.len();
                        let start_clamped = start_idx.min(len);
                        let end_clamped = end_idx.min(len);
                        if start_clamped >= end_clamped
                        {
                            Ok(Value::String(intern::intern("")))
                        }
                        else
                        {
                            let slice: String = chars[start_clamped..end_clamped].iter().collect();
                            Ok(Value::String(intern::intern_owned(slice)))
                        }
                    }
                    Value::Array(arr) =>
                    {
                        let vec = arr.borrow();
                        let len = vec.len();
                        let start_clamped = start_idx.min(len);
                        let end_clamped = end_idx.min(len);
                        if start_clamped >= end_clamped
                        {
                            Ok(Value::Array(Rc::new(RefCell::new(Vec::new()))))
                        }
                        else
                        {
                            let slice: Vec<Value> = vec[start_clamped..end_clamped].to_vec();
                            Ok(Value::Array(Rc::new(RefCell::new(slice))))
                        }
                    }
                    Value::F64Array(arr) =>
                    {
                        let vec = arr.borrow();
                        let len = vec.len();
                        let start_clamped = start_idx.min(len);
                        let end_clamped = end_idx.min(len);
                        if start_clamped >= end_clamped
                        {
                            Ok(Value::F64Array(Rc::new(RefCell::new(Vec::new()))))
                        }
                        else
                        {
                            let slice: Vec<f64> = vec[start_clamped..end_clamped].to_vec();
                            Ok(Value::F64Array(Rc::new(RefCell::new(slice))))
                        }
                    }
                    _ => Err(RuntimeError::simple("Slice is only supported for strings and arrays".to_string(), line)),
                }
            }
            ExprKind::Call {
                function,
                args,
                block,
                inlined_body,
            } =>
            {
                // 1. Check Cached Inlined Body
                if let Some(inlined) = inlined_body.borrow().as_ref()
                {
                    return self.eval(inlined, slots);
                }

                if let Some(block) = block.as_ref()
                {
                    if args.is_empty()
                    {
                        if let ExprKind::Index { target, index } = &function.kind
                        {
                            let target_val = self.eval(target, slots)?;
                            let index_val = self.eval(index, slots)?;
                            if let Value::String(name) = index_val
                            {
                                if matches!(name.as_str(), "each" | "apply" | "map" | "filter")
                                {
                                    let method = name.as_str();
                                    let saved_env = self.env.clone();
                                    let target = match target_val
                                    {
                                        Value::Array(arr) =>
                                        {
                                            Some(BlockCollectionTarget::Array(arr))
                                        }
                                        Value::F32Array(arr) =>
                                        {
                                            Some(BlockCollectionTarget::F32Array(arr))
                                        }
                                        Value::F64Array(arr) =>
                                        {
                                            Some(BlockCollectionTarget::F64Array(arr))
                                        }
                                        Value::I32Array(arr) =>
                                        {
                                            Some(BlockCollectionTarget::I32Array(arr))
                                        }
                                        Value::I64Array(arr) =>
                                        {
                                            Some(BlockCollectionTarget::I64Array(arr))
                                        }
                                        Value::Map(map) => Some(BlockCollectionTarget::Map(map)),
                                        _ => None,
                                    };
                                    if let Some(target) = target
                                    {
                                        return self.apply_block_collection(
                                            method, target, block, saved_env, line,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }

                if let ExprKind::Identifier { name, .. } = &function.kind
                {
                    match symbol_name(*name).as_str()
                    {
                        "puts" | "print" =>
                        {
                            let mut last_val = Value::Nil;
                            for arg in args
                            {
                                let val = self.eval(arg, slots)?;
                                if symbol_name(*name).as_str() == "puts"
                                {
                                    println!("{}", val);
                                }
                                else
                                {
                                    print!("{}", val);
                                    io::stdout().flush().unwrap();
                                }
                                last_val = val;
                            }
                            return Ok(last_val);
                        }
                        "len" =>
                        {
                            let val = self.eval(&args[0], slots)?;
                            return match val
                            {
                                Value::String(s) => Ok(default_int(s.len() as i128)),
                                Value::Array(arr) => Ok(default_int(arr.borrow().len() as i128)),
                                Value::F64Array(arr) => Ok(default_int(arr.borrow().len() as i128)),
                                Value::Bytes(bytes) => Ok(default_int(bytes.len() as i128)),
                                Value::ByteBuf(buf) => Ok(default_int(buf.borrow().len() as i128)),
                                Value::BytesView(view) => Ok(default_int(view.len as i128)),
                                Value::Map(map) => Ok(default_int(map.borrow().data.len() as i128)),
                                Value::Env(env) => Ok(default_int(env.data.len() as i128)),
                                Value::Mmap(mmap) => Ok(default_int(mmap.len() as i128)),
                                Value::MmapMut(mmap) =>
                                {
                                    Ok(default_int(mmap.borrow().len() as i128))
                                }
                                _ => Ok(default_int(0)),
                            };
                        }
                        "read_file" =>
                        {
                            let path = self.eval(&args[0], slots)?.to_string();
                            return match fs::read_to_string(&path)
                            {
                                Ok(content) => Ok(Value::String(intern::intern_owned(content))),
                                Err(_) => Ok(Value::Nil),
                            };
                        }
                        "write_file" =>
                        {
                            let path = self.eval(&args[0], slots)?.to_string();
                            let content = self.eval(&args[1], slots)?.to_string();
                            return match fs::File::create(&path)
                            {
                                Ok(mut file) =>
                                {
                                    write!(file, "{}", content).unwrap();
                                    Ok(Value::Boolean(true))
                                }
                                Err(_) => Ok(Value::Boolean(false)),
                            };
                        }
                        "typeof" =>
                        {
                            let val = self.eval(&args[0], slots)?;
                            return self.call_builtin(&Builtin::Typeof, &[val]);
                        }
                        "f64" =>
                        {
                            let val = self.eval(&args[0], slots)?;
                            return cast_f64_value(&val)
                                .map_err(|message| RuntimeError::simple(message, line));
                        }
                        "f32" =>
                        {
                            let val = self.eval(&args[0], slots)?;
                            return cast_f32_value(&val)
                                .map_err(|message| RuntimeError::simple(message, line));
                        }
                        "i64" =>
                        {
                            let val = self.eval(&args[0], slots)?;
                            return cast_i64_value(&val)
                                .map_err(|message| RuntimeError::simple(message, line));
                        }
                        "i32" =>
                        {
                            let val = self.eval(&args[0], slots)?;
                            return cast_i32_value(&val)
                                .map_err(|message| RuntimeError::simple(message, line));
                        }
                        "u64" =>
                        {
                            let val = self.eval(&args[0], slots)?;
                            return cast_u64_value(&val)
                                .map_err(|message| RuntimeError::simple(message, line));
                        }
                        "u32" =>
                        {
                            let val = self.eval(&args[0], slots)?;
                            return cast_u32_value(&val)
                                .map_err(|message| RuntimeError::simple(message, line));
                        }
                        _ =>
                        {}
                    }
                }

                let func_val = self.eval(function, slots)?;
                match func_val
                {
                    Value::Function(data) =>
                    {
                        // 2. Attempt JIT Inlining
                        // Inline if:
                        // - Function is simple (no locals/assignments).
                        // - Function does not capture environment (no slot=None identifiers).
                        // - Args are simple expressions (Identifiers/Literals) to avoid code explosion or side-effect duplication.
                        if data.is_simple
                            && inlined_body.borrow().is_none()
                            && !data.uses_env
                            && args.len() == data.params.len()
                        {
                            let small_body = expr_size(&data.body) <= 40;
                            let safe_args = args.iter().all(is_inline_safe_arg);
                            if safe_args && small_body
                            {
                                let inlined = substitute(&data.body, args);
                                inlined_body.replace(Some(inlined));
                                // Run the newly minted inlined body immediately
                                return self.eval(inlined_body.borrow().as_ref().unwrap(), slots);
                            }
                        }

                        let mut arg_vals: smallvec::SmallVec<[Value; 8]> =
                            smallvec::SmallVec::new();
                        for (i, arg_expr) in args.iter().enumerate()
                        {
                            let val = self.eval(arg_expr, slots)?;
                            if i < data.params.len()
                            {
                                if data.params[i].is_ref
                                {
                                    if let Value::Reference(_) = val
                                    {
                                        arg_vals.push(val);
                                    }
                                    else
                                    {
                                        return Err(RuntimeError::simple(format!(
                                                "Argument #{} expected to be a reference (&var), but got value",
                                                i + 1
                                            ), line));
                                    }
                                }
                                else
                                {
                                    arg_vals.push(val);
                                }
                            }
                            else
                            {
                                arg_vals.push(val);
                            }
                        }

                        self.invoke_function(data, arg_vals, line, block.clone().map(Rc::new))
                    }
                    Value::NativeFunction(func) =>
                    {
                        if block.is_some()
                        {
                            return Err(RuntimeError::simple("Native function does not accept a block".to_string(), line));
                        }
                        let mut arg_vals: smallvec::SmallVec<[Value; 8]> =
                            smallvec::SmallVec::new();
                        for arg_expr in args
                        {
                            arg_vals.push(self.eval(arg_expr, slots)?);
                        }
                        func(&arg_vals).map_err(|message| RuntimeError::simple(message, line))
                    }
                    Value::HostFunction(func) =>
                    {
                        if block.is_some()
                        {
                            return Err(RuntimeError::simple("Host function does not accept a block".to_string(), line));
                        }
                        let mut arg_vals: smallvec::SmallVec<[Value; 8]> =
                            smallvec::SmallVec::new();
                        for arg_expr in args
                        {
                            arg_vals.push(self.eval(arg_expr, slots)?);
                        }
                        func(self, &arg_vals).map_err(|message| RuntimeError::simple(message, line))
                    }
                    Value::WasmFunction(func) =>
                    {
                        if block.is_some()
                        {
                            return Err(RuntimeError::simple("Wasm function does not accept a block".to_string(), line));
                        }
                        let mut arg_vals: smallvec::SmallVec<[Value; 8]> =
                            smallvec::SmallVec::new();
                        for arg_expr in args
                        {
                            arg_vals.push(self.eval(arg_expr, slots)?);
                        }
                        self.call_wasm_function(func, arg_vals, line)
                    }
                    Value::BoundMethod(method) =>
                    {
                        let mut arg_vals: smallvec::SmallVec<[Value; 8]> =
                            smallvec::SmallVec::new();
                        arg_vals.push(method.receiver.clone());
                        for arg_expr in args
                        {
                            arg_vals.push(self.eval(arg_expr, slots)?);
                        }
                        self.call_value(
                            method.func.clone(),
                            arg_vals,
                            line,
                            block.clone().map(Rc::new),
                        )
                    }
                    _ => Err(RuntimeError::simple(format!("Tried to call a non-function value: {}", func_val), line)),
                }
            }
            ExprKind::BinaryOp { left, op, right } =>
            {
                let l = match &left.kind
                {
                    ExprKind::Integer { value, kind } => make_signed_int(*value, *kind),
                    ExprKind::Unsigned { value, kind } => make_unsigned_int(*value, *kind),
                    ExprKind::Float { value, kind } => make_float(*value, *kind),
                    ExprKind::Identifier { slot: Some(s), .. } =>
                    {
                        if let Some(v) = slots.get(*s)
                        {
                            if let Value::Uninitialized = v
                            {
                                self.eval(left, slots)?
                            }
                            else
                            {
                                v.clone()
                            }
                        }
                        else
                        {
                            self.eval(left, slots)?
                        }
                    }
                    _ => self.eval(left, slots)?,
                };
                let r = match &right.kind
                {
                    ExprKind::Integer { value, kind } => make_signed_int(*value, *kind),
                    ExprKind::Unsigned { value, kind } => make_unsigned_int(*value, *kind),
                    ExprKind::Float { value, kind } => make_float(*value, *kind),
                    ExprKind::Identifier { slot: Some(s), .. } =>
                    {
                        if let Some(v) = slots.get(*s)
                        {
                            if let Value::Uninitialized = v
                            {
                                self.eval(right, slots)?
                            }
                            else
                            {
                                v.clone()
                            }
                        }
                        else
                        {
                            self.eval(right, slots)?
                        }
                    }
                    _ => self.eval(right, slots)?,
                };
                match (l, r)
                {
                    (
                        Value::Integer {
                            value: i1,
                            kind: k1,
                        },
                        Value::Integer {
                            value: i2,
                            kind: k2,
                        },
                    ) =>
                    {
                        let kind = signed_kind_for_bits(int_kind_bits(k1).max(int_kind_bits(k2)));
                        match op
                        {
                            Op::Add => Ok(make_signed_int(i1 + i2, kind)),
                            Op::Subtract => Ok(make_signed_int(i1 - i2, kind)),
                            Op::Multiply => Ok(make_signed_int(i1 * i2, kind)),
                            Op::Divide => Ok(make_signed_int(i1 / i2, kind)),
                            Op::Power => Ok(pow_signed_int(i1, i2, kind)),
                            Op::GreaterThan => Ok(Value::Boolean(i1 > i2)),
                            Op::LessThan => Ok(Value::Boolean(i1 < i2)),
                            Op::Equal => Ok(Value::Boolean(i1 == i2)),
                            Op::NotEqual => Ok(Value::Boolean(i1 != i2)),
                        }
                    }
                    (
                        Value::Unsigned {
                            value: u1,
                            kind: k1,
                        },
                        Value::Unsigned {
                            value: u2,
                            kind: k2,
                        },
                    ) =>
                    {
                        let kind = unsigned_kind_for_bits(int_kind_bits(k1).max(int_kind_bits(k2)));
                        match op
                        {
                            Op::Add => Ok(make_unsigned_int(u1 + u2, kind)),
                            Op::Subtract => Ok(make_unsigned_int(u1 - u2, kind)),
                            Op::Multiply => Ok(make_unsigned_int(u1 * u2, kind)),
                            Op::Divide => Ok(make_unsigned_int(u1 / u2, kind)),
                            Op::Power => Ok(pow_unsigned_int(u1, u2, kind)),
                            Op::GreaterThan => Ok(Value::Boolean(u1 > u2)),
                            Op::LessThan => Ok(Value::Boolean(u1 < u2)),
                            Op::Equal => Ok(Value::Boolean(u1 == u2)),
                            Op::NotEqual => Ok(Value::Boolean(u1 != u2)),
                        }
                    }
                    (Value::Integer { value: i1, .. }, Value::Unsigned { value: u2, .. }) =>
                    {
                        let u2_i = i128::try_from(u2).map_err(|_| RuntimeError::simple("Unsigned value too large for signed operation".to_string(), line))?;
                        let kind = IntKind::I128;
                        match op
                        {
                            Op::Add => Ok(make_signed_int(i1 + u2_i, kind)),
                            Op::Subtract => Ok(make_signed_int(i1 - u2_i, kind)),
                            Op::Multiply => Ok(make_signed_int(i1 * u2_i, kind)),
                            Op::Divide => Ok(make_signed_int(i1 / u2_i, kind)),
                            Op::Power => Ok(pow_signed_int(i1, u2_i, kind)),
                            Op::GreaterThan => Ok(Value::Boolean(i1 > u2_i)),
                            Op::LessThan => Ok(Value::Boolean(i1 < u2_i)),
                            Op::Equal => Ok(Value::Boolean(i1 == u2_i)),
                            Op::NotEqual => Ok(Value::Boolean(i1 != u2_i)),
                        }
                    }
                    (Value::Unsigned { value: u1, .. }, Value::Integer { value: i2, .. }) =>
                    {
                        let u1_i = i128::try_from(u1).map_err(|_| RuntimeError::simple("Unsigned value too large for signed operation".to_string(), line))?;
                        let kind = IntKind::I128;
                        match op
                        {
                            Op::Add => Ok(make_signed_int(u1_i + i2, kind)),
                            Op::Subtract => Ok(make_signed_int(u1_i - i2, kind)),
                            Op::Multiply => Ok(make_signed_int(u1_i * i2, kind)),
                            Op::Divide => Ok(make_signed_int(u1_i / i2, kind)),
                            Op::Power => Ok(pow_signed_int(u1_i, i2, kind)),
                            Op::GreaterThan => Ok(Value::Boolean(u1_i > i2)),
                            Op::LessThan => Ok(Value::Boolean(u1_i < i2)),
                            Op::Equal => Ok(Value::Boolean(u1_i == i2)),
                            Op::NotEqual => Ok(Value::Boolean(u1_i != i2)),
                        }
                    }
                    (
                        Value::Float {
                            value: f1,
                            kind: k1,
                        },
                        Value::Float {
                            value: f2,
                            kind: k2,
                        },
                    ) =>
                    {
                        let kind = promote_float_kind(k1, k2);
                        match op
                        {
                            Op::Add => Ok(make_float(f1 + f2, kind)),
                            Op::Subtract => Ok(make_float(f1 - f2, kind)),
                            Op::Multiply => Ok(make_float(f1 * f2, kind)),
                            Op::Divide => Ok(make_float(f1 / f2, kind)),
                            Op::Power => Ok(make_float(f1.powf(f2), kind)),
                            Op::GreaterThan => Ok(Value::Boolean(f1 > f2)),
                            Op::LessThan => Ok(Value::Boolean(f1 < f2)),
                            Op::Equal => Ok(Value::Boolean(f1 == f2)),
                            Op::NotEqual => Ok(Value::Boolean(f1 != f2)),
                        }
                    }
                    (v @ Value::Integer { .. }, Value::Float { value: f, kind })
                    | (v @ Value::Unsigned { .. }, Value::Float { value: f, kind }) =>
                    {
                        let f1 = int_value_as_f64(&v).unwrap_or(0.0);
                        match op
                        {
                            Op::Add => Ok(make_float(f1 + f, kind)),
                            Op::Subtract => Ok(make_float(f1 - f, kind)),
                            Op::Multiply => Ok(make_float(f1 * f, kind)),
                            Op::Divide => Ok(make_float(f1 / f, kind)),
                            Op::Power => Ok(make_float(f1.powf(f), kind)),
                            Op::GreaterThan => Ok(Value::Boolean(f1 > f)),
                            Op::LessThan => Ok(Value::Boolean(f1 < f)),
                            Op::Equal => Ok(Value::Boolean(f1 == f)),
                            Op::NotEqual => Ok(Value::Boolean(f1 != f)),
                        }
                    }
                    (Value::Float { value: f, kind }, v @ Value::Integer { .. })
                    | (Value::Float { value: f, kind }, v @ Value::Unsigned { .. }) =>
                    {
                        let f2 = int_value_as_f64(&v).unwrap_or(0.0);
                        match op
                        {
                            Op::Add => Ok(make_float(f + f2, kind)),
                            Op::Subtract => Ok(make_float(f - f2, kind)),
                            Op::Multiply => Ok(make_float(f * f2, kind)),
                            Op::Divide => Ok(make_float(f / f2, kind)),
                            Op::Power => Ok(make_float(f.powf(f2), kind)),
                            Op::GreaterThan => Ok(Value::Boolean(f > f2)),
                            Op::LessThan => Ok(Value::Boolean(f < f2)),
                            Op::Equal => Ok(Value::Boolean(f == f2)),
                            Op::NotEqual => Ok(Value::Boolean(f != f2)),
                        }
                    }
                    (Value::String(s1), Value::String(s2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut out = s1.clone();
                            Rc::make_mut(&mut out).push_str(&s2);
                            Ok(Value::String(out))
                        }
                        Op::Equal => Ok(Value::Boolean(s1 == s2)),
                        Op::NotEqual => Ok(Value::Boolean(s1 != s2)),
                        _ => Err(RuntimeError::simple("Invalid operation on two strings".to_string(), line)),
                    },
                    (Value::String(s), v2) => match op
                    {
                        Op::Add =>
                        {
                            let mut out = s.clone();
                            Rc::make_mut(&mut out).push_str(&v2.inspect());
                            Ok(Value::String(out))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple(format!("Invalid operation between String and {:?}", v2), line)),
                    },
                    // Array concatenation cases
                    (Value::Array(arr1), Value::Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result = arr1.borrow().clone();
                            result.extend(arr2.borrow().iter().cloned());
                            Ok(Value::Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(*arr1.borrow() == *arr2.borrow())),
                        Op::NotEqual => Ok(Value::Boolean(*arr1.borrow() != *arr2.borrow())),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::Array(arr1), Value::I64Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result = arr1.borrow().clone();
                            for v in arr2.borrow().iter()
                            {
                                result.push(make_signed_int(*v as i128, IntKind::I64));
                            }
                            Ok(Value::Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::Array(arr1), Value::I32Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result = arr1.borrow().clone();
                            for v in arr2.borrow().iter()
                            {
                                result.push(make_signed_int(*v as i128, IntKind::I32));
                            }
                            Ok(Value::Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::Array(arr1), Value::F64Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result = arr1.borrow().clone();
                            for v in arr2.borrow().iter()
                            {
                                result.push(make_float(*v, FloatKind::F64));
                            }
                            Ok(Value::Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::Array(arr1), Value::F32Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result = arr1.borrow().clone();
                            for v in arr2.borrow().iter()
                            {
                                result.push(make_float(*v as f64, FloatKind::F32));
                            }
                            Ok(Value::Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::I64Array(arr1), Value::I64Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result = arr1.borrow().clone();
                            result.extend(arr2.borrow().iter().cloned());
                            Ok(Value::I64Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(*arr1.borrow() == *arr2.borrow())),
                        Op::NotEqual => Ok(Value::Boolean(*arr1.borrow() != *arr2.borrow())),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::I32Array(arr1), Value::I32Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result = arr1.borrow().clone();
                            result.extend(arr2.borrow().iter().cloned());
                            Ok(Value::I32Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(*arr1.borrow() == *arr2.borrow())),
                        Op::NotEqual => Ok(Value::Boolean(*arr1.borrow() != *arr2.borrow())),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::I64Array(arr1), Value::Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result: Vec<Value> = arr1
                                .borrow()
                                .iter()
                                .map(|v| make_signed_int(*v as i128, IntKind::I64))
                                .collect();
                            result.extend(arr2.borrow().iter().cloned());
                            Ok(Value::Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::I32Array(arr1), Value::Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result: Vec<Value> = arr1
                                .borrow()
                                .iter()
                                .map(|v| make_signed_int(*v as i128, IntKind::I32))
                                .collect();
                            result.extend(arr2.borrow().iter().cloned());
                            Ok(Value::Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::I64Array(arr1), Value::F64Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result: Vec<f64> =
                                arr1.borrow().iter().map(|v| *v as f64).collect();
                            result.extend(arr2.borrow().iter().cloned());
                            Ok(Value::F64Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::I32Array(arr1), Value::F32Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result: Vec<f32> =
                                arr1.borrow().iter().map(|v| *v as f32).collect();
                            result.extend(arr2.borrow().iter().cloned());
                            Ok(Value::F32Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::F64Array(arr1), Value::F64Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result = arr1.borrow().clone();
                            result.extend(arr2.borrow().iter().cloned());
                            Ok(Value::F64Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(*arr1.borrow() == *arr2.borrow())),
                        Op::NotEqual => Ok(Value::Boolean(*arr1.borrow() != *arr2.borrow())),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::F32Array(arr1), Value::F32Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result = arr1.borrow().clone();
                            result.extend(arr2.borrow().iter().cloned());
                            Ok(Value::F32Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(*arr1.borrow() == *arr2.borrow())),
                        Op::NotEqual => Ok(Value::Boolean(*arr1.borrow() != *arr2.borrow())),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::F64Array(arr1), Value::Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result: Vec<Value> = arr1
                                .borrow()
                                .iter()
                                .map(|v| make_float(*v, FloatKind::F64))
                                .collect();
                            result.extend(arr2.borrow().iter().cloned());
                            Ok(Value::Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::F32Array(arr1), Value::Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result: Vec<Value> = arr1
                                .borrow()
                                .iter()
                                .map(|v| make_float(*v as f64, FloatKind::F32))
                                .collect();
                            result.extend(arr2.borrow().iter().cloned());
                            Ok(Value::Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::F64Array(arr1), Value::I64Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result = arr1.borrow().clone();
                            for v in arr2.borrow().iter()
                            {
                                result.push(*v as f64);
                            }
                            Ok(Value::F64Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (Value::F32Array(arr1), Value::I32Array(arr2)) => match op
                    {
                        Op::Add =>
                        {
                            let mut result = arr1.borrow().clone();
                            for v in arr2.borrow().iter()
                            {
                                result.push(*v as f32);
                            }
                            Ok(Value::F32Array(Rc::new(RefCell::new(result))))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple("Invalid operation on arrays".to_string(), line)),
                    },
                    (v1, v2) => match op
                    {
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError::simple(format!(
                                "Type mismatch: Cannot operate {:?} on {:?} and {:?}",
                                op, v1, v2
                            ), line)),
                    },
                }
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } =>
            {
                let val = self.eval(condition, slots)?;
                let is_truthy = match val
                {
                    Value::Boolean(false) | Value::Nil => false,
                    _ => true,
                };
                if is_truthy
                {
                    self.eval(then_branch, slots)
                }
                else if let Some(else_expr) = else_branch
                {
                    self.eval(else_expr, slots)
                }
                else
                {
                    Ok(Value::Nil)
                }
            }
            ExprKind::While { condition, body } =>
            {
                let mut last_val = Value::Nil;
                loop
                {
                    let cond_val = self.eval(condition, slots)?;
                    let is_true = match cond_val
                    {
                        Value::Boolean(false) | Value::Nil => false,
                        _ => true,
                    };
                    if !is_true
                    {
                        break;
                    }
                    last_val = self.eval(body, slots)?;
                }
                Ok(last_val)
            }
            ExprKind::For {
                var,
                iterable,
                body,
                ..
            } =>
            {
                let iter_val = self.eval(iterable, slots)?;
                let mut last_val = Value::Nil;
                match iter_val
                {
                    Value::Array(arr) =>
                    {
                        let len = arr.borrow().len();
                        for idx in 0..len
                        {
                            let item = {
                                let vec = arr.borrow();
                                vec[idx].clone()
                            };
                            self.env.borrow_mut().assign(*var, item);
                            last_val = self.eval(body, slots)?;
                        }
                        Ok(last_val)
                    }
                    Value::F64Array(arr) =>
                    {
                        let len = arr.borrow().len();
                        for idx in 0..len
                        {
                            let item = {
                                let vec = arr.borrow();
                                make_float(vec[idx], FloatKind::F64)
                            };
                            self.env.borrow_mut().assign(*var, item);
                            last_val = self.eval(body, slots)?;
                        }
                        Ok(last_val)
                    }
                    Value::F32Array(arr) =>
                    {
                        let len = arr.borrow().len();
                        for idx in 0..len
                        {
                            let item = {
                                let vec = arr.borrow();
                                make_float(vec[idx] as f64, FloatKind::F32)
                            };
                            self.env.borrow_mut().assign(*var, item);
                            last_val = self.eval(body, slots)?;
                        }
                        Ok(last_val)
                    }
                    Value::I64Array(arr) =>
                    {
                        let len = arr.borrow().len();
                        for idx in 0..len
                        {
                            let item = {
                                let vec = arr.borrow();
                                make_signed_int(vec[idx] as i128, IntKind::I64)
                            };
                            self.env.borrow_mut().assign(*var, item);
                            last_val = self.eval(body, slots)?;
                        }
                        Ok(last_val)
                    }
                    Value::I32Array(arr) =>
                    {
                        let len = arr.borrow().len();
                        for idx in 0..len
                        {
                            let item = {
                                let vec = arr.borrow();
                                make_signed_int(vec[idx] as i128, IntKind::I32)
                            };
                            self.env.borrow_mut().assign(*var, item);
                            last_val = self.eval(body, slots)?;
                        }
                        Ok(last_val)
                    }
                    Value::Map(map) =>
                    {
                        let map_ref = map.borrow();
                        let mut keys = Vec::with_capacity(map_ref.data.len());
                        keys.extend(map_ref.data.keys().cloned());
                        for key in keys
                        {
                            self.env.borrow_mut().assign(*var, Value::String(key));
                            last_val = self.eval(body, slots)?;
                        }
                        Ok(last_val)
                    }
                    _ => Err(RuntimeError::simple("Type is not iterable".to_string(), line)),
                }
            }
        ExprKind::Loop {
            count,
            var,
            var_slot,
            body,
        } =>
        {
            let count_val = self.eval(count, slots)?;
            let n = number_to_usize(&count_val).ok_or_else(|| RuntimeError::simple("Loop count must be a non-negative number".to_string(), line))?;
            for idx in 0..n
            {
                if let Some(slot) = var_slot
                {
                    if let Some(slot_val) = slots.get_mut(*slot)
                    {
                        *slot_val = default_int(idx as i128);
                    }
                }
                else if let Some(name) = var
                {
                    self.env
                        .borrow_mut()
                        .assign(*name, default_int(idx as i128));
                }
                let _ = self.eval(body, slots)?;
            }
            Ok(Value::Nil)
        }
        ExprKind::Collect {
            count,
            into,
            var,
            var_slot,
            body,
        } =>
        {
            let count_val = self.eval(count, slots)?;
            let n = number_to_usize(&count_val).ok_or_else(|| RuntimeError::simple("Collect count must be a non-negative number".to_string(), line))?;
            let into_val = if let Some(expr) = into
            {
                Some(self.eval(expr, slots)?)
            }
            else
            {
                None
            };

            match into_val
            {
                Some(Value::Array(arr)) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err(RuntimeError::simple("Collect into array length mismatch".to_string(), line));
                    }
                    for idx in 0..n
                    {
                        if let Some(slot) = var_slot
                        {
                            if let Some(slot_val) = slots.get_mut(*slot)
                            {
                                *slot_val = default_int(idx as i128);
                            }
                        }
                        else if let Some(name) = var
                        {
                            self.env
                                .borrow_mut()
                                .assign(*name, default_int(idx as i128));
                        }
                        let val = self.eval(body, slots)?;
                        arr.borrow_mut()[idx] = val;
                    }
                    Ok(Value::Array(arr))
                }
                Some(Value::F64Array(arr)) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err(RuntimeError::simple("Collect into array length mismatch".to_string(), line));
                    }
                    for idx in 0..n
                    {
                        if let Some(slot) = var_slot
                        {
                            if let Some(slot_val) = slots.get_mut(*slot)
                            {
                                *slot_val = default_int(idx as i128);
                            }
                        }
                        else if let Some(name) = var
                        {
                            self.env
                                .borrow_mut()
                                .assign(*name, default_int(idx as i128));
                        }
                        let val = self.eval(body, slots)?;
                        let num = int_value_as_f64(&val).ok_or_else(|| RuntimeError::simple("Collect into F64Array expects numeric results".to_string(), line))?;
                        arr.borrow_mut()[idx] = num;
                    }
                    Ok(Value::F64Array(arr))
                }
                Some(Value::F32Array(arr)) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err(RuntimeError::simple("Collect into array length mismatch".to_string(), line));
                    }
                    for idx in 0..n
                    {
                        if let Some(slot) = var_slot
                        {
                            if let Some(slot_val) = slots.get_mut(*slot)
                            {
                                *slot_val = default_int(idx as i128);
                            }
                        }
                        else if let Some(name) = var
                        {
                            self.env
                                .borrow_mut()
                                .assign(*name, default_int(idx as i128));
                        }
                        let val = self.eval(body, slots)?;
                        let num = int_value_as_f64(&val).ok_or_else(|| RuntimeError::simple("Collect into F32Array expects numeric results".to_string(), line))?;
                        arr.borrow_mut()[idx] = num as f32;
                    }
                    Ok(Value::F32Array(arr))
                }
                Some(Value::I64Array(arr)) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err(RuntimeError::simple("Collect into array length mismatch".to_string(), line));
                    }
                    for idx in 0..n
                    {
                        if let Some(slot) = var_slot
                        {
                            if let Some(slot_val) = slots.get_mut(*slot)
                            {
                                *slot_val = default_int(idx as i128);
                            }
                        }
                        else if let Some(name) = var
                        {
                            self.env
                                .borrow_mut()
                                .assign(*name, default_int(idx as i128));
                        }
                        let val = self.eval(body, slots)?;
                        let num = int_value_as_i64(&val).ok_or_else(|| RuntimeError::simple("Collect into I64Array expects integer results".to_string(), line))?;
                        arr.borrow_mut()[idx] = num;
                    }
                    Ok(Value::I64Array(arr))
                }
                Some(Value::I32Array(arr)) =>
                {
                    if arr.borrow().len() != n
                    {
                        return Err(RuntimeError::simple("Collect into array length mismatch".to_string(), line));
                    }
                    for idx in 0..n
                    {
                        if let Some(slot) = var_slot
                        {
                            if let Some(slot_val) = slots.get_mut(*slot)
                            {
                                *slot_val = default_int(idx as i128);
                            }
                        }
                        else if let Some(name) = var
                        {
                            self.env
                                .borrow_mut()
                                .assign(*name, default_int(idx as i128));
                        }
                        let val = self.eval(body, slots)?;
                        let num = int_value_as_i64(&val).ok_or_else(|| RuntimeError::simple("Collect into I32Array expects integer results".to_string(), line))?;
                        if num < i32::MIN as i64 || num > i32::MAX as i64
                        {
                            return Err(RuntimeError::simple("Collect into I32Array result out of range".to_string(), line));
                        }
                        arr.borrow_mut()[idx] = num as i32;
                    }
                    Ok(Value::I32Array(arr))
                }
                Some(_) =>
                {
                    Err(RuntimeError::simple("Collect into expects an array".to_string(), line))
                }
                None =>
                {
                    let mut out = Vec::with_capacity(n);
                    for idx in 0..n
                    {
                        if let Some(slot) = var_slot
                        {
                            if let Some(slot_val) = slots.get_mut(*slot)
                            {
                                *slot_val = default_int(idx as i128);
                            }
                        }
                        else if let Some(name) = var
                        {
                            self.env
                                .borrow_mut()
                                .assign(*name, default_int(idx as i128));
                        }
                        out.push(self.eval(body, slots)?);
                    }
                    Ok(Value::Array(Rc::new(RefCell::new(out))))
                }
            }
        }
            ExprKind::Block(statements) =>
            {
                let mut last = Value::Nil;
                for stmt in statements
                {
                    last = self.eval(stmt, slots)?;
                }
                Ok(last)
            }
        };
        CURRENT_SPAN.with(|span| {
            *span.borrow_mut() = prev_span;
        });
        result
    }

    pub fn eval_ast_in(
        &mut self,
        mut ast: Expr,
        env: Rc<EnvValue>,
        program: Option<Value>,
    ) -> EvalResult
    {
        resolve_slots(&mut ast);
        let new_env = self.get_env(None, false);
        {
            let env_ptr = Rc::as_ptr(&env) as usize;
            if !self.env_seed_cache.contains_key(&env_ptr)
            {
                let mut entries = Vec::with_capacity(env.data.len());
                for (key, value) in env.data.iter()
                {
                    let name = intern::intern_symbol(key.as_str());
                    entries.push((name, value.clone()));
                }
                self.env_seed_cache.insert(env_ptr, entries);
            }
            if let Some(cached) = self.env_seed_cache.get(&env_ptr)
            {
                for (name, value) in cached
                {
                    new_env.borrow_mut().define(*name, clone_frozen_value(value));
                }
            }
        }
        if let Some(Value::Reference(reference)) = program
        {
            let program_sym = intern::intern_symbol("program");
            new_env
                .borrow_mut()
                .define(program_sym, Value::Reference(reference));
        }

        let original_env = self.env.clone();
        let original_autoload_std = self.autoload_std;
        self.autoload_std = false;
        self.env = new_env.clone();
        let result = self.eval(&ast, &mut []);
        self.env = original_env;
        self.autoload_std = original_autoload_std;
        self.recycle_env(new_env);
        result
    }
}
