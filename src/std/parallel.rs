use crate::ast::{FloatKind, IntKind};
use crate::intern;
use crate::value::{MapValue, Value};
use rayon::prelude::*;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Copy)]
enum Numeric
{
    Int(i64),
    Float(f64),
}

fn value_to_i64(value: Value) -> Result<i64, String>
{
    match value
    {
        Value::Integer { value, .. } =>
        {
            i64::try_from(value).map_err(|_| "parallel expects integer result".to_string())
        }
        Value::Unsigned { value, .. } =>
        {
            i64::try_from(value).map_err(|_| "parallel expects integer result".to_string())
        }
        _ => Err("parallel expects integer result".to_string()),
    }
}

fn value_to_f64(value: Value) -> Result<f64, String>
{
    match value
    {
        Value::Float { value, .. } => Ok(value),
        Value::Integer { value, .. } => Ok(value as f64),
        Value::Unsigned { value, .. } => Ok(value as f64),
        _ => Err("parallel expects numeric result".to_string()),
    }
}

fn value_to_numeric(value: Value) -> Result<Numeric, String>
{
    match value
    {
        Value::Float { value, .. } => Ok(Numeric::Float(value)),
        Value::Integer { value, .. } => Ok(Numeric::Int(value as i64)),
        Value::Unsigned { value, .. } => Ok(Numeric::Int(value as i64)),
        _ => Err("parallel expects numeric result".to_string()),
    }
}

fn get_native_function(value: &Value) -> Result<fn(&[Value]) -> Result<Value, String>, String>
{
    match value
    {
        Value::NativeFunction(func) => Ok(*func),
        _ => Err("parallel requires a native function".to_string()),
    }
}

fn map_numeric_results(results: Vec<Numeric>) -> Value
{
    let mut all_int = true;
    for item in &results
    {
        if matches!(item, Numeric::Float(_))
        {
            all_int = false;
            break;
        }
    }
    if all_int
    {
        let vals: Vec<i64> = results
            .into_iter()
            .map(|v| match v
            {
                Numeric::Int(i) => i,
                Numeric::Float(f) => f as i64,
            })
            .collect();
        Value::I64Array(Rc::new(RefCell::new(vals)))
    }
    else
    {
        let vals: Vec<f64> = results
            .into_iter()
            .map(|v| match v
            {
                Numeric::Int(i) => i as f64,
                Numeric::Float(f) => f,
            })
            .collect();
        Value::F64Array(Rc::new(RefCell::new(vals)))
    }
}

fn native_parallel_map(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 2
    {
        return Err("parallel.map expects array and native function".to_string());
    }
    let func = get_native_function(&args[1])?;
    match &args[0]
    {
        Value::F64Array(arr) =>
        {
            let values = arr.borrow();
            let result: Result<Vec<f64>, String> = values
                .par_iter()
                .map(|v| {
                    let arg = Value::Float {
                        value: *v,
                        kind: FloatKind::F64,
                    };
                    let out = func(&[arg])?;
                    value_to_f64(out)
                })
                .collect();
            Ok(Value::F64Array(Rc::new(RefCell::new(result?))))
        }
        Value::F32Array(arr) =>
        {
            let values = arr.borrow();
            let result: Result<Vec<f32>, String> = values
                .par_iter()
                .map(|v| {
                    let arg = Value::Float {
                        value: *v as f64,
                        kind: FloatKind::F32,
                    };
                    let out = func(&[arg])?;
                    value_to_f64(out).map(|f| f as f32)
                })
                .collect();
            Ok(Value::F32Array(Rc::new(RefCell::new(result?))))
        }
        Value::I64Array(arr) =>
        {
            let values = arr.borrow();
            let result: Result<Vec<i64>, String> = values
                .par_iter()
                .map(|v| {
                    let arg = Value::Integer {
                        value: *v as i128,
                        kind: IntKind::I64,
                    };
                    let out = func(&[arg])?;
                    value_to_i64(out)
                })
                .collect();
            Ok(Value::I64Array(Rc::new(RefCell::new(result?))))
        }
        Value::I32Array(arr) =>
        {
            let values = arr.borrow();
            let result: Result<Vec<i32>, String> = values
                .par_iter()
                .map(|v| {
                    let arg = Value::Integer {
                        value: *v as i128,
                        kind: IntKind::I32,
                    };
                    let out = func(&[arg])?;
                    let i = value_to_i64(out)?;
                    if i < i32::MIN as i64 || i > i32::MAX as i64
                    {
                        return Err("parallel.map result out of i32 range".to_string());
                    }
                    Ok(i as i32)
                })
                .collect();
            Ok(Value::I32Array(Rc::new(RefCell::new(result?))))
        }
        _ => Err("parallel.map requires a numeric array".to_string()),
    }
}

fn native_parallel_each(args: &[Value]) -> Result<Value, String>
{
    native_parallel_map(args)
}

fn native_parallel_apply(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 2
    {
        return Err("parallel.apply expects array and native function".to_string());
    }
    let func = get_native_function(&args[1])?;
    match &args[0]
    {
        Value::F64Array(arr) =>
        {
            let values = arr.borrow();
            let result: Result<(), String> = values
                .par_iter()
                .map(|v| {
                    let arg = Value::Float {
                        value: *v,
                        kind: FloatKind::F64,
                    };
                    func(&[arg]).map(|_| ())
                })
                .collect();
            result?;
            Ok(Value::F64Array(arr.clone()))
        }
        Value::F32Array(arr) =>
        {
            let values = arr.borrow();
            let result: Result<(), String> = values
                .par_iter()
                .map(|v| {
                    let arg = Value::Float {
                        value: *v as f64,
                        kind: FloatKind::F32,
                    };
                    func(&[arg]).map(|_| ())
                })
                .collect();
            result?;
            Ok(Value::F32Array(arr.clone()))
        }
        Value::I64Array(arr) =>
        {
            let values = arr.borrow();
            let result: Result<(), String> = values
                .par_iter()
                .map(|v| {
                    let arg = Value::Integer {
                        value: *v as i128,
                        kind: IntKind::I64,
                    };
                    func(&[arg]).map(|_| ())
                })
                .collect();
            result?;
            Ok(Value::I64Array(arr.clone()))
        }
        Value::I32Array(arr) =>
        {
            let values = arr.borrow();
            let result: Result<(), String> = values
                .par_iter()
                .map(|v| {
                    let arg = Value::Integer {
                        value: *v as i128,
                        kind: IntKind::I32,
                    };
                    func(&[arg]).map(|_| ())
                })
                .collect();
            result?;
            Ok(Value::I32Array(arr.clone()))
        }
        _ => Err("parallel.apply requires a numeric array".to_string()),
    }
}

fn native_parallel_loop(args: &[Value]) -> Result<Value, String>
{
    if args.len() < 2 || args.len() > 3
    {
        return Err(
            "parallel.loop expects count, native function, and optional context".to_string()
        );
    }
    let n = match &args[0]
    {
        Value::Integer { value, .. } if *value >= 0 => *value as usize,
        Value::Unsigned { value, .. } => *value as usize,
        _ => return Err("parallel.loop expects non-negative integer count".to_string()),
    };
    let func = get_native_function(&args[1])?;
    let context = if args.len() == 3
    {
        Some(value_to_numeric(args[2].clone())?)
    }
    else
    {
        None
    };

    let results: Result<Vec<Numeric>, String> = (0..n)
        .into_par_iter()
        .map(|idx| {
            let arg = Value::Integer {
                value: idx as i128,
                kind: IntKind::I64,
            };
            let out = if let Some(ctx) = &context
            {
                let ctx_val = match ctx
                {
                    Numeric::Int(i) => Value::Integer {
                        value: *i as i128,
                        kind: IntKind::I64,
                    },
                    Numeric::Float(f) => Value::Float {
                        value: *f,
                        kind: FloatKind::F64,
                    },
                };
                func(&[arg, ctx_val])?
            }
            else
            {
                func(&[arg])?
            };
            value_to_numeric(out)
        })
        .collect();
    Ok(map_numeric_results(results?))
}

fn value_to_usize(v: &Value) -> Result<usize, String>
{
    match v
    {
        Value::Integer { value, .. } => Ok(*value as usize),
        Value::Unsigned { value, .. } => Ok(*value as usize),
        Value::Float { value, .. } => Ok(*value as usize),
        _ => Err("eval_a expects numeric arguments".to_string()),
    }
}

fn eval_a(i: usize, j: usize) -> f64
{
    let ij = i + j;
    let denom = (ij * (ij + 1) / 2 + i + 1) as f64;
    1.0 / denom
}

fn native_eval_a_i_j(args: &[Value]) -> Result<Value, String>
{
    // args[0] is j (idx), args[1] is i (ctx)
    let j = value_to_usize(&args[0])?;
    let i = value_to_usize(&args[1])?;
    Ok(Value::Float {
        value: eval_a(i, j),
        kind: FloatKind::F64,
    })
}

fn native_eval_a_j_i(args: &[Value]) -> Result<Value, String>
{
    // args[0] is j (idx), args[1] is i (ctx)
    let j = value_to_usize(&args[0])?;
    let i = value_to_usize(&args[1])?;
    Ok(Value::Float {
        value: eval_a(j, i),
        kind: FloatKind::F64,
    })
}

pub fn build_parallel_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("each"), Value::NativeFunction(native_parallel_each));
    map.insert(intern::intern("apply"), Value::NativeFunction(native_parallel_apply));
    map.insert(intern::intern("map"), Value::NativeFunction(native_parallel_map));
    map.insert(intern::intern("loop"), Value::NativeFunction(native_parallel_loop));
    map.insert(
        intern::intern("eval_a_i_j"),
        Value::NativeFunction(native_eval_a_i_j),
    );
    map.insert(
        intern::intern("eval_a_j_i"),
        Value::NativeFunction(native_eval_a_j_i),
    );
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
