use crate::ast::{FloatKind, IntKind};
use crate::eval::Interpreter;
use crate::intern;
use crate::sexpr;
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
            "parallel.loop expects count, function, and optional context".to_string()
        );
    }
    let n = match &args[0]
    {
        Value::Integer { value, .. } if *value >= 0 => *value as usize,
        Value::Unsigned { value, .. } => *value as usize,
        _ => return Err("parallel.loop expects non-negative integer count".to_string()),
    };

    // Prepare context
    let context_sexpr = if args.len() == 3
    {
        Some(sexpr::value_to_sexpr(&args[2])?)
    }
    else
    {
        None
    };

    // Prepare function (Native or User Function)
    if let Value::NativeFunction(func) = &args[1]
    {
        let func = *func;
        // Native path (Send-safe)
        // BUT context_sexpr is SExpr. We need to convert back or pass original Value if it was simple?
        // Wait, native functions expect Value.
        // We can reconstruct value from SExpr in the thread.
        
        // However, for Native function, we don't need SExpr if we assume Native function handles Value creation inside thread?
        // No, Value is !Send. We must pass Send data.
        // So SExpr roundtrip is necessary even for native function arguments (context).
        
        let results: Result<Vec<Numeric>, String> = (0..n)
            .into_par_iter()
            .map(|idx| {
                let arg = Value::Integer {
                    value: idx as i128,
                    kind: IntKind::I64,
                };
                let out = if let Some(ctx_sexpr) = &context_sexpr
                {
                    let ctx_val = sexpr::sexpr_to_value(ctx_sexpr)?;
                    func(&[arg, ctx_val])?
                }
                else
                {
                    func(&[arg])?
                };
                value_to_numeric(out)
            })
            .collect();
        return Ok(map_numeric_results(results?));
    }
    
    // User Function path
    if let Value::Function(_) = &args[1]
    {
        let func_sexpr = sexpr::value_to_sexpr(&args[1])?;
        
        let results: Result<Vec<Numeric>, String> = (0..n)
            .into_par_iter()
            .map(|idx| {
                let arg = Value::Integer {
                    value: idx as i128,
                    kind: IntKind::I64,
                };
                
                // Initialize thread-local interpreter
                let mut interpreter = Interpreter::new();
                
                // Reconstruct function
                let func_val = sexpr::sexpr_to_value(&func_sexpr)?;
                
                let out = if let Some(ctx_sexpr) = &context_sexpr
                {
                    let ctx_val = sexpr::sexpr_to_value(ctx_sexpr)?;
                    
                    // Inject context into function environment if it's a Map or Struct
                    if let Value::Function(data) = &func_val
                    {
                        let mut env = data.env.borrow_mut();
                        env.is_partial = true; // Allow lookup from child scope (closure)
                        match &ctx_val
                        {
                            Value::Map(map) =>
                            {
                                for (key, val) in &map.borrow().data
                                {
                                    let sym = intern::intern_symbol(key.as_str());
                                    env.define(sym, val.clone());
                                }
                            }
                            Value::StructInstance(s) =>
                            {
                                let fields = s.fields.borrow();
                                for (name, idx) in &s.ty.field_map
                                {
                                    if let Some(val) = fields.get(*idx)
                                    {
                                        let sym = intern::intern_symbol(name.as_str());
                                        env.define(sym, val.clone());
                                    }
                                }
                            }
                            _ => {}
                        }
                    }

                    // Call the function
                    // We can use call_value_from_host or just call_value
                    // But call_value is private?
                    // call_value_from_host is public.
                    interpreter.call_value_from_host(func_val, vec![arg, ctx_val])?
                }
                else
                {
                    interpreter.call_value_from_host(func_val, vec![arg])?
                };
                value_to_numeric(out)
            })
            .collect();
        return Ok(map_numeric_results(results?));
    }

    Err("parallel.loop expects a native function or a user function".to_string())
}

pub fn build_parallel_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("each"), Value::NativeFunction(native_parallel_each));
    map.insert(intern::intern("apply"), Value::NativeFunction(native_parallel_apply));
    map.insert(intern::intern("map"), Value::NativeFunction(native_parallel_map));
    map.insert(intern::intern("loop"), Value::NativeFunction(native_parallel_loop));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}