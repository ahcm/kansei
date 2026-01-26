use crate::ast::{FloatKind, IntKind};
use crate::eval::Interpreter;
use crate::intern;
use crate::sexpr;
use crate::value::{MapValue, Value, clone_frozen_value};
use rayon::prelude::*;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

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

fn get_native_function(value: &Value) -> Result<fn(&[Value]) -> Result<Value, String>, String>
{
    match value
    {
        Value::NativeFunction(func) => Ok(*func),
        _ => Err("parallel requires a native function".to_string()),
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

fn fill_collect_into(into: &Value, values: Vec<Value>) -> Result<Value, String>
{
    match into
    {
        Value::Array(arr) =>
        {
            if arr.borrow().len() != values.len()
            {
                return Err("parallel.collect into array length mismatch".to_string());
            }
            let mut arr_mut = arr.borrow_mut();
            for (idx, val) in values.into_iter().enumerate()
            {
                arr_mut[idx] = val;
            }
            Ok(Value::Array(arr.clone()))
        }
        Value::F64Array(arr) =>
        {
            if arr.borrow().len() != values.len()
            {
                return Err("parallel.collect into array length mismatch".to_string());
            }
            let mut arr_mut = arr.borrow_mut();
            for (idx, val) in values.into_iter().enumerate()
            {
                let num = value_to_f64(val)?;
                arr_mut[idx] = num;
            }
            Ok(Value::F64Array(arr.clone()))
        }
        Value::F32Array(arr) =>
        {
            if arr.borrow().len() != values.len()
            {
                return Err("parallel.collect into array length mismatch".to_string());
            }
            let mut arr_mut = arr.borrow_mut();
            for (idx, val) in values.into_iter().enumerate()
            {
                let num = value_to_f64(val)? as f32;
                arr_mut[idx] = num;
            }
            Ok(Value::F32Array(arr.clone()))
        }
        Value::I64Array(arr) =>
        {
            if arr.borrow().len() != values.len()
            {
                return Err("parallel.collect into array length mismatch".to_string());
            }
            let mut arr_mut = arr.borrow_mut();
            for (idx, val) in values.into_iter().enumerate()
            {
                let num = value_to_i64(val)?;
                arr_mut[idx] = num;
            }
            Ok(Value::I64Array(arr.clone()))
        }
        Value::I32Array(arr) =>
        {
            if arr.borrow().len() != values.len()
            {
                return Err("parallel.collect into array length mismatch".to_string());
            }
            let mut arr_mut = arr.borrow_mut();
            for (idx, val) in values.into_iter().enumerate()
            {
                let num = value_to_i64(val)?;
                if num < i32::MIN as i64 || num > i32::MAX as i64
                {
                    return Err("parallel.collect into I32Array result out of range".to_string());
                }
                arr_mut[idx] = num as i32;
            }
            Ok(Value::I32Array(arr.clone()))
        }
        _ => Err("parallel.collect into expects an array".to_string()),
    }
}

fn resolve_parallel_args(args: &[Value]) -> Result<(&Value, Option<&Value>, bool), String>
{
    if args.len() < 2 || args.len() > 3
    {
        return Err("parallel function expects target, function, and optional context".to_string());
    }
    let mut func_arg = &args[1];
    let mut context_arg = if args.len() == 3
    {
        Some(&args[2])
    }
    else
    {
        None
    };
    let mut new_order = false;

    if !matches!(func_arg, Value::Function(_) | Value::NativeFunction(_))
    {
        if args.len() == 3 && matches!(&args[2], Value::Function(_) | Value::NativeFunction(_))
        {
            func_arg = &args[2];
            context_arg = Some(&args[1]);
            new_order = true;
        }
        else
        {
            return Err("parallel function expects a function as 2nd or 3rd argument".to_string());
        }
    }
    Ok((func_arg, context_arg, new_order))
}

fn resolve_parallel_collect_args(
    args: &[Value],
) -> Result<(&Value, Option<&Value>, Option<&Value>, bool), String>
{
    if args.len() < 2 || args.len() > 4
    {
        return Err(
            "parallel.collect expects count, function, optional context, and optional into"
                .to_string(),
        );
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
            return Err("parallel.collect expects a function as 2nd or 3rd argument".to_string());
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
        return Err("parallel.collect expects function as 2nd or 3rd argument".to_string());
    }

    Ok((&args[func_idx], context_arg, into_arg, new_order))
}

fn native_parallel_map(args: &[Value]) -> Result<Value, String>
{
    // For now, only support native functions to avoid code explosion,
    // unless requested. Keeping legacy support.
    // If user passed context or new order, we error for now or fallback?
    // We'll support Legacy Native path fully.

    if args.len() == 2
    {
        if let Value::NativeFunction(func) = &args[1]
        {
            let func = *func;
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
                    return Ok(Value::F64Array(Rc::new(RefCell::new(result?))));
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
                    return Ok(Value::F32Array(Rc::new(RefCell::new(result?))));
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
                    return Ok(Value::I64Array(Rc::new(RefCell::new(result?))));
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
                    return Ok(Value::I32Array(Rc::new(RefCell::new(result?))));
                }
                _ => return Err("parallel.map requires a numeric array".to_string()),
            }
        }
    }

    Err("parallel.map only supports (array, native_function) for now".to_string())
}

fn native_parallel_each(args: &[Value]) -> Result<Value, String>
{
    native_parallel_map(args)
}

fn native_parallel_apply(args: &[Value]) -> Result<Value, String>
{
    // Legacy support only
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
    let (func_arg, context_arg, new_order) = resolve_parallel_args(args)?;

    let n = match &args[0]
    {
        Value::Integer { value, .. } if *value >= 0 => *value as usize,
        Value::Unsigned { value, .. } => *value as usize,
        _ => return Err("parallel.loop expects non-negative integer count".to_string()),
    };

    let context_sexpr = if let Some(val) = context_arg
    {
        Some(sexpr::value_to_sexpr(val)?)
    }
    else
    {
        None
    };

    if let Value::NativeFunction(func) = func_arg
    {
        let func = *func;

        let result: Result<(), String> = (0..n)
            .into_par_iter()
            .try_for_each(|idx| {
                let arg = Value::Integer {
                    value: idx as i128,
                    kind: IntKind::I64,
                };
                let _ = if let Some(ctx_sexpr) = &context_sexpr
                {
                    let ctx_val = sexpr::sexpr_to_value(ctx_sexpr)?;
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
                Ok(())
            });
        result?;
        return Ok(Value::Nil);
    }

    if let Value::Function(_) = func_arg
    {
        let func_sexpr = sexpr::value_to_sexpr(func_arg)?;

        let result: Result<(), String> = (0..n)
            .into_par_iter()
            .try_for_each(|idx| {
                let arg = Value::Integer {
                    value: idx as i128,
                    kind: IntKind::I64,
                };

                let mut interpreter = Interpreter::new();
                interpreter.set_autoload_std(false);
                let func_val = sexpr::sexpr_to_value(&func_sexpr)?;

                let _ = if let Some(ctx_sexpr) = &context_sexpr
                {
                    let ctx_val = sexpr::sexpr_to_value(ctx_sexpr)?;

                    // Inject context into function environment if it's an Env
                    if let Value::Function(data) = &func_val
                    {
                        {
                            let mut env = data.env.borrow_mut();
                            env.is_partial = true; // Allow lookup from child scope (closure)
                            match &ctx_val
                            {
                                Value::Env(env_val) =>
                                {
                                    for (key, val) in &env_val.data
                                    {
                                        let sym = intern::intern_symbol(key.as_str());
                                        env.define(sym, clone_frozen_value(val));
                                    }
                                }
                                _ =>
                                {}
                            }
                        }

                        let args = if data.params.len() == 1
                        {
                            vec![arg]
                        }
                        else if new_order
                        {
                            vec![ctx_val, arg]
                        }
                        else
                        {
                            vec![arg, ctx_val]
                        };
                        interpreter.call_value_from_host(func_val, args)?
                    }
                    else
                    {
                        return Err("Deserialized parallel function is invalid".to_string());
                    }
                }
                else
                {
                    interpreter.call_value_from_host(func_val, vec![arg])?
                };
                Ok(())
            });
        result?;
        return Ok(Value::Nil);
    }

    Err("parallel.loop expects a native function or a user function".to_string())
}

fn native_parallel_collect(args: &[Value]) -> Result<Value, String>
{
    let (func_arg, context_arg, into_arg, new_order) = resolve_parallel_collect_args(args)?;

    let n = match &args[0]
    {
        Value::Integer { value, .. } if *value >= 0 => *value as usize,
        Value::Unsigned { value, .. } => *value as usize,
        _ => return Err("parallel.collect expects non-negative integer count".to_string()),
    };

    let context_sexpr = if let Some(val) = context_arg
    {
        Some(sexpr::value_to_sexpr(val)?)
    }
    else
    {
        None
    };

    if let Some(into) = into_arg
    {
        if !is_collect_into_array(into)
        {
            return Err("parallel.collect into expects an array".to_string());
        }
        let len_ok = match into
        {
            Value::Array(arr) => arr.borrow().len() == n,
            Value::F64Array(arr) => arr.borrow().len() == n,
            Value::F32Array(arr) => arr.borrow().len() == n,
            Value::I64Array(arr) => arr.borrow().len() == n,
            Value::I32Array(arr) => arr.borrow().len() == n,
            _ => false,
        };
        if !len_ok
        {
            return Err("parallel.collect into array length mismatch".to_string());
        }
    }

    if let Value::NativeFunction(func) = func_arg
    {
        let func = *func;
        let results: Result<Vec<String>, String> = (0..n)
            .into_par_iter()
            .map(|idx| {
                let arg = Value::Integer {
                    value: idx as i128,
                    kind: IntKind::I64,
                };
                let out = if let Some(ctx_sexpr) = &context_sexpr
                {
                    let ctx_val = sexpr::sexpr_to_value(ctx_sexpr)?;
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
                let sexpr = sexpr::value_to_sexpr(&out)?;
                Ok(sexpr.to_string())
            })
            .collect();
        let mut out = Vec::with_capacity(n);
        for item in results?
        {
            let sexpr = sexpr::parse_sexpr(&item)?;
            out.push(sexpr::sexpr_to_value(&sexpr)?);
        }
        if let Some(into) = into_arg
        {
            return fill_collect_into(into, out);
        }
        return Ok(Value::Array(Rc::new(RefCell::new(out))));
    }

    if let Value::Function(_) = func_arg
    {
        let func_sexpr = sexpr::value_to_sexpr(func_arg)?;

        let results: Result<Vec<String>, String> = (0..n)
            .into_par_iter()
            .map(|idx| {
                let arg = Value::Integer {
                    value: idx as i128,
                    kind: IntKind::I64,
                };

                let mut interpreter = Interpreter::new();
                interpreter.set_autoload_std(false);
                let func_val = sexpr::sexpr_to_value(&func_sexpr)?;

                let out = if let Some(ctx_sexpr) = &context_sexpr
                {
                    let ctx_val = sexpr::sexpr_to_value(ctx_sexpr)?;

                    // Inject context into function environment if it's an Env
                    if let Value::Function(data) = &func_val
                    {
                        {
                            let mut env = data.env.borrow_mut();
                            env.is_partial = true;
                            if let Value::Env(env_val) = &ctx_val
                            {
                                for (key, val) in &env_val.data
                                {
                                    let sym = intern::intern_symbol(key.as_str());
                                    env.define(sym, clone_frozen_value(val));
                                }
                            }
                        }

                        let args = if data.params.len() == 1
                        {
                            vec![arg]
                        }
                        else if new_order
                        {
                            vec![ctx_val, arg]
                        }
                        else
                        {
                            vec![arg, ctx_val]
                        };
                        interpreter.call_value_from_host(func_val, args)?
                    }
                    else
                    {
                        return Err("Deserialized parallel function is invalid".to_string());
                    }
                }
                else
                {
                    interpreter.call_value_from_host(func_val, vec![arg])?
                };
                let sexpr = sexpr::value_to_sexpr(&out)?;
                Ok(sexpr.to_string())
            })
            .collect();
        let mut out = Vec::with_capacity(n);
        for item in results?
        {
            let sexpr = sexpr::parse_sexpr(&item)?;
            out.push(sexpr::sexpr_to_value(&sexpr)?);
        }
        if let Some(into) = into_arg
        {
            return fill_collect_into(into, out);
        }
        return Ok(Value::Array(Rc::new(RefCell::new(out))));
    }

    Err("parallel.collect expects a native function or a user function".to_string())
}

pub fn build_parallel_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("each"), Value::NativeFunction(native_parallel_each));
    map.insert(intern::intern("apply"), Value::NativeFunction(native_parallel_apply));
    map.insert(intern::intern("map"), Value::NativeFunction(native_parallel_map));
    map.insert(intern::intern("loop"), Value::NativeFunction(native_parallel_loop));
    map.insert(intern::intern("collect"), Value::NativeFunction(native_parallel_collect));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
