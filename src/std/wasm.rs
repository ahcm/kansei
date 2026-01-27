use crate::eval::Interpreter;
use crate::intern;
use crate::value::{HostFunction, MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq)]
enum SpecKind
{
    Function,
    Module,
}

fn type_list_from_value(value: &Value, name: &str) -> Result<Vec<String>, String>
{
    match value
    {
        Value::Array(arr) =>
        {
            let mut out = Vec::new();
            for item in arr.borrow().iter()
            {
                match item
                {
                    Value::String(s) => out.push(s.as_str().to_string()),
                    _ => return Err(format!("{name} expects an array of strings")),
                }
            }
            Ok(out)
        }
        _ => Err(format!("{name} expects an array of strings")),
    }
}

fn spec_kind(spec: &MapValue) -> Result<SpecKind, String>
{
    if spec.data.contains_key(&intern::intern("functions"))
    {
        Ok(SpecKind::Module)
    }
    else if spec.data.contains_key(&intern::intern("params"))
    {
        Ok(SpecKind::Function)
    }
    else
    {
        Err("wasm spec must include 'functions' or 'params'".to_string())
    }
}

fn function_spec_from_module_spec(
    spec: &MapValue,
    func_name: &str,
) -> Result<Rc<RefCell<MapValue>>, String>
{
    let funcs_val = spec
        .data
        .get(&intern::intern("functions"))
        .cloned()
        .ok_or_else(|| "wasm spec missing functions".to_string())?;
    let funcs = match funcs_val
    {
        Value::Array(arr) => arr,
        _ => return Err("wasm spec functions must be an array".to_string()),
    };
    for func in funcs.borrow().iter()
    {
        if let Value::Map(map) = func
        {
            let name_val = map.borrow().data.get(&intern::intern("name")).cloned();
            if let Some(Value::String(name)) = name_val
            {
                if name.as_str() == func_name
                {
                    return Ok(map.clone());
                }
            }
        }
    }
    Err(format!("wasm spec has no function named '{func_name}'"))
}

fn parse_function_spec(
    spec_val: &Value,
    func_name: Option<&str>,
) -> Result<(Vec<String>, Option<String>), String>
{
    let spec_map = match spec_val
    {
        Value::Map(map) => map.clone(),
        _ => return Err("wasm spec must be a map".to_string()),
    };
    let kind = spec_kind(&spec_map.borrow())?;
    let func_spec = match kind
    {
        SpecKind::Function => spec_map,
        SpecKind::Module =>
        {
            let name = func_name.ok_or_else(|| "wasm spec requires function name".to_string())?;
            function_spec_from_module_spec(&spec_map.borrow(), name)?
        }
    };
    let func_map = func_spec.borrow();
    let params_val = func_map
        .data
        .get(&intern::intern("params"))
        .cloned()
        .ok_or_else(|| "wasm spec missing params".to_string())?;
    let params = type_list_from_value(&params_val, "wasm spec params")?;
    let result = match func_map.data.get(&intern::intern("result")).cloned()
    {
        Some(Value::String(s)) => Some(s.as_str().to_string()),
        Some(Value::Nil) | None => None,
        _ => return Err("wasm spec result must be a string".to_string()),
    };
    Ok((params, result))
}

fn type_matches(value: &Value, type_name: &str) -> bool
{
    match type_name
    {
        "any" => true,
        "nil" | "void" => matches!(value, Value::Nil),
        "bool" => matches!(value, Value::Boolean(_)),
        "string" => matches!(value, Value::String(_)),
        "i32" | "i64" | "u32" | "u64" =>
        {
            matches!(value, Value::Integer { .. } | Value::Unsigned { .. } | Value::Boolean(_))
        }
        "f32" | "f64" =>
        {
            matches!(
                value,
                Value::Float { .. } | Value::Integer { .. } | Value::Unsigned { .. }
            )
        }
        "i32[]" | "u32[]" => matches!(value, Value::Array(_) | Value::I32Array(_) | Value::I64Array(_)),
        "f64[]" => matches!(value, Value::F64Array(_) | Value::Array(_)),
        _ => false,
    }
}

fn value_type_name(value: &Value) -> &'static str
{
    match value
    {
        Value::Integer { .. } => "Integer",
        Value::Unsigned { .. } => "Unsigned",
        Value::Float { .. } => "Float",
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
        Value::StructType(_) => "StructType",
        Value::StructInstance(_) => "Struct",
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
        Value::Reference(_) => "Reference",
        Value::Uninitialized => "Uninitialized",
    }
}

fn value_as_i64(value: &Value) -> Option<i64>
{
    match value
    {
        Value::Integer { value, .. } => i64::try_from(*value).ok(),
        Value::Unsigned { value, .. } => i64::try_from(*value).ok(),
        Value::Boolean(b) => Some(if *b { 1 } else { 0 }),
        _ => None,
    }
}

fn value_as_f64(value: &Value) -> Option<f64>
{
    match value
    {
        Value::Float { value, .. } => Some(*value),
        Value::Integer { value, .. } => Some(*value as f64),
        Value::Unsigned { value, .. } => Some(*value as f64),
        _ => None,
    }
}

fn validate_value_for_type(value: &Value, type_name: &str) -> bool
{
    match type_name
    {
        "i32" | "i64" | "u32" | "u64" => value_as_i64(value).is_some(),
        "f32" | "f64" => value_as_f64(value).is_some(),
        "i32[]" | "u32[]" =>
        {
            match value
            {
                Value::Array(arr) =>
                {
                    for item in arr.borrow().iter()
                    {
                        let num = match value_as_i64(item)
                        {
                            Some(num) => num,
                            None => return false,
                        };
                        if type_name == "u32[]" && (num < 0 || num > u32::MAX as i64)
                        {
                            return false;
                        }
                    }
                    true
                }
                Value::I32Array(_) | Value::I64Array(_) => true,
                _ => false,
            }
        }
        "f64[]" =>
        {
            match value
            {
                Value::F64Array(_) => true,
                Value::Array(arr) =>
                {
                    for item in arr.borrow().iter()
                    {
                        if value_as_f64(item).is_none()
                        {
                            return false;
                        }
                    }
                    true
                }
                _ => false,
            }
        }
        _ => type_matches(value, type_name),
    }
}

fn coerce_arg_for_type(value: &Value, type_name: &str) -> Result<Value, String>
{
    match type_name
    {
        "i32[]" | "u32[]" =>
        {
            match value
            {
                Value::Array(_) => Ok(value.clone()),
                Value::I32Array(arr) =>
                {
                    let vals = arr
                        .borrow()
                        .iter()
                        .map(|v| Value::Integer {
                            value: *v as i128,
                            kind: crate::ast::IntKind::I64,
                        })
                        .collect();
                    Ok(Value::Array(Rc::new(RefCell::new(vals))))
                }
                Value::I64Array(arr) =>
                {
                    let vals = arr
                        .borrow()
                        .iter()
                        .map(|v| Value::Integer {
                            value: *v as i128,
                            kind: crate::ast::IntKind::I64,
                        })
                        .collect();
                    Ok(Value::Array(Rc::new(RefCell::new(vals))))
                }
                _ => Err(format!("expected {type_name} array")),
            }
        }
        "f64[]" =>
        {
            match value
            {
                Value::F64Array(_) => Ok(value.clone()),
                Value::Array(arr) =>
                {
                    let mut out = Vec::new();
                    for item in arr.borrow().iter()
                    {
                        let num = match item
                        {
                            Value::Float { value, .. } => *value,
                            Value::Integer { value, .. } => *value as f64,
                            Value::Unsigned { value, .. } => *value as f64,
                            _ => return Err("f64[] expects numeric array".to_string()),
                        };
                        out.push(num);
                    }
                    Ok(Value::F64Array(Rc::new(RefCell::new(out))))
                }
                _ => Err("expected f64[] array".to_string()),
            }
        }
        _ => Ok(value.clone()),
    }
}

fn lookup_wasm_function(
    interpreter: &mut Interpreter,
    module_name: &str,
    func_name: &str,
) -> Result<Rc<crate::wasm::WasmFunction>, String>
{
    let wasm_sym = intern::intern_symbol("wasm");
    let wasm_ns = match interpreter.get_global_value(wasm_sym)
    {
        Some(Value::Map(map)) => map,
        _ =>
        {
            return Err(format!(
                "wasm module '{}' not loaded (use load wasm::{})",
                module_name, module_name
            ))
        }
    };

    let module_value = {
        let map_ref = wasm_ns.borrow();
        map_ref
            .data
            .get(&intern::intern(module_name))
            .cloned()
            .ok_or_else(|| {
                format!(
                    "wasm module '{}' not loaded (use load wasm::{})",
                    module_name, module_name
                )
            })?
    };

    let module_map = match module_value
    {
        Value::Map(map) => map,
        _ => return Err(format!("wasm module '{}' is not a map", module_name)),
    };

    let func_value = {
        let map_ref = module_map.borrow();
        map_ref
            .data
            .get(&intern::intern(func_name))
            .cloned()
            .ok_or_else(|| format!("wasm function '{}' not found in module '{}'", func_name, module_name))?
    };

    match func_value
    {
        Value::WasmFunction(func) => Ok(func),
        _ => Err(format!(
            "wasm member '{}' in module '{}' is not a function",
            func_name, module_name
        )),
    }
}

fn native_wasm_list(interpreter: &mut Interpreter, _args: &[Value]) -> Result<Value, String>
{
    let modules = interpreter
        .list_wasm_modules()
        .into_iter()
        .map(|name| Value::String(intern::intern_owned(name)))
        .collect();
    Ok(Value::Array(Rc::new(RefCell::new(modules))))
}

fn native_wasm_call(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    if args.len() < 2
    {
        return Err("wasm.call expects module name and function name".to_string());
    }
    let module_name = match &args[0]
    {
        Value::String(s) => s.as_str(),
        _ => return Err("wasm.call expects module name as string".to_string()),
    };
    let func_name = match &args[1]
    {
        Value::String(s) => s.as_str(),
        _ => return Err("wasm.call expects function name as string".to_string()),
    };
    let func = lookup_wasm_function(interpreter, module_name, func_name)?;
    let rest = args[2..].to_vec();
    interpreter.call_wasm_function_public(func, rest)
}

fn native_wasm_validate(_interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    if args.is_empty()
    {
        return Err("wasm.validate expects spec and arguments".to_string());
    }
    let spec = &args[0];
    let mut arg_index = 1;
    let mut func_name = None;
    if let Value::Map(map) = spec
    {
        if spec_kind(&map.borrow())? == SpecKind::Module
        {
            let name_val = args
                .get(arg_index)
                .ok_or_else(|| "wasm.validate expects function name".to_string())?;
            match name_val
            {
                Value::String(s) => func_name = Some(s.as_str()),
                _ => return Err("wasm.validate expects function name as string".to_string()),
            }
            arg_index += 1;
        }
    }
    let (params, _) = parse_function_spec(spec, func_name)?;
    if args.len() - arg_index != params.len()
    {
        return Ok(Value::Boolean(false));
    }
    for (idx, param) in params.iter().enumerate()
    {
        let val = &args[arg_index + idx];
        if !validate_value_for_type(val, param)
        {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}

fn native_wasm_call_typed(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    if args.len() < 3
    {
        return Err("wasm.call_typed expects module, function, spec, and args".to_string());
    }
    let module_name = match &args[0]
    {
        Value::String(s) => s.as_str(),
        _ => return Err("wasm.call_typed expects module name as string".to_string()),
    };
    let func_name = match &args[1]
    {
        Value::String(s) => s.as_str(),
        _ => return Err("wasm.call_typed expects function name as string".to_string()),
    };
    let spec = &args[2];
    let (params, result) = parse_function_spec(spec, Some(func_name))?;

    let provided = args.len() - 3;
    if provided != params.len()
    {
        return Err(format!(
            "wasm.call_typed argument count mismatch: expected {}, got {}",
            params.len(),
            provided
        ));
    }

    let mut coerced = Vec::with_capacity(provided);
    for (idx, param) in params.iter().enumerate()
    {
        let val = &args[3 + idx];
        if !type_matches(val, param)
        {
            return Err(format!(
                "wasm.call_typed arg {} expected {}, got {}",
                idx,
                param,
                value_type_name(val)
            ));
        }
        coerced.push(coerce_arg_for_type(val, param)?);
    }

    let func = lookup_wasm_function(interpreter, module_name, func_name)?;
    let result_val = interpreter.call_wasm_function_public(func, coerced)?;
    if let Some(result_type) = result
    {
        if result_type != "any" && !type_matches(&result_val, &result_type)
        {
            return Err(format!(
            "wasm.call_typed result expected {}, got {}",
            result_type,
            value_type_name(&result_val)
        ));
    }
    }
    Ok(result_val)
}

pub fn build_wasm_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("list"), Value::HostFunction(native_wasm_list as HostFunction));
    map.insert(intern::intern("call"), Value::HostFunction(native_wasm_call as HostFunction));
    map.insert(
        intern::intern("call_typed"),
        Value::HostFunction(native_wasm_call_typed as HostFunction),
    );
    map.insert(
        intern::intern("validate"),
        Value::HostFunction(native_wasm_validate as HostFunction),
    );
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
