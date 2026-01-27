use crate::eval::Interpreter;
use crate::intern;
use crate::value::{HostFunction, MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

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
        Value::WasmFunction(func) =>
        {
            let rest = args[2..].to_vec();
            interpreter.call_wasm_function_public(func, rest)
        }
        _ => Err(format!(
            "wasm member '{}' in module '{}' is not a function",
            func_name, module_name
        )),
    }
}

pub fn build_wasm_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("list"), Value::HostFunction(native_wasm_list as HostFunction));
    map.insert(intern::intern("call"), Value::HostFunction(native_wasm_call as HostFunction));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
