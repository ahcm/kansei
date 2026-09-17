//! Builtin implementations and native module registration.
use super::*;

pub(super) fn native_int64_parse(args: &[Value]) -> Result<Value, String>
{
    parse_signed_int(args, IntKind::I64, "Int64")
}

pub(super) fn native_float64_parse(args: &[Value]) -> Result<Value, String>
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

pub(super) fn native_float64_sqrt(args: &[Value]) -> Result<Value, String>
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

pub(super) fn native_float32_parse(args: &[Value]) -> Result<Value, String>
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

pub(super) fn native_float32_sqrt(args: &[Value]) -> Result<Value, String>
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

pub(super) fn native_float128_parse(args: &[Value]) -> Result<Value, String>
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

pub(super) fn native_float128_sqrt(args: &[Value]) -> Result<Value, String>
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

pub(super) fn native_f64(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("f64 expects 1 argument".to_string());
    }
    cast_f64_value(&args[0])
}

pub(super) fn native_f32(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("f32 expects 1 argument".to_string());
    }
    cast_f32_value(&args[0])
}

pub(super) fn native_i64(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("i64 expects 1 argument".to_string());
    }
    cast_i64_value(&args[0])
}

pub(super) fn native_i32(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("i32 expects 1 argument".to_string());
    }
    cast_i32_value(&args[0])
}

pub(super) fn native_u64(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("u64 expects 1 argument".to_string());
    }
    cast_u64_value(&args[0])
}

pub(super) fn native_u32(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("u32 expects 1 argument".to_string());
    }
    cast_u32_value(&args[0])
}

pub(super) fn native_int8_parse(args: &[Value]) -> Result<Value, String>
{
    parse_signed_int(args, IntKind::I8, "Int8")
}
pub(super) fn native_int16_parse(args: &[Value]) -> Result<Value, String>
{
    parse_signed_int(args, IntKind::I16, "Int16")
}
pub(super) fn native_int32_parse(args: &[Value]) -> Result<Value, String>
{
    parse_signed_int(args, IntKind::I32, "Int32")
}
pub(super) fn native_int128_parse(args: &[Value]) -> Result<Value, String>
{
    parse_signed_int(args, IntKind::I128, "Int128")
}

pub(super) fn native_uint8_parse(args: &[Value]) -> Result<Value, String>
{
    parse_unsigned_int(args, IntKind::U8, "Uint8")
}
pub(super) fn native_uint16_parse(args: &[Value]) -> Result<Value, String>
{
    parse_unsigned_int(args, IntKind::U16, "Uint16")
}
pub(super) fn native_uint32_parse(args: &[Value]) -> Result<Value, String>
{
    parse_unsigned_int(args, IntKind::U32, "Uint32")
}
pub(super) fn native_uint64_parse(args: &[Value]) -> Result<Value, String>
{
    parse_unsigned_int(args, IntKind::U64, "Uint64")
}
pub(super) fn native_uint128_parse(args: &[Value]) -> Result<Value, String>
{
    parse_unsigned_int(args, IntKind::U128, "Uint128")
}

pub(super) fn build_int64_module() -> Value
{
    let mut int64_map = FxHashMap::default();
    int64_map.insert(intern::intern("parse"), Value::NativeFunction(native_int64_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(int64_map))))
}

pub(super) fn build_int8_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_int8_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub(super) fn build_int16_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_int16_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub(super) fn build_int32_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_int32_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub(super) fn build_int128_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_int128_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub(super) fn build_uint8_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint8_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub(super) fn build_uint16_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint16_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub(super) fn build_uint32_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint32_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub(super) fn build_uint64_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint64_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub(super) fn build_uint128_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint128_parse));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub(super) fn build_float32_module() -> Value
{
    let mut float32_map = FxHashMap::default();
    float32_map.insert(intern::intern("parse"), Value::NativeFunction(native_float32_parse));
    float32_map.insert(intern::intern("sqrt"), Value::NativeFunction(native_float32_sqrt));
    Value::Map(Rc::new(RefCell::new(MapValue::new(float32_map))))
}

pub(super) fn build_float64_module() -> Value
{
    let mut float64_map = FxHashMap::default();
    float64_map.insert(intern::intern("parse"), Value::NativeFunction(native_float64_parse));
    float64_map.insert(intern::intern("sqrt"), Value::NativeFunction(native_float64_sqrt));
    Value::Map(Rc::new(RefCell::new(MapValue::new(float64_map))))
}

pub(super) fn build_float128_module() -> Value
{
    let mut float128_map = FxHashMap::default();
    float128_map.insert(intern::intern("parse"), Value::NativeFunction(native_float128_parse));
    float128_map.insert(intern::intern("sqrt"), Value::NativeFunction(native_float128_sqrt));
    Value::Map(Rc::new(RefCell::new(MapValue::new(float128_map))))
}

pub(super) fn build_std_module() -> Value
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

impl Interpreter
{
    pub(super) fn call_builtin(&mut self, builtin: &Builtin, args: &[Value]) -> EvalResult
    {
        match builtin
        {
            Builtin::Puts | Builtin::Print | Builtin::Eputs | Builtin::Eprint =>
            {
                let writer = match builtin
                {
                    Builtin::Puts | Builtin::Print => &mut self.stdout,
                    _ => &mut self.stderr,
                };
                let newline = matches!(builtin, Builtin::Puts | Builtin::Eputs);
                let mut last = Value::Nil;
                for arg in args
                {
                    let result = if newline
                    {
                        writeln!(writer, "{}", arg)
                    }
                    else
                    {
                        write!(writer, "{}", arg).and_then(|_| writer.flush())
                    };
                    result.map_err(|err| RuntimeError::simple(format!("output write failed: {err}"), 0))?;
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
                    #[cfg(feature = "lib-mmap") ]
                    Value::BytesView(view) => Ok(default_int(view.len as i128)),
                    Value::Map(map) => Ok(default_int(map.borrow().data.len() as i128)),
                    Value::Env(env) => Ok(default_int(env.data.len() as i128)),
                    #[cfg(feature = "lib-mmap") ]
                    Value::Mmap(mmap) => Ok(default_int(mmap.len() as i128)),
                    #[cfg(feature = "lib-mmap") ]
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
                    #[cfg(feature = "lib-mmap") ]
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
                    #[cfg(feature = "lib-polars") ]
                    Value::DataFrame(_) => "DataFrame",
                    #[cfg(feature = "lib-sqlite") ]
                    Value::Sqlite(_) => "Sqlite",
                    #[cfg(feature = "lib-mmap") ]
                    Value::Mmap(_) => "Mmap",
                    #[cfg(feature = "lib-mmap") ]
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
}

