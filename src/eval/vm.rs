//! Bytecode execution and runtime lookup caches.
use super::*;

pub(super) fn eval_map_index_cached(
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

pub(super) fn resolve_method_value(
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

pub(super) fn eval_index_cached_value(
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
        #[cfg(feature = "lib-mmap") ]
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
        #[cfg(feature = "lib-mmap") ]
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
        #[cfg(feature = "lib-mmap") ]
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

pub(super) fn eval_index_assign_value(
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
        #[cfg(feature = "lib-mmap") ]
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

pub(super) fn lookup_env_value_with_owner(
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

pub(super) fn lookup_env_value_recursive_with_owner(
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

pub(super) fn load_global_cached(
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

pub(super) fn eval_call_value_cached(
    interpreter: &mut Interpreter,
    func_val: Value,
    arg_vals: smallvec::SmallVec<[Value; 8]>,
    cache: &Rc<RefCell<CallSiteCache>>,
) -> EvalResult
{
    eval_call_value_cached_generic(interpreter, func_val, arg_vals, cache, None)
}

pub(super) fn eval_call_value_cached_with_block(
    interpreter: &mut Interpreter,
    func_val: Value,
    arg_vals: smallvec::SmallVec<[Value; 8]>,
    cache: &Rc<RefCell<CallSiteCache>>,
    block: &Rc<Closure>,
) -> EvalResult
{
    eval_call_value_cached_generic(interpreter, func_val, arg_vals, cache, Some(block))
}

pub(super) fn eval_call_value_cached_generic(
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

pub(super) fn reg_binop_kind(op: RegBinOp) -> BinOpKind
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

pub(super) fn eval_f64_index_cached_value(
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

pub(super) fn eval_map_index_cached_value(
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

pub(super) fn execute_reg_instructions(
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
                        #[cfg(feature = "lib-mmap") ]
                        Value::BytesView(view) => default_int(view.len as i128),
                        Value::Map(map) => default_int(map.borrow().data.len() as i128),
                        Value::Env(env) => default_int(env.data.len() as i128),
                        #[cfg(feature = "lib-mmap") ]
                        Value::Mmap(mmap) => default_int(mmap.len() as i128),
                        #[cfg(feature = "lib-mmap") ]
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

pub(super) fn try_execute_fast_float_reg(fast: &FastRegFunction, slots: &mut [Value]) -> Option<Value>
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

pub(super) fn pop_args_from_stack(
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

pub(super) fn pop_method_target_and_resolve(
    stack: &mut Vec<Value>,
    name: &Rc<String>,
    map_cache: &Rc<RefCell<MapAccessCache>>,
) -> EvalResult
{
    let target_val = stack.pop().ok_or_else(|| RuntimeError::simple("Missing target for method call".to_string(), 0))?;
    resolve_method_value(target_val, name, map_cache)
}

pub(super) fn err_index_requires_int() -> RuntimeError
{
    RuntimeError::simple("Array index must be an integer".to_string(), 0)
}

pub(super) fn err_index_unsupported() -> RuntimeError
{
    RuntimeError::simple("Index operator not supported on this type".to_string(), 0)
}

pub(super) enum RangeEndNum
{
    Int(i64),
    Float(f64),
}

pub(super) fn range_end_value(end: &RangeEnd, slots: &[Value], const_pool: &[Value]) -> Value
{
    match end
    {
        RangeEnd::Slot(s) => slots.get(*s).cloned().unwrap_or(Value::Nil),
        RangeEnd::Const(idx) => const_pool.get(*idx).cloned().unwrap_or(Value::Nil),
    }
}

pub(super) fn range_end_f64(end: &RangeEnd, slots: &[Value], const_pool: &[Value])
-> Result<f64, RuntimeError>
{
    let end_val = range_end_value(end, slots, const_pool);
    match end_val
    {
        Value::Float { value, kind } => Ok(normalize_float_value(value, kind)),
        _ => int_value_as_f64(&end_val).ok_or_else(|| RuntimeError::simple("Range end must be a number".to_string(), 0)),
    }
}

pub(super) fn range_end_num(
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
pub(super) enum HotInstr
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

pub(super) fn build_hot_cache(code: &[Instruction]) -> Rc<Vec<Option<HotInstr>>>
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

pub(super) fn execute_instructions(
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
                        #[cfg(feature = "lib-mmap") ]
                        Value::BytesView(view) => default_int(view.len as i128),
                        Value::Map(map) => default_int(map.borrow().data.len() as i128),
                        Value::Env(env) => default_int(env.data.len() as i128),
                        #[cfg(feature = "lib-mmap") ]
                        Value::Mmap(mmap) => default_int(mmap.len() as i128),
                        #[cfg(feature = "lib-mmap") ]
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
                        #[cfg(feature = "lib-mmap") ]
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
                        #[cfg(feature = "lib-mmap") ]
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
                        #[cfg(feature = "lib-mmap") ]
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

