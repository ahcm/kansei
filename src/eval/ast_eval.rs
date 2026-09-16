//! Tree evaluation and host AST entry points.
use super::*;

impl Interpreter
{
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
                        let args = if data.params.is_empty()
                        {
                            smallvec::SmallVec::new()
                        }
                        else
                        {
                            smallvec::smallvec![default_int(i as i128)]
                        };
                        vals.push(self.invoke_function(data.clone(), args, line, None)?);
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
                    if let Some(builtin) = builtin_from_symbol(*name)
                    {
                        let mut values = smallvec::SmallVec::<[Value; 8]>::new();
                        for arg in args { values.push(self.eval(arg, slots)?); }
                        return self.call_builtin(&builtin, &values);
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
                        Op::Equal => Ok(Value::Boolean(v1 == v2)),
                        Op::NotEqual => Ok(Value::Boolean(v1 != v2)),
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
}

impl Interpreter
{
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
