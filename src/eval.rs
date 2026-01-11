use crate::ast::{Closure, Expr, ExprKind, Op};
use crate::intern;
use crate::value::{Builtin, Environment, Instruction, Value};
use rustc_hash::FxHashMap;
use std::collections::HashSet;
use std::fs;
use std::io::{self, Write};
use std::process::Command;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub line: usize,
}

pub type EvalResult = Result<Value, RuntimeError>;

fn collect_declarations(expr: &Expr, decls: &mut HashSet<Rc<String>>) {
    match &expr.kind {
        ExprKind::Assignment { name, .. } => {
            decls.insert(name.clone());
        }
        ExprKind::FunctionDef { name, .. } => {
            decls.insert(name.clone());
        }
        ExprKind::AnonymousFunction { .. } => {
        }
        ExprKind::IndexAssignment { target, index, value } => {
            collect_declarations(target, decls);
            collect_declarations(index, decls);
            collect_declarations(value, decls);
        }
        ExprKind::Block(stmts) => {
            for stmt in stmts {
                collect_declarations(stmt, decls);
            }
        }
        ExprKind::If { then_branch, else_branch, .. } => {
            collect_declarations(then_branch, decls);
            if let Some(else_expr) = else_branch {
                collect_declarations(else_expr, decls);
            }
        }
        ExprKind::While { body, .. } => {
            collect_declarations(body, decls);
        }
        ExprKind::For { var, iterable, body, .. } => {
            decls.insert(var.clone());
            collect_declarations(iterable, decls);
            collect_declarations(body, decls);
        }
        ExprKind::Array(elements) => {
            for e in elements {
                collect_declarations(e, decls);
            }
        }
        ExprKind::ArrayGenerator { generator, size } => {
            collect_declarations(generator, decls);
            collect_declarations(size, decls);
        }
        ExprKind::Map(entries) => {
            for (k, v) in entries {
                collect_declarations(k, decls);
                collect_declarations(v, decls);
            }
        }
        _ => {}
    }
}

fn build_slot_map(params: &[(Rc<String>, bool)], locals: HashSet<Rc<String>>) -> (FxHashMap<Rc<String>, usize>, Vec<Rc<String>>) {
    let mut slot_map = FxHashMap::default();
    let mut slot_names = Vec::new();
    for (p, _) in params {
        if !slot_map.contains_key(p) {
            slot_map.insert(p.clone(), slot_names.len());
            slot_names.push(p.clone());
        }
    }
    for l in locals {
        if !slot_map.contains_key(&l) {
            slot_map.insert(l.clone(), slot_names.len());
            slot_names.push(l);
        }
    }
    (slot_map, slot_names)
}

pub fn resolve_slots(expr: &mut Expr) {
    resolve_functions(expr);
}

fn resolve_functions(expr: &mut Expr) {
    match &mut expr.kind {
        ExprKind::FunctionDef { params, body, slots, .. } => {
            if slots.is_none() {
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);
                let (slot_map, slot_names) = build_slot_map(params, locals);
                resolve(body.as_mut(), &slot_map);
                *slots = Some(Rc::new(slot_names));
            }
            resolve_functions(body);
        }
        ExprKind::AnonymousFunction { params, body, slots } => {
            if slots.is_none() {
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);
                let (slot_map, slot_names) = build_slot_map(params, locals);
                resolve(body.as_mut(), &slot_map);
                *slots = Some(Rc::new(slot_names));
            }
            resolve_functions(body);
        }
        ExprKind::Assignment { value, .. } => resolve_functions(value),
        ExprKind::IndexAssignment { target, index, value } => {
            resolve_functions(target);
            resolve_functions(index);
            resolve_functions(value);
        }
        ExprKind::BinaryOp { left, right, .. } => {
            resolve_functions(left);
            resolve_functions(right);
        }
        ExprKind::Block(stmts) => {
            for stmt in stmts {
                resolve_functions(stmt);
            }
        }
        ExprKind::If { condition, then_branch, else_branch } => {
            resolve_functions(condition);
            resolve_functions(then_branch);
            if let Some(eb) = else_branch {
                resolve_functions(eb);
            }
        }
        ExprKind::While { condition, body } => {
            resolve_functions(condition);
            resolve_functions(body);
        }
        ExprKind::For { iterable, body, .. } => {
            resolve_functions(iterable);
            resolve_functions(body);
        }
        ExprKind::Call { function, args, block, .. } => {
            resolve_functions(function);
            for arg in args {
                resolve_functions(arg);
            }
            if let Some(c) = block {
                resolve_functions(&mut c.body);
            }
        }
        ExprKind::Array(elements) => {
            for e in elements {
                resolve_functions(e);
            }
        }
        ExprKind::ArrayGenerator { generator, size } => {
            resolve_functions(generator);
            resolve_functions(size);
        }
        ExprKind::Map(entries) => {
            for (k, v) in entries {
                resolve_functions(k);
                resolve_functions(v);
            }
        }
        ExprKind::Index { target, index } => {
            resolve_functions(target);
            resolve_functions(index);
        }
        ExprKind::Yield(args) => {
            for a in args {
                resolve_functions(a);
            }
        }
        _ => {}
    }
}

fn uses_environment(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Identifier { slot: None, .. } => true,
        ExprKind::Identifier { .. } => false,
        ExprKind::BinaryOp { left, right, .. } => uses_environment(left) || uses_environment(right),
        ExprKind::If { condition, then_branch, else_branch } => {
            uses_environment(condition) || uses_environment(then_branch) || else_branch.as_ref().map_or(false, |e| uses_environment(e))
        }
        ExprKind::Call { function, args, .. } => uses_environment(function) || args.iter().any(uses_environment),
        // Simple functions (is_simple) only have these constructs roughly. 
        // We can be conservative.
        ExprKind::Integer(_) | ExprKind::Float(_) | ExprKind::String(_) | ExprKind::Boolean(_) | ExprKind::Nil => false,
        _ => true, // Conservative fallback for blocks, loops, etc. if they slipped into is_simple
    }
}

fn builtin_from_name(name: &Rc<String>) -> Option<Builtin> {
    match name.as_str() {
        "puts" => Some(Builtin::Puts),
        "print" => Some(Builtin::Print),
        "len" => Some(Builtin::Len),
        "read_file" => Some(Builtin::ReadFile),
        "write_file" => Some(Builtin::WriteFile),
        _ => None,
    }
}

fn push_const(consts: &mut Vec<Value>, value: Value) -> usize {
    if let Some(idx) = consts.iter().position(|v| v == &value) {
        return idx;
    }
    consts.push(value);
    consts.len() - 1
}

fn compile_expr(expr: &Expr, code: &mut Vec<Instruction>, consts: &mut Vec<Value>, want_value: bool) -> bool {
    match &expr.kind {
        ExprKind::Integer(i) => {
            let idx = push_const(consts, Value::Integer(*i));
            code.push(Instruction::LoadConstIdx(idx));
        }
        ExprKind::Float(f) => {
            let idx = push_const(consts, Value::Float(*f));
            code.push(Instruction::LoadConstIdx(idx));
        }
        ExprKind::Identifier { slot: Some(s), .. } => code.push(Instruction::LoadSlot(*s)),
        ExprKind::Boolean(b) => {
            let idx = push_const(consts, Value::Boolean(*b));
            code.push(Instruction::LoadConstIdx(idx));
        }
        ExprKind::Nil => {
            let idx = push_const(consts, Value::Nil);
            code.push(Instruction::LoadConstIdx(idx));
        }
        ExprKind::Assignment { value, slot: Some(s), .. } => {
            if !compile_expr(value, code, consts, true) { return false; }
            code.push(Instruction::StoreSlot(*s));
        }
        ExprKind::Call { function, args, block, .. } => {
            if block.is_some() {
                return false;
            }
            if let ExprKind::Identifier { name, .. } = &function.kind {
                if let Some(builtin) = builtin_from_name(name) {
                    for arg in args {
                        if !compile_expr(arg, code, consts, true) { return false; }
                    }
                    code.push(Instruction::CallBuiltin(builtin, args.len()));
                    if !want_value {
                        code.push(Instruction::Pop);
                    }
                    return true;
                }
            }
            if !compile_expr(function, code, consts, true) { return false; }
            for arg in args {
                if !compile_expr(arg, code, consts, true) { return false; }
            }
            code.push(Instruction::CallValue(args.len()));
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Index { target, index } => {
            if !compile_expr(target, code, consts, true) { return false; }
            if !compile_expr(index, code, consts, true) { return false; }
            code.push(Instruction::Index);
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::IndexAssignment { target, index, value } => {
            if !compile_expr(target, code, consts, true) { return false; }
            if !compile_expr(index, code, consts, true) { return false; }
            if !compile_expr(value, code, consts, true) { return false; }
            code.push(Instruction::IndexAssign);
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::BinaryOp { left, op, right } => {
            if !compile_expr(left, code, consts, true) { return false; }
            if !compile_expr(right, code, consts, true) { return false; }
            match op {
                Op::Add => code.push(Instruction::Add),
                Op::Subtract => code.push(Instruction::Sub),
                Op::Multiply => code.push(Instruction::Mul),
                Op::Divide => code.push(Instruction::Div),
                Op::Equal => code.push(Instruction::Eq),
                Op::GreaterThan => code.push(Instruction::Gt),
                Op::LessThan => code.push(Instruction::Lt),
                _ => return false,
            }
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Block(stmts) => {
            if stmts.is_empty() {
                if want_value {
                    let idx = push_const(consts, Value::Nil);
                    code.push(Instruction::LoadConstIdx(idx));
                }
            } else {
                let last_idx = stmts.len() - 1;
                for (idx, stmt) in stmts.iter().enumerate() {
                    let is_last = idx == last_idx;
                    if !compile_expr(stmt, code, consts, is_last && want_value) { return false; }
                }
            }
        }
        ExprKind::If { condition, then_branch, else_branch } => {
            if !compile_expr(condition, code, consts, true) { return false; }
            let jump_if_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            if !compile_expr(then_branch, code, consts, want_value) { return false; }
            let jump_end_idx = code.len();
            code.push(Instruction::Jump(usize::MAX));
            let else_target = code.len();
            if let Some(else_expr) = else_branch {
                if !compile_expr(else_expr, code, consts, want_value) { return false; }
            } else {
                if want_value {
                    let idx = push_const(consts, Value::Nil);
                    code.push(Instruction::LoadConstIdx(idx));
                }
            }
            let end_target = code.len();
            code[jump_if_false_idx] = Instruction::JumpIfFalse(else_target);
            code[jump_end_idx] = Instruction::Jump(end_target);
        }
        ExprKind::While { condition, body } => {
            let loop_start = code.len();
            if !compile_expr(condition, code, consts, true) { return false; }
            let jump_if_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            if want_value {
                let nil_idx = push_const(consts, Value::Nil);
                code.push(Instruction::LoadConstIdx(nil_idx)); // placeholder last value
                code.push(Instruction::Pop);
                if !compile_expr(body, code, consts, true) { return false; }
            } else {
                if !compile_expr(body, code, consts, false) { return false; }
            }
            code.push(Instruction::Jump(loop_start));
            let loop_end = code.len();
            code[jump_if_false_idx] = Instruction::JumpIfFalse(loop_end);
            if want_value {
                let nil_idx = push_const(consts, Value::Nil);
                code.push(Instruction::LoadConstIdx(nil_idx));
            }
        }
        ExprKind::For { var_slot: Some(var_slot), iterable, body, .. } => {
            if !compile_expr(iterable, code, consts, true) { return false; }
            let mut body_code = Vec::new();
            if !compile_expr(body, &mut body_code, consts, true) { return false; }
            code.push(Instruction::ForEach { var_slot: *var_slot, body: Rc::new(body_code) });
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Array(elements) => {
            for e in elements {
                if !compile_expr(e, code, consts, true) { return false; }
            }
            code.push(Instruction::MakeArray(elements.len()));
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::ArrayGenerator { generator, size } => {
            if !compile_expr(generator, code, consts, true) { return false; }
            if !compile_expr(size, code, consts, true) { return false; }
            code.push(Instruction::ArrayGen);
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Map(entries) => {
            for (k, v) in entries {
                if !compile_expr(k, code, consts, true) { return false; }
                if !compile_expr(v, code, consts, true) { return false; }
            }
            code.push(Instruction::MakeMap(entries.len()));
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        _ => return false,
    }
    true
}

fn substitute(expr: &Expr, args: &[Expr]) -> Expr {
    match &expr.kind {
        ExprKind::Identifier { slot: Some(s), .. } => {
            if *s < args.len() {
                args[*s].clone()
            } else {
                expr.clone()
            }
        }
        ExprKind::BinaryOp { left, op, right } => Expr {
            kind: ExprKind::BinaryOp {
                left: Box::new(substitute(left, args)),
                op: op.clone(),
                right: Box::new(substitute(right, args)),
            },
            line: expr.line,
        },
        ExprKind::If { condition, then_branch, else_branch } => Expr {
            kind: ExprKind::If {
                condition: Box::new(substitute(condition, args)),
                then_branch: Box::new(substitute(then_branch, args)),
                else_branch: else_branch.as_ref().map(|e| Box::new(substitute(e, args))),
            },
            line: expr.line,
        },
        ExprKind::Call { function, args: call_args, block, inlined_body } => Expr {
            kind: ExprKind::Call {
                function: Box::new(substitute(function, args)),
                args: call_args.iter().map(|a| substitute(a, args)).collect(),
                block: block.clone(), // Blocks shouldn't be here in is_simple, but safe to clone
                inlined_body: inlined_body.clone(),
            },
            line: expr.line,
        },
        // Literals
        _ => expr.clone(),
    }
}

fn execute_instructions(
    interpreter: &mut Interpreter,
    code: &[Instruction],
    const_pool: &[Value],
    slots: &mut [Value],
) -> EvalResult {
    let mut stack = Vec::with_capacity(8);
    let mut ip = 0;
    while ip < code.len() {
        match &code[ip] {
            Instruction::LoadConstIdx(idx) => {
                let val = const_pool.get(*idx).cloned().unwrap_or(Value::Nil);
                stack.push(val);
            }
            Instruction::LoadSlot(s) => stack.push(slots[*s].clone()),
            Instruction::StoreSlot(s) => {
                let val = stack.pop().unwrap();
                if let Some(slot) = slots.get_mut(*s) {
                    *slot = val.clone();
                }
                stack.push(val);
            }
            Instruction::Pop => {
                stack.pop();
            }
            Instruction::JumpIfFalse(target) => {
                let cond = stack.pop().unwrap_or(Value::Nil);
                let is_false = matches!(cond, Value::Boolean(false) | Value::Nil);
                if is_false {
                    ip = *target;
                    continue;
                }
            }
            Instruction::Jump(target) => {
                ip = *target;
                continue;
            }
            Instruction::CallBuiltin(builtin, argc) => {
                if *argc > stack.len() {
                    return Err(RuntimeError { message: "Invalid argument count".to_string(), line: 0 });
                }
                let mut args = Vec::with_capacity(*argc);
                for _ in 0..*argc {
                    args.push(stack.pop().unwrap());
                }
                args.reverse();
                let result = interpreter.call_builtin(builtin, &args)?;
                stack.push(result);
            }
            Instruction::CallValue(argc) => {
                if *argc > stack.len() {
                    return Err(RuntimeError { message: "Invalid argument count".to_string(), line: 0 });
                }
                let mut args = smallvec::SmallVec::<[Value; 8]>::with_capacity(*argc);
                for _ in 0..*argc {
                    args.push(stack.pop().unwrap());
                }
                args.reverse();
                let func_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing function value for call".to_string(),
                    line: 0,
                })?;
                let result = interpreter.call_value(func_val, args, 0, None)?;
                stack.push(result);
            }
            Instruction::ForEach { var_slot, body } => {
                let iter_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing iterable for for-loop".to_string(),
                    line: 0,
                })?;
                let mut last = Value::Nil;
                match iter_val {
                    Value::Array(arr) => {
                        let len = arr.borrow().len();
                        for idx in 0..len {
                            let item = {
                                let vec = arr.borrow();
                                vec[idx].clone()
                            };
                            if let Some(slot) = slots.get_mut(*var_slot) {
                                *slot = item;
                            }
                            last = execute_instructions(interpreter, body, const_pool, slots)?;
                        }
                    }
                    Value::Map(map) => {
                        let map_ref = map.borrow();
                        let mut keys = Vec::with_capacity(map_ref.len());
                        keys.extend(map_ref.keys().cloned());
                        for key in keys {
                            if let Some(slot) = slots.get_mut(*var_slot) {
                                *slot = Value::String(key);
                            }
                            last = execute_instructions(interpreter, body, const_pool, slots)?;
                        }
                    }
                    _ => return Err(RuntimeError { message: "Type is not iterable".to_string(), line: 0 }),
                }
                stack.push(last);
            }
            Instruction::MakeArray(count) => {
                if *count > stack.len() {
                    return Err(RuntimeError { message: "Invalid array length".to_string(), line: 0 });
                }
                let mut elems = Vec::with_capacity(*count);
                for _ in 0..*count {
                    elems.push(stack.pop().unwrap());
                }
                elems.reverse();
                stack.push(Value::Array(Rc::new(RefCell::new(elems))));
            }
            Instruction::MakeMap(count) => {
                let pair_count = count.saturating_mul(2);
                if pair_count > stack.len() {
                    return Err(RuntimeError { message: "Invalid map length".to_string(), line: 0 });
                }
                let mut entries = Vec::with_capacity(*count);
                for _ in 0..*count {
                    let val = stack.pop().unwrap();
                    let key_val = stack.pop().unwrap();
                    entries.push((key_val, val));
                }
                entries.reverse();
                let mut map = FxHashMap::default();
                for (k_val, v_val) in entries {
                    let key = match k_val {
                        Value::String(s) => s,
                        _ => intern::intern_owned(k_val.inspect()),
                    };
                    map.insert(key, v_val);
                }
                stack.push(Value::Map(Rc::new(RefCell::new(map))));
            }
            Instruction::Index => {
                let index_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing index for index expression".to_string(),
                    line: 0,
                })?;
                let target_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing target for index expression".to_string(),
                    line: 0,
                })?;
                let result = match target_val {
                    Value::Array(arr) => {
                        if let Value::Integer(idx) = index_val {
                            let i = idx as usize;
                            let vec = arr.borrow();
                            if idx >= 0 && i < vec.len() {
                                vec[i].clone()
                            } else {
                                Value::Nil
                            }
                        } else {
                            return Err(RuntimeError { message: "Array index must be an integer".to_string(), line: 0 });
                        }
                    }
                    Value::Map(map) => {
                        let key = match index_val {
                             Value::String(s) => s,
                             _ => intern::intern_owned(index_val.inspect()),
                        };
                        map.borrow().get(&key).cloned().unwrap_or(Value::Nil)
                    }
                    _ => return Err(RuntimeError { message: "Index operator not supported on this type".to_string(), line: 0 }),
                };
                stack.push(result);
            }
            Instruction::IndexAssign => {
                let value = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing value for index assignment".to_string(),
                    line: 0,
                })?;
                let index_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing index for index assignment".to_string(),
                    line: 0,
                })?;
                let target_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing target for index assignment".to_string(),
                    line: 0,
                })?;
                match target_val {
                    Value::Array(arr) => {
                        if let Value::Integer(idx) = index_val {
                            let mut vec = arr.borrow_mut();
                            let i = idx as usize;
                            if idx >= 0 && i < vec.len() {
                                vec[i] = value.clone();
                            } else {
                                return Err(RuntimeError { message: format!("Array index out of bounds: {}", idx), line: 0 });
                            }
                        } else {
                            return Err(RuntimeError { message: "Array index must be an integer".to_string(), line: 0 });
                        }
                    }
                    Value::Map(map) => {
                        let key = match index_val {
                             Value::String(s) => s,
                             _ => intern::intern_owned(index_val.inspect()),
                        };
                        map.borrow_mut().insert(key, value.clone());
                    }
                    _ => return Err(RuntimeError { message: "Index assignment not supported on this type".to_string(), line: 0 }),
                }
                stack.push(value);
            }
            Instruction::ArrayGen => {
                let size_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing size for array generator".to_string(),
                    line: 0,
                })?;
                let gen_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing generator for array".to_string(),
                    line: 0,
                })?;
                let n = match size_val {
                    Value::Integer(i) if i >= 0 => i as usize,
                    _ => return Err(RuntimeError { message: "Array size must be a non-negative integer".to_string(), line: 0 }),
                };
                let mut vals = Vec::with_capacity(n);
                if let Value::Function(data) = gen_val {
                    for i in 0..n {
                        let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(Value::Uninitialized, data.declarations.len());
                        if !data.params.is_empty() {
                             new_slots[data.param_offset] = Value::Integer(i as i64);
                        }
                        let result = if let Some(code) = &data.code {
                            execute_instructions(interpreter, code, const_pool, &mut new_slots)?
                        } else if data.uses_env {
                            let new_env = interpreter.get_env(Some(data.env.clone()), false);
                            let original_env = interpreter.env.clone();
                            interpreter.env = new_env.clone();
                            let result = interpreter.eval(&data.body, &mut new_slots)?;
                            interpreter.env = original_env;
                            interpreter.recycle_env(new_env);
                            result
                        } else {
                            interpreter.eval(&data.body, &mut new_slots)?
                        };
                        vals.push(result);
                    }
                } else {
                    for _ in 0..n {
                        vals.push(gen_val.clone());
                    }
                }
                stack.push(Value::Array(Rc::new(RefCell::new(vals))));
            }
            inst => {
                let r = stack.pop().unwrap();
                let l = stack.pop().unwrap();
                let res = match (l, r) {
                    (Value::Integer(i1), Value::Integer(i2)) => match inst {
                        Instruction::Add => Value::Integer(i1 + i2),
                        Instruction::Sub => Value::Integer(i1 - i2),
                        Instruction::Mul => Value::Integer(i1 * i2),
                        Instruction::Div => Value::Integer(i1 / i2),
                        Instruction::Eq => Value::Boolean(i1 == i2),
                        Instruction::Gt => Value::Boolean(i1 > i2),
                        Instruction::Lt => Value::Boolean(i1 < i2),
                        _ => unreachable!(),
                    },
                    (Value::Float(f1), Value::Float(f2)) => match inst {
                        Instruction::Add => Value::Float(f1 + f2),
                        Instruction::Sub => Value::Float(f1 - f2),
                        Instruction::Mul => Value::Float(f1 * f2),
                        Instruction::Div => Value::Float(f1 / f2),
                        Instruction::Eq => Value::Boolean(f1 == f2),
                        Instruction::Gt => Value::Boolean(f1 > f2),
                        Instruction::Lt => Value::Boolean(f1 < f2),
                        _ => unreachable!(),
                    },
                    (Value::Integer(i), Value::Float(f)) => {
                        let f1 = i as f64;
                        match inst {
                            Instruction::Add => Value::Float(f1 + f),
                            Instruction::Sub => Value::Float(f1 - f),
                            Instruction::Mul => Value::Float(f1 * f),
                            Instruction::Div => Value::Float(f1 / f),
                            Instruction::Eq => Value::Boolean(f1 == f),
                            Instruction::Gt => Value::Boolean(f1 > f),
                            Instruction::Lt => Value::Boolean(f1 < f),
                            _ => unreachable!(),
                        }
                    },
                    (Value::Float(f), Value::Integer(i)) => {
                        let f2 = i as f64;
                        match inst {
                            Instruction::Add => Value::Float(f + f2),
                            Instruction::Sub => Value::Float(f - f2),
                            Instruction::Mul => Value::Float(f * f2),
                            Instruction::Div => Value::Float(f / f2),
                            Instruction::Eq => Value::Boolean(f == f2),
                            Instruction::Gt => Value::Boolean(f > f2),
                            Instruction::Lt => Value::Boolean(f < f2),
                            _ => unreachable!(),
                        }
                    },
                    (Value::String(s1), Value::String(s2)) => match inst {
                        Instruction::Add => {
                            let mut out = s1.clone();
                            Rc::make_mut(&mut out).push_str(&s2);
                            Value::String(out)
                        }
                        _ => return Err(RuntimeError { message: "Invalid types for operation".to_string(), line: 0 }),
                    },
                    (Value::String(s), v2) => match inst {
                        Instruction::Add => {
                            let mut out = s.clone();
                            Rc::make_mut(&mut out).push_str(&v2.inspect());
                            Value::String(out)
                        }
                        _ => return Err(RuntimeError { message: "Invalid types for operation".to_string(), line: 0 }),
                    },
                    _ => return Err(RuntimeError { message: "Invalid types for operation".to_string(), line: 0 }),
                };
                stack.push(res);
            }
        }
        ip += 1;
    }
    Ok(stack.pop().unwrap())
}

fn is_simple(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Yield(_) | ExprKind::FunctionDef { .. } | ExprKind::AnonymousFunction { .. } => false,
        ExprKind::Assignment { slot: None, .. } => false,
        ExprKind::Block(stmts) => stmts.iter().all(is_simple),
        ExprKind::If { condition, then_branch, else_branch } => {
            is_simple(condition) && is_simple(then_branch) && else_branch.as_ref().map_or(true, |eb| is_simple(eb))
        }
        ExprKind::While { condition, body } => is_simple(condition) && is_simple(body),
        ExprKind::For { iterable, body, .. } => is_simple(iterable) && is_simple(body),
        ExprKind::BinaryOp { left, right, .. } => is_simple(left) && is_simple(right),
        ExprKind::Call { function, args, block, .. } => {
            if block.is_some() { return false; }
            is_simple(function) && args.iter().all(is_simple)
        }
        ExprKind::Array(elements) => elements.iter().all(is_simple),
        ExprKind::ArrayGenerator { generator, size } => is_simple(generator) && is_simple(size),
        ExprKind::Map(entries) => entries.iter().all(|(k, v)| is_simple(k) && is_simple(v)),
        ExprKind::Index { target, index } => is_simple(target) && is_simple(index),
        ExprKind::IndexAssignment { target, index, value } => is_simple(target) && is_simple(index) && is_simple(value),
        _ => true,
    }
}

fn resolve(expr: &mut Expr, slot_map: &FxHashMap<Rc<String>, usize>) {
    match &mut expr.kind {
        ExprKind::Identifier { name, slot } => {
            if let Some(s) = slot_map.get(name) {
                *slot = Some(*s);
            }
        }
        ExprKind::Assignment { name, value, slot } => {
            if let Some(s) = slot_map.get(name) {
                *slot = Some(*s);
            }
            resolve(value, slot_map);
        }
        ExprKind::BinaryOp { left, right, .. } => {
            resolve(left, slot_map);
            resolve(right, slot_map);
        }
        ExprKind::Block(stmts) => {
            for stmt in stmts {
                resolve(stmt, slot_map);
            }
        }
        ExprKind::If { condition, then_branch, else_branch } => {
            resolve(condition, slot_map);
            resolve(then_branch, slot_map);
            if let Some(eb) = else_branch {
                resolve(eb, slot_map);
            }
        }
        ExprKind::While { condition, body } => {
            resolve(condition, slot_map);
            resolve(body, slot_map);
        }
        ExprKind::For { var, var_slot, iterable, body, .. } => {
            if let Some(s) = slot_map.get(var) {
                *var_slot = Some(*s);
            }
            resolve(iterable, slot_map);
            resolve(body, slot_map);
        }
        ExprKind::Call { function, args, block, .. } => {
            resolve(function, slot_map);
            for arg in args {
                resolve(arg, slot_map);
            }
            if let Some(c) = block {
                resolve(&mut c.body, slot_map);
            }
        }
        ExprKind::Array(elements) => {
            for e in elements {
                resolve(e, slot_map);
            }
        }
        ExprKind::ArrayGenerator { generator, size } => {
            resolve(generator, slot_map);
            resolve(size, slot_map);
        }
        ExprKind::Map(entries) => {
            for (k, v) in entries {
                resolve(k, slot_map);
                resolve(v, slot_map);
            }
        }
        ExprKind::Index { target, index } => {
            resolve(target, slot_map);
            resolve(index, slot_map);
        }
        ExprKind::IndexAssignment { target, index, value } => {
            resolve(target, slot_map);
            resolve(index, slot_map);
            resolve(value, slot_map);
        }
        ExprKind::Yield(args) => {
            for a in args {
                resolve(a, slot_map);
            }
        }
        _ => {}
    }
}

pub struct Interpreter {
    // Current environment (scope)
    env: Rc<RefCell<Environment>>,
    // Stack of blocks passed to currently executing functions.
    block_stack: Vec<Option<(Closure, Rc<RefCell<Environment>>)>>,
    // Pool of spare environments for reuse
    env_pool: Vec<Rc<RefCell<Environment>>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new(None))),
            block_stack: Vec::new(),
            env_pool: Vec::with_capacity(32),
        }
    }

    fn get_env(&mut self, parent: Option<Rc<RefCell<Environment>>>, is_partial: bool) -> Rc<RefCell<Environment>> {
        if let Some(env_rc) = self.env_pool.pop() {
            env_rc.borrow_mut().reset(parent, is_partial);
            env_rc
        } else {
            Rc::new(RefCell::new(Environment::new(parent)))
        }
    }

    fn recycle_env(&mut self, env_rc: Rc<RefCell<Environment>>) {
        if Rc::strong_count(&env_rc) == 1 {
            if self.env_pool.len() < 128 {
                self.env_pool.push(env_rc);
            }
        }
    }

    pub fn define_global(&mut self, name: Rc<String>, val: Value) {
        self.env.borrow_mut().define(name, val);
    }

    fn call_builtin(&mut self, builtin: &Builtin, args: &[Value]) -> EvalResult {
        match builtin {
            Builtin::Puts => {
                let mut last = Value::Nil;
                for arg in args {
                    println!("{}", arg);
                    last = arg.clone();
                }
                Ok(last)
            }
            Builtin::Print => {
                let mut last = Value::Nil;
                for arg in args {
                    print!("{}", arg);
                    io::stdout().flush().unwrap();
                    last = arg.clone();
                }
                Ok(last)
            }
            Builtin::Len => {
                let val = args.get(0).cloned().unwrap_or(Value::Nil);
                match val {
                    Value::String(s) => Ok(Value::Integer(s.len() as i64)),
                    Value::Array(arr) => Ok(Value::Integer(arr.borrow().len() as i64)),
                    Value::Map(map) => Ok(Value::Integer(map.borrow().len() as i64)),
                    _ => Ok(Value::Integer(0)),
                }
            }
            Builtin::ReadFile => {
                let path = args.get(0).cloned().unwrap_or(Value::Nil).to_string();
                match fs::read_to_string(&path) {
                    Ok(content) => Ok(Value::String(intern::intern_owned(content))),
                    Err(_) => Ok(Value::Nil),
                }
            }
            Builtin::WriteFile => {
                let path = args.get(0).cloned().unwrap_or(Value::Nil).to_string();
                let content = args.get(1).cloned().unwrap_or(Value::Nil).to_string();
                match fs::File::create(&path) {
                    Ok(mut file) => {
                        write!(file, "{}", content).unwrap();
                        Ok(Value::Boolean(true))
                    }
                    Err(_) => Ok(Value::Boolean(false)),
                }
            }
        }
    }

    fn call_value(
        &mut self,
        func_val: Value,
        arg_vals: smallvec::SmallVec<[Value; 8]>,
        line: usize,
        block: Option<Closure>,
    ) -> EvalResult {
        if let Value::Function(data) = func_val {
            self.invoke_function(data, arg_vals, line, block)
        } else {
            Err(RuntimeError { message: format!("Tried to call a non-function value: {}", func_val), line })
        }
    }

    fn invoke_function(
        &mut self,
        data: Rc<crate::value::FunctionData>,
        arg_vals: smallvec::SmallVec<[Value; 8]>,
        line: usize,
        block: Option<Closure>,
    ) -> EvalResult {
        if arg_vals.len() < data.params.len() {
            let new_env = self.get_env(Some(data.env.clone()), true);
            for ((param, _), val) in data.params.iter().zip(arg_vals.iter()) {
                 new_env.borrow_mut().define(param.clone(), val.clone());
            }
            let num_bound = arg_vals.len();
            let remaining_params = data.params[num_bound..].to_vec();
                        return Ok(Value::Function(Rc::new(crate::value::FunctionData {
                            params: remaining_params,
                            body: data.body.clone(),
                            declarations: data.declarations.clone(),
                            param_offset: data.param_offset + num_bound,
                            is_simple: data.is_simple,
                            uses_env: data.uses_env,
                            code: data.code.clone(),
                            const_pool: data.const_pool.clone(),
                            env: new_env,
                        })));
        } else if arg_vals.len() > data.params.len() {
             return Err(RuntimeError { message: "Too many arguments".to_string(), line });
        }

        // FULL CALL
        if let Some(code) = &data.code {
            let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(Value::Uninitialized, data.declarations.len());
            for (i, val) in arg_vals.into_iter().enumerate() {
                new_slots[i + data.param_offset] = val;
            }
            return execute_instructions(self, code, &data.const_pool, &mut new_slots);
        }

        if data.is_simple {
            let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(Value::Uninitialized, data.declarations.len());
            for (i, val) in arg_vals.into_iter().enumerate() {
                new_slots[i + data.param_offset] = val;
            }
            if data.uses_env {
                let original_env = self.env.clone();
                self.env = data.env.clone();
                let result = self.eval(&data.body, &mut new_slots)?;
                self.env = original_env;
                return Ok(result);
            }
            return self.eval(&data.body, &mut new_slots);
        }

        let block_entry = if let Some(closure) = block {
            Some((closure, self.env.clone()))
        } else {
            None
        };
        self.block_stack.push(block_entry);

        let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(Value::Uninitialized, data.declarations.len());
        for (i, val) in arg_vals.into_iter().enumerate() {
            new_slots[i + data.param_offset] = val;
        }

        let result = if data.uses_env {
            let new_env = self.get_env(Some(data.env.clone()), false);
            let original_env = self.env.clone();
            self.env = new_env.clone();
            let result = self.eval(&data.body, &mut new_slots)?;
            self.env = original_env;
            self.recycle_env(new_env);
            result
        } else {
            self.eval(&data.body, &mut new_slots)?
        };
        self.block_stack.pop();
        Ok(result)
    }

    pub fn eval(&mut self, expr: &Expr, slots: &mut [Value]) -> EvalResult {
        let line = expr.line;
        match &expr.kind {
            ExprKind::Integer(i) => Ok(Value::Integer(*i)),
            ExprKind::Float(f) => Ok(Value::Float(*f)),
            ExprKind::String(s) => Ok(Value::String(s.clone())),
            ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
            ExprKind::Nil => Ok(Value::Nil),
            ExprKind::Shell(cmd_str) => {
                let output = if cfg!(target_os = "windows") {
                    Command::new("cmd").args(&["/C", cmd_str.as_str()]).output()
                } else {
                    Command::new("sh").arg("-c").arg(cmd_str.as_str()).output()
                };

                match output {
                    Ok(o) => {
                        let res = String::from_utf8_lossy(&o.stdout).to_string();
                        Ok(Value::String(intern::intern_owned(res.trim().to_string())))
                    }
                    Err(_) => Ok(Value::String(intern::intern_owned("".to_string()))),
                }
            }
            ExprKind::Identifier { name, slot } => {
                if let Some(s) = slot {
                    if let Some(val) = slots.get(*s) {
                        if let Value::Uninitialized = val {
                            // Fallback to name-based lookup (e.g., for currying)
                        } else {
                            return Ok(val.clone());
                        }
                    }
                }
                let val = self.env.borrow().get(name).ok_or_else(|| RuntimeError {
                    message: format!("Undefined variable: {}", name.as_str()),
                    line,
                })?;
                if let Value::Uninitialized = val {
                    return Err(RuntimeError {
                        message: format!("Variable '{}' used before assignment", name.as_str()),
                        line,
                    });
                }
                Ok(val)
            }
            ExprKind::Reference(name) => {
                let val = self.env.borrow_mut().promote(name).ok_or_else(|| RuntimeError {
                    message: format!("Undefined variable referenced: {}", name.as_str()),
                    line,
                })?;
                Ok(val)
            }
            ExprKind::Assignment { name, value, slot } => {
                let val = self.eval(value, slots)?;
                if let Some(s) = slot {
                    if let Some(slot_val) = slots.get_mut(*s) {
                        *slot_val = val.clone();
                    }
                } else {
                    self.env.borrow_mut().set(name.clone(), val.clone());
                }
                Ok(val)
            }
            ExprKind::IndexAssignment { target, index, value } => {
                let target_val = self.eval(target, slots)?;
                let index_val = self.eval(index, slots)?;
                let val = self.eval(value, slots)?;

                match target_val {
                    Value::Array(arr) => {
                        if let Value::Integer(idx) = index_val {
                            let mut vec = arr.borrow_mut();
                            let i = idx as usize;
                            if idx >= 0 && i < vec.len() {
                                vec[i] = val.clone();
                            } else {
                                return Err(RuntimeError {
                                    message: format!("Array index out of bounds: {}", idx),
                                    line,
                                });
                            }
                        } else {
                            return Err(RuntimeError {
                                message: "Array index must be an integer".to_string(),
                                line,
                            });
                        }
                    }
                    Value::Map(map) => {
                        let key = match index_val {
                             Value::String(s) => s,
                             _ => intern::intern_owned(index_val.inspect()),
                        };
                        map.borrow_mut().insert(key, val.clone());
                    }
                    _ => return Err(RuntimeError {
                        message: "Index assignment not supported on this type".to_string(),
                        line,
                    }),
                }
                Ok(val)
            }
        ExprKind::FunctionDef { name, params, body, slots } => {
            let func_env = self.env.clone();
            let (resolved_body, slot_names) = if let Some(slot_names) = slots {
                (body.clone(), slot_names.clone())
            } else {
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);
                let (slot_map, slot_names) = build_slot_map(params, locals);
                let mut resolved = body.clone();
                resolve(resolved.as_mut(), &slot_map);
                (resolved, Rc::new(slot_names))
            };
            let simple = is_simple(&resolved_body);
            let uses_env = uses_environment(&resolved_body);
                
            let mut code = Vec::new();
            let mut const_pool = Vec::new();
            let compiled = if simple { compile_expr(&resolved_body, &mut code, &mut const_pool, true) } else { false };

            let func = Value::Function(Rc::new(crate::value::FunctionData {
                params: params.clone(),
                body: *resolved_body,
                declarations: slot_names,
                param_offset: 0,
                is_simple: simple,
                uses_env,
                code: if compiled { Some(Rc::new(code)) } else { None },
                const_pool: Rc::new(const_pool),
                env: func_env,
            }));
                self.env.borrow_mut().define(name.clone(), func.clone());
                Ok(func)
            }
        ExprKind::AnonymousFunction { params, body, slots } => {
            let func_env = self.env.clone();
            let (resolved_body, slot_names) = if let Some(slot_names) = slots {
                (body.clone(), slot_names.clone())
            } else {
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);
                let (slot_map, slot_names) = build_slot_map(params, locals);
                let mut resolved = body.clone();
                resolve(resolved.as_mut(), &slot_map);
                (resolved, Rc::new(slot_names))
            };
            let simple = is_simple(&resolved_body);
            let uses_env = uses_environment(&resolved_body);
                
            let mut code = Vec::new();
            let mut const_pool = Vec::new();
            let compiled = if simple { compile_expr(&resolved_body, &mut code, &mut const_pool, true) } else { false };

                Ok(Value::Function(Rc::new(crate::value::FunctionData {
                    params: params.clone(),
                    body: *resolved_body,
                    declarations: slot_names,
                    param_offset: 0,
                    is_simple: simple,
                    uses_env,
                    code: if compiled { Some(Rc::new(code)) } else { None },
                    const_pool: Rc::new(const_pool),
                    env: func_env,
                })))
            }
            ExprKind::Yield(args) => {
                let block_data = self.block_stack.last().cloned();
                
                if let Some(Some((closure, saved_env))) = block_data {
                     let mut arg_vals = Vec::new();
                     for a in args {
                         arg_vals.push(self.eval(a, slots)?);
                     }
                     
                     let new_env = self.get_env(Some(saved_env.clone()), false);
                     
                     let mut arg_iter = arg_vals.into_iter();
                     for (param_name, is_ref) in &closure.params {
                         if *is_ref {
                             let ref_val = saved_env.borrow_mut().promote(param_name)
                                 .ok_or_else(|| RuntimeError {
                                     message: format!("Undefined variable captured: {}", param_name),
                                     line,
                                 })?;
                             new_env.borrow_mut().define(param_name.clone(), ref_val);
                         } else {
                             let val = arg_iter.next().unwrap_or(Value::Nil);
                             new_env.borrow_mut().define(param_name.clone(), val);
                         }
                     }
                     
                     let mut locals = HashSet::new();
                     collect_declarations(&closure.body, &mut locals);
                     for local in locals {
                         if !new_env.borrow().values.contains_key(&local) {
                             new_env.borrow_mut().define(local, Value::Uninitialized);
                         }
                     }
                     
                     let original_env = self.env.clone();
                     self.env = new_env.clone();
                     let result = self.eval(&closure.body, &mut []);
                     self.env = original_env;
                     self.recycle_env(new_env);
                     result
                } else {
                    Err(RuntimeError {
                        message: "No block given for yield".to_string(),
                        line,
                    })
                }
            }
            ExprKind::Array(elements) => {
                let mut vals = Vec::new();
                for e in elements {
                    vals.push(self.eval(e, slots)?);
                }
                Ok(Value::Array(Rc::new(RefCell::new(vals))))
            }
            ExprKind::ArrayGenerator { generator, size } => {
                let gen_val = self.eval(generator, slots)?;
                let size_val = self.eval(size, slots)?;
                let n = match size_val {
                    Value::Integer(i) if i >= 0 => i as usize,
                    _ => return Err(RuntimeError {
                        message: "Array size must be a non-negative integer".to_string(),
                        line,
                    }),
                };
                let mut vals = Vec::with_capacity(n);
                if let Value::Function(data) = gen_val {
                    for i in 0..n {
                        let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(Value::Uninitialized, data.declarations.len());
                        if data.params.len() > 0 {
                             new_slots[data.param_offset] = Value::Integer(i as i64);
                        }
                        
                        let result = if let Some(code) = &data.code {
                            execute_instructions(self, code, &data.const_pool, &mut new_slots)?
                        } else if data.uses_env {
                            let new_env = self.get_env(Some(data.env.clone()), false);
                            let original_env = self.env.clone();
                            self.env = new_env.clone();
                            let result = self.eval(&data.body, &mut new_slots)?;
                            self.env = original_env;
                            self.recycle_env(new_env);
                            result
                        } else {
                            self.eval(&data.body, &mut new_slots)?
                        };
                        vals.push(result);
                    }
                } else {
                    for _ in 0..n {
                        vals.push(gen_val.clone());
                    }
                }
                Ok(Value::Array(Rc::new(RefCell::new(vals))))
            }
            ExprKind::Map(entries) => {
                let mut map = FxHashMap::default();
                for (k_expr, v_expr) in entries {
                    let k_val = self.eval(k_expr, slots)?;
                    let v_val = self.eval(v_expr, slots)?;
                    let k_str = match k_val {
                        Value::String(s) => s,
                        _ => intern::intern_owned(k_val.inspect()),
                    };
                    map.insert(k_str, v_val);
                }
                Ok(Value::Map(Rc::new(RefCell::new(map))))
            }
            ExprKind::Index { target, index } => {
                let target_val = self.eval(target, slots)?;
                let index_val = self.eval(index, slots)?;
                match target_val {
                    Value::Array(arr) => {
                        if let Value::Integer(idx) = index_val {
                            let i = idx as usize; 
                            let vec = arr.borrow();
                            if idx >= 0 && i < vec.len() {
                                Ok(vec[i].clone())
                            } else {
                                Ok(Value::Nil)
                            }
                        } else {
                            Err(RuntimeError {
                                message: "Array index must be an integer".to_string(),
                                line,
                            })
                        }
                    }
                    Value::Map(map) => {
                        let key = match index_val {
                             Value::String(s) => s,
                             _ => intern::intern_owned(index_val.inspect()),
                        };
                        Ok(map.borrow().get(&key).cloned().unwrap_or(Value::Nil))
                    }
                    _ => Err(RuntimeError {
                        message: "Index operator not supported on this type".to_string(),
                        line,
                    }),
                }
            }
            ExprKind::Call { function, args, block, inlined_body } => {
                // 1. Check Cached Inlined Body
                if let Some(inlined) = inlined_body.borrow().as_ref() {
                    return self.eval(inlined, slots);
                }

                if let ExprKind::Identifier { name, .. } = &function.kind {
                     match name.as_str() {
                        "puts" | "print" => {
                            let mut last_val = Value::Nil;
                            for arg in args {
                                let val = self.eval(arg, slots)?;
                                if name.as_str() == "puts" {
                                    println!("{}", val);
                                } else {
                                    print!("{}", val);
                                    io::stdout().flush().unwrap();
                                }
                                last_val = val;
                            }
                            return Ok(last_val);
                        }
                        "len" => {
                            let val = self.eval(&args[0], slots)?;
                            return match val {
                                Value::String(s) => Ok(Value::Integer(s.len() as i64)),
                                Value::Array(arr) => Ok(Value::Integer(arr.borrow().len() as i64)),
                                Value::Map(map) => Ok(Value::Integer(map.borrow().len() as i64)),
                                _ => Ok(Value::Integer(0)),
                            };
                        }
                        "read_file" => {
                            let path = self.eval(&args[0], slots)?.to_string();
                            return match fs::read_to_string(&path) {
                                Ok(content) => Ok(Value::String(intern::intern_owned(content))),
                                Err(_) => Ok(Value::Nil),
                            };
                        }
                        "write_file" => {
                            let path = self.eval(&args[0], slots)?.to_string();
                            let content = self.eval(&args[1], slots)?.to_string();
                            return match fs::File::create(&path) {
                                Ok(mut file) => {
                                    write!(file, "{}", content).unwrap();
                                    Ok(Value::Boolean(true))
                                }
                                Err(_) => Ok(Value::Boolean(false)),
                            };
                        }
                        _ => {}
                     }
                }

                let func_val = self.eval(function, slots)?;
                if let Value::Function(data) = func_val {
                    // 2. Attempt JIT Inlining
                    // Inline if:
                    // - Function is simple (no locals/assignments).
                    // - Function does not capture environment (no slot=None identifiers).
                    // - Args are simple expressions (Identifiers/Literals) to avoid code explosion or side-effect duplication.
                    if data.is_simple && inlined_body.borrow().is_none() && !data.uses_env {
                        let safe_args = args.iter().all(|a| matches!(a.kind, ExprKind::Identifier{..} | ExprKind::Integer(_) | ExprKind::Float(_) | ExprKind::Boolean(_)));
                        if safe_args {
                            let inlined = substitute(&data.body, args);
                            inlined_body.replace(Some(inlined));
                            // Run the newly minted inlined body immediately
                            return self.eval(inlined_body.borrow().as_ref().unwrap(), slots);
                        }
                    }

                    let mut arg_vals: smallvec::SmallVec<[Value; 8]> = smallvec::SmallVec::new();
                    for (i, arg_expr) in args.iter().enumerate() {
                        let val = self.eval(arg_expr, slots)?;
                        if i < data.params.len() {
                            let (_, is_ref) = data.params[i];
                            if is_ref {
                                if let Value::Reference(_) = val {
                                    arg_vals.push(val);
                                } else {
                                    return Err(RuntimeError {
                                        message: format!("Argument #{} expected to be a reference (&var), but got value", i + 1),
                                        line,
                                    });
                                }
                            } else {
                                arg_vals.push(val);
                            }
                        } else {
                            arg_vals.push(val);
                        }
                    }

                    self.invoke_function(data, arg_vals, line, block.clone())
                } else {
                    Err(RuntimeError { message: format!("Tried to call a non-function value: {}", func_val), line })
                }
            }
            ExprKind::BinaryOp { left, op, right } => {
                let l = match &left.kind {
                    ExprKind::Integer(i) => Value::Integer(*i),
                    ExprKind::Float(f) => Value::Float(*f),
                    ExprKind::Identifier { slot: Some(s), .. } => {
                        let v = &slots[*s];
                        if let Value::Uninitialized = v { self.eval(left, slots)? } else { v.clone() }
                    }
                    _ => self.eval(left, slots)?,
                };
                let r = match &right.kind {
                    ExprKind::Integer(i) => Value::Integer(*i),
                    ExprKind::Float(f) => Value::Float(*f),
                    ExprKind::Identifier { slot: Some(s), .. } => {
                        let v = &slots[*s];
                        if let Value::Uninitialized = v { self.eval(right, slots)? } else { v.clone() }
                    }
                    _ => self.eval(right, slots)?,
                };
                match (l, r) {
                    (Value::Integer(i1), Value::Integer(i2)) => match op {
                        Op::Add => Ok(Value::Integer(i1 + i2)),
                        Op::Subtract => Ok(Value::Integer(i1 - i2)),
                        Op::Multiply => Ok(Value::Integer(i1 * i2)),
                        Op::Divide => Ok(Value::Integer(i1 / i2)),
                        Op::GreaterThan => Ok(Value::Boolean(i1 > i2)),
                        Op::LessThan => Ok(Value::Boolean(i1 < i2)),
                        Op::Equal => Ok(Value::Boolean(i1 == i2)),
                        Op::NotEqual => Ok(Value::Boolean(i1 != i2)),
                    },
                    (Value::Float(f1), Value::Float(f2)) => match op {
                        Op::Add => Ok(Value::Float(f1 + f2)),
                        Op::Subtract => Ok(Value::Float(f1 - f2)),
                        Op::Multiply => Ok(Value::Float(f1 * f2)),
                        Op::Divide => Ok(Value::Float(f1 / f2)),
                        Op::GreaterThan => Ok(Value::Boolean(f1 > f2)),
                        Op::LessThan => Ok(Value::Boolean(f1 < f2)),
                        Op::Equal => Ok(Value::Boolean(f1 == f2)),
                        Op::NotEqual => Ok(Value::Boolean(f1 != f2)),
                    },
                    (Value::Integer(i), Value::Float(f)) => {
                        let f1 = i as f64;
                        match op {
                            Op::Add => Ok(Value::Float(f1 + f)),
                            Op::Subtract => Ok(Value::Float(f1 - f)),
                            Op::Multiply => Ok(Value::Float(f1 * f)),
                            Op::Divide => Ok(Value::Float(f1 / f)),
                            Op::GreaterThan => Ok(Value::Boolean(f1 > f)),
                            Op::LessThan => Ok(Value::Boolean(f1 < f)),
                            Op::Equal => Ok(Value::Boolean(f1 == f)),
                            Op::NotEqual => Ok(Value::Boolean(f1 != f)),
                        }
                    },
                    (Value::Float(f), Value::Integer(i)) => {
                        let f2 = i as f64;
                        match op {
                            Op::Add => Ok(Value::Float(f + f2)),
                            Op::Subtract => Ok(Value::Float(f - f2)),
                            Op::Multiply => Ok(Value::Float(f * f2)),
                            Op::Divide => Ok(Value::Float(f / f2)),
                            Op::GreaterThan => Ok(Value::Boolean(f > f2)),
                            Op::LessThan => Ok(Value::Boolean(f < f2)),
                            Op::Equal => Ok(Value::Boolean(f == f2)),
                            Op::NotEqual => Ok(Value::Boolean(f != f2)),
                        }
                    },
                    (Value::String(s1), Value::String(s2)) => match op {
                        Op::Add => {
                            let mut out = s1.clone();
                            Rc::make_mut(&mut out).push_str(&s2);
                            Ok(Value::String(out))
                        }
                        Op::Equal => Ok(Value::Boolean(s1 == s2)),
                        Op::NotEqual => Ok(Value::Boolean(s1 != s2)),
                        _ => Err(RuntimeError { message: "Invalid operation on two strings".to_string(), line }),
                    },
                    (Value::String(s), v2) => match op {
                        Op::Add => {
                            let mut out = s.clone();
                            Rc::make_mut(&mut out).push_str(&v2.inspect());
                            Ok(Value::String(out))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError { message: format!("Invalid operation between String and {:?}", v2), line }),
                    },
                    (v1, v2) => match op {
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError { message: format!("Type mismatch: Cannot operate {:?} on {:?} and {:?}", op, v1, v2), line }),
                    },
                }
            }
            ExprKind::If { condition, then_branch, else_branch } => {
                let val = self.eval(condition, slots)?;
                let is_truthy = match val { Value::Boolean(false) | Value::Nil => false, _ => true };
                if is_truthy { self.eval(then_branch, slots) } else if let Some(else_expr) = else_branch { self.eval(else_expr, slots) } else { Ok(Value::Nil) }
            }
            ExprKind::While { condition, body } => {
                let mut last_val = Value::Nil;
                loop {
                    let cond_val = self.eval(condition, slots)?;
                    let is_true = match cond_val { Value::Boolean(false) | Value::Nil => false, _ => true };
                    if !is_true { break; }
                    last_val = self.eval(body, slots)?;
                }
                Ok(last_val)
            }
            ExprKind::For { var, iterable, body, .. } => {
                let iter_val = self.eval(iterable, slots)?;
                let mut last_val = Value::Nil;
                match iter_val {
                    Value::Array(arr) => {
                        let len = arr.borrow().len();
                        for idx in 0..len {
                            let item = {
                                let vec = arr.borrow();
                                vec[idx].clone()
                            };
                            self.env.borrow_mut().assign(var.clone(), item);
                            last_val = self.eval(body, slots)?;
                        }
                        Ok(last_val)
                    }
                    Value::Map(map) => {
                        let map_ref = map.borrow();
                        let mut keys = Vec::with_capacity(map_ref.len());
                        keys.extend(map_ref.keys().cloned());
                        for key in keys {
                            self.env.borrow_mut().assign(var.clone(), Value::String(key));
                            last_val = self.eval(body, slots)?;
                        }
                        Ok(last_val)
                    }
                    _ => Err(RuntimeError { message: "Type is not iterable".to_string(), line }),
                }
            }
            ExprKind::Block(statements) => {
                let mut last = Value::Nil;
                for stmt in statements { last = self.eval(stmt, slots)?; }
                Ok(last)
            }
        }
    }
}
