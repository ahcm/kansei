use crate::ast::{Closure, Expr, ExprKind, Op};
use crate::value::{Environment, Value};
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

fn collect_declarations(expr: &Expr, decls: &mut HashSet<String>) {
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

fn resolve(expr: &mut Expr, slot_map: &FxHashMap<String, usize>) {
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
        ExprKind::For { iterable, body, .. } => {
            resolve(iterable, slot_map);
            resolve(body, slot_map);
        }
        ExprKind::Call { function, args, block } => {
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
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new(None))),
            block_stack: Vec::new(),
        }
    }

    pub fn define_global(&mut self, name: String, val: Value) {
        self.env.borrow_mut().define(name, val);
    }

    pub fn eval(&mut self, expr: &Expr) -> EvalResult {
        let line = expr.line;
        match &expr.kind {
            ExprKind::Integer(i) => Ok(Value::Integer(*i)),
            ExprKind::Float(f) => Ok(Value::Float(*f)),
            ExprKind::String(s) => Ok(Value::String(Rc::new(s.clone()))),
            ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
            ExprKind::Nil => Ok(Value::Nil),
            ExprKind::Shell(cmd_str) => {
                let output = if cfg!(target_os = "windows") {
                    Command::new("cmd").args(&["/C", cmd_str]).output()
                } else {
                    Command::new("sh").arg("-c").arg(cmd_str).output()
                };

                match output {
                    Ok(o) => {
                        let res = String::from_utf8_lossy(&o.stdout).to_string();
                        Ok(Value::String(Rc::new(res.trim().to_string())))
                    }
                    Err(_) => Ok(Value::String(Rc::new("".to_string()))),
                }
            }
            ExprKind::Identifier { name, slot } => {
                if let Some(s) = slot {
                    if let Some(val) = self.env.borrow().get_slot(*s) {
                        if let Value::Uninitialized = val {
                            // Fallback to name-based lookup (e.g., for currying)
                        } else {
                            return Ok(val);
                        }
                    }
                }
                let val = self.env.borrow().get(name).ok_or_else(|| RuntimeError {
                    message: format!("Undefined variable: {}", name),
                    line,
                })?;
                if let Value::Uninitialized = val {
                    return Err(RuntimeError {
                        message: format!("Variable '{}' used before assignment", name),
                        line,
                    });
                }
                Ok(val)
            }
            ExprKind::Reference(name) => {
                let val = self.env.borrow_mut().promote(name).ok_or_else(|| RuntimeError {
                    message: format!("Undefined variable referenced: {}", name),
                    line,
                })?;
                Ok(val)
            }
            ExprKind::Assignment { name, value, slot } => {
                let val = self.eval(value)?;
                if let Some(s) = slot {
                    self.env.borrow_mut().set_slot(*s, val.clone());
                } else {
                    self.env.borrow_mut().set(name.clone(), val.clone());
                }
                Ok(val)
            }
            ExprKind::IndexAssignment { target, index, value } => {
                let target_val = self.eval(target)?;
                let index_val = self.eval(index)?;
                let val = self.eval(value)?;

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
                             _ => Rc::new(index_val.inspect()),
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
            ExprKind::FunctionDef { name, params, body } => {
                let func_env = self.env.clone();
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);

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

                let mut resolved_body = body.clone();
                resolve(&mut resolved_body, &slot_map);

                let func = Value::Function {
                    params: params.clone(),
                    body: resolved_body,
                    declarations: Rc::new(slot_names),
                    param_offset: 0,
                    env: func_env,
                };
                self.env.borrow_mut().define(name.clone(), func.clone());
                Ok(func)
            }
            ExprKind::AnonymousFunction { params, body } => {
                let func_env = self.env.clone();
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);

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

                let mut resolved_body = body.clone();
                resolve(&mut resolved_body, &slot_map);

                Ok(Value::Function {
                    params: params.clone(),
                    body: resolved_body,
                    declarations: Rc::new(slot_names),
                    param_offset: 0,
                    env: func_env,
                })
            }
            ExprKind::Yield(args) => {
                let block_data = self.block_stack.last().cloned();
                
                if let Some(Some((closure, saved_env))) = block_data {
                     let mut arg_vals = Vec::new();
                     for a in args {
                         arg_vals.push(self.eval(a)?);
                     }
                     
                     let new_env = Rc::new(RefCell::new(Environment::new(Some(saved_env.clone()))));
                     
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
                     self.env = new_env;
                     let result = self.eval(&closure.body)?;
                     self.env = original_env;
                     Ok(result)
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
                    vals.push(self.eval(e)?);
                }
                Ok(Value::Array(Rc::new(RefCell::new(vals))))
            }
            ExprKind::ArrayGenerator { generator, size } => {
                let gen_val = self.eval(generator)?;
                let size_val = self.eval(size)?;
                let n = match size_val {
                    Value::Integer(i) if i >= 0 => i as usize,
                    _ => return Err(RuntimeError {
                        message: "Array size must be a non-negative integer".to_string(),
                        line,
                    }),
                };
                let mut vals = Vec::with_capacity(n);
                if let Value::Function { params, body, declarations: func_decls, param_offset, env: func_env } = gen_val {
                    for i in 0..n {
                        let new_env = Rc::new(RefCell::new(Environment::new(Some(func_env.clone()))));
                        new_env.borrow_mut().slots = vec![Value::Uninitialized; func_decls.len()];
                        if params.len() > 0 {
                            new_env.borrow_mut().set_slot(param_offset, Value::Integer(i as i64));
                        }
                        let original_env = self.env.clone();
                        self.env = new_env;
                        let result = self.eval(&body)?;
                        self.env = original_env;
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
                    let k_val = self.eval(k_expr)?;
                    let v_val = self.eval(v_expr)?;
                    let k_str = match k_val {
                        Value::String(s) => s,
                        _ => Rc::new(k_val.inspect()),
                    };
                    map.insert(k_str, v_val);
                }
                Ok(Value::Map(Rc::new(RefCell::new(map))))
            }
            ExprKind::Index { target, index } => {
                let target_val = self.eval(target)?;
                let index_val = self.eval(index)?;
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
                             _ => Rc::new(index_val.inspect()),
                        };
                        Ok(map.borrow().get(&key).cloned().unwrap_or(Value::Nil))
                    }
                    _ => Err(RuntimeError {
                        message: "Index operator not supported on this type".to_string(),
                        line,
                    }),
                }
            }
            ExprKind::Call { function, args, block } => {
                if let ExprKind::Identifier { name, .. } = &function.kind {
                     match name.as_str() {
                        "puts" | "print" => {
                            let mut last_val = Value::Nil;
                            for arg in args {
                                let val = self.eval(arg)?;
                                if name == "puts" {
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
                            let val = self.eval(&args[0])?;
                            return match val {
                                Value::String(s) => Ok(Value::Integer(s.len() as i64)),
                                Value::Array(arr) => Ok(Value::Integer(arr.borrow().len() as i64)),
                                Value::Map(map) => Ok(Value::Integer(map.borrow().len() as i64)),
                                _ => Ok(Value::Integer(0)),
                            };
                        }
                        "read_file" => {
                            let path = self.eval(&args[0])?.to_string();
                            return match fs::read_to_string(&path) {
                                Ok(content) => Ok(Value::String(Rc::new(content))),
                                Err(_) => Ok(Value::Nil),
                            };
                        }
                        "write_file" => {
                            let path = self.eval(&args[0])?.to_string();
                            let content = self.eval(&args[1])?.to_string();
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

                let func_val = self.eval(function)?;
                if let Value::Function { params: func_params, body: func_body, declarations: func_decls, param_offset, env: func_env } = func_val {
                    let mut arg_vals = Vec::new();
                    for (i, arg_expr) in args.iter().enumerate() {
                        let val = self.eval(arg_expr)?;
                        if i < func_params.len() {
                            let (_, is_ref) = func_params[i];
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

                    if arg_vals.len() < func_params.len() {
                        let new_env = Rc::new(RefCell::new(Environment::new_partial(Some(func_env.clone()))));
                        for ((param, _), val) in func_params.iter().zip(arg_vals.iter()) {
                             new_env.borrow_mut().define(param.clone(), val.clone());
                        }
                        let num_bound = arg_vals.len();
                        let remaining_params = func_params[num_bound..].to_vec();
                        return Ok(Value::Function {
                            params: remaining_params,
                            body: func_body.clone(),
                            declarations: func_decls.clone(),
                            param_offset: param_offset + num_bound,
                            env: new_env,
                        });
                    } else if arg_vals.len() > func_params.len() {
                         return Err(RuntimeError { message: "Too many arguments".to_string(), line });
                    }

                    let block_entry = if let Some(closure) = block {
                        Some((closure.clone(), self.env.clone()))
                    } else {
                        None
                    };
                    self.block_stack.push(block_entry);

                    let new_env = Rc::new(RefCell::new(Environment::new(Some(func_env.clone()))));
                    new_env.borrow_mut().slots = vec![Value::Uninitialized; func_decls.len()];
                    for (i, val) in arg_vals.into_iter().enumerate() {
                        new_env.borrow_mut().set_slot(i + param_offset, val);
                    }

                    let original_env = self.env.clone();
                    self.env = new_env;
                    let result = self.eval(&func_body)?;
                    self.env = original_env;
                    self.block_stack.pop();
                    Ok(result)
                } else {
                    Err(RuntimeError { message: format!("Tried to call a non-function value: {}", func_val), line })
                }
            }
            ExprKind::BinaryOp { left, op, right } => {
                let l = self.eval(left)?;
                let r = self.eval(right)?;
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
                        Op::Add => Ok(Value::String(Rc::new(format!("{}{}", s1, s2)))),
                        Op::Equal => Ok(Value::Boolean(s1 == s2)),
                        Op::NotEqual => Ok(Value::Boolean(s1 != s2)),
                        _ => Err(RuntimeError { message: "Invalid operation on two strings".to_string(), line }),
                    },
                    (Value::String(s), v2) => match op {
                        Op::Add => Ok(Value::String(Rc::new(format!("{}{}", s, v2.inspect())))),
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
                let val = self.eval(condition)?;
                let is_truthy = match val { Value::Boolean(false) | Value::Nil => false, _ => true };
                if is_truthy { self.eval(then_branch) } else if let Some(else_expr) = else_branch { self.eval(else_expr) } else { Ok(Value::Nil) }
            }
            ExprKind::While { condition, body } => {
                let mut last_val = Value::Nil;
                loop {
                    let cond_val = self.eval(condition)?;
                    let is_true = match cond_val { Value::Boolean(false) | Value::Nil => false, _ => true };
                    if !is_true { break; }
                    last_val = self.eval(body)?;
                }
                Ok(last_val)
            }
            ExprKind::For { var, iterable, body } => {
                let iter_val = self.eval(iterable)?;
                let mut last_val = Value::Nil;
                match iter_val {
                    Value::Array(arr) => {
                        let vec = arr.borrow().clone();
                        for item in vec {
                            self.env.borrow_mut().assign(var.clone(), item);
                            last_val = self.eval(body)?;
                        }
                        Ok(last_val)
                    }
                    Value::Map(map) => {
                        let keys: Vec<Rc<String>> = map.borrow().keys().cloned().collect();
                        for key in keys {
                            self.env.borrow_mut().assign(var.clone(), Value::String(key));
                            last_val = self.eval(body)?;
                        }
                        Ok(last_val)
                    }
                    _ => Err(RuntimeError { message: "Type is not iterable".to_string(), line }),
                }
            }
            ExprKind::Block(statements) => {
                let mut last = Value::Nil;
                for stmt in statements { last = self.eval(stmt)?; }
                Ok(last)
            }
        }
    }
}
