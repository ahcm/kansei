use crate::ast::{Closure, Expr, Op};
use crate::value::{Environment, Value};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::{self, Write};
use std::process::Command;
use std::rc::Rc;
use std::cell::RefCell;

fn collect_declarations(expr: &Expr, decls: &mut HashSet<String>) {
    match expr {
        Expr::Assignment { name, .. } => {
            decls.insert(name.clone());
        }
        Expr::FunctionDef { name, .. } => {
            decls.insert(name.clone());
        }
        Expr::AnonymousFunction { .. } => {
            // Anonymous functions don't declare a name in the current scope
        }
        Expr::IndexAssignment { target, index, value } => {
            collect_declarations(target, decls);
            collect_declarations(index, decls);
            collect_declarations(value, decls);
        }
        Expr::Block(stmts) => {
            for stmt in stmts {
                collect_declarations(stmt, decls);
            }
        }
        Expr::If { then_branch, else_branch, .. } => {
            collect_declarations(then_branch, decls);
            if let Some(else_expr) = else_branch {
                collect_declarations(else_expr, decls);
            }
        }
        Expr::While { body, .. } => {
            collect_declarations(body, decls);
        }
        Expr::Array(elements) => {
            for e in elements {
                collect_declarations(e, decls);
            }
        }
        Expr::ArrayGenerator { generator, size } => {
            collect_declarations(generator, decls);
            collect_declarations(size, decls);
        }
        Expr::Map(entries) => {
            for (k, v) in entries {
                collect_declarations(k, decls);
                collect_declarations(v, decls);
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

    pub fn eval(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Integer(i) => Value::Integer(*i),
            Expr::String(s) => Value::String(s.clone()),
            Expr::Boolean(b) => Value::Boolean(*b),
            Expr::Nil => Value::Nil,
            Expr::Shell(cmd_str) => {
                let output = if cfg!(target_os = "windows") {
                    Command::new("cmd").args(&["/C", cmd_str]).output()
                } else {
                    Command::new("sh").arg("-c").arg(cmd_str).output()
                };

                match output {
                    Ok(o) => {
                        let res = String::from_utf8_lossy(&o.stdout).to_string();
                        Value::String(res.trim().to_string())
                    }
                    Err(_) => Value::String("".to_string()),
                }
            }
            Expr::Identifier(name) => {
                let val = self.env.borrow().get(name).expect(&format!("Undefined variable: {}", name));
                if let Value::Uninitialized = val {
                    panic!("Variable '{}' used before assignment", name);
                }
                val
            }
            Expr::Reference(name) => {
                self.env.borrow_mut().promote(name).expect(&format!("Undefined variable referenced: {}", name))
            }
            Expr::Assignment { name, value } => {
                let val = self.eval(value);
                self.env.borrow_mut().set(name.clone(), val.clone());
                val
            }
            Expr::IndexAssignment { target, index, value } => {
                let target_val = self.eval(target);
                let index_val = self.eval(index);
                let val = self.eval(value);

                match target_val {
                    Value::Array(arr) => {
                        if let Value::Integer(idx) = index_val {
                            let mut vec = arr.borrow_mut();
                            let i = idx as usize;
                            if idx >= 0 && i < vec.len() {
                                vec[i] = val.clone();
                            } else if idx >= 0 && i == vec.len() {
                                // Support append? Common in scripting, but maybe too implicit.
                                // Let's stick to strict bounds for now, but maybe allow extending by 1.
                                // Python allows `a[i] = x` only if `i < len`.
                                // Ruby allows extending.
                                // Let's allow expanding? No, strict bounds for index assignment usually.
                                // Use `push` method for appending (not implemented yet).
                                // But `a[0] = 1` implies existing index.
                                panic!("Array index out of bounds: {}", idx);
                            } else {
                                panic!("Array index out of bounds: {}", idx);
                            }
                        } else {
                            panic!("Array index must be an integer");
                        }
                    }
                    Value::Map(map) => {
                        let key = match index_val {
                             Value::String(s) => s,
                             _ => index_val.inspect(),
                        };
                        map.borrow_mut().insert(key, val.clone());
                    }
                    _ => panic!("Index assignment not supported on this type"),
                }
                val
            }
            Expr::FunctionDef { name, params, body } => {
                // Capture current env (Closure)
                let func_env = self.env.clone();
                let func = Value::Function {
                    params: params.clone(),
                    body: body.clone(),
                    env: func_env,
                };
                self.env.borrow_mut().define(name.clone(), func.clone());
                func
            }
            Expr::AnonymousFunction { params, body } => {
                let func_env = self.env.clone();
                Value::Function {
                    params: params.clone(),
                    body: body.clone(),
                    env: func_env,
                }
            }
            Expr::Yield(args) => {
                let block_data = self.block_stack.last().cloned();
                
                if let Some(Some((closure, saved_env))) = block_data {
                     let arg_vals: Vec<Value> = args.iter().map(|a| self.eval(a)).collect();
                     
                     // Create new env for block, parent = saved_env.
                     // Access to non-functions in parent is restricted in Environment::get.
                     let new_env = Rc::new(RefCell::new(Environment::new(Some(saved_env.clone()))));
                     
                     // Bind params
                     let mut arg_iter = arg_vals.into_iter();
                     for (param_name, is_ref) in &closure.params {
                         if *is_ref {
                             let ref_val = saved_env.borrow_mut().promote(param_name)
                                 .expect(&format!("Undefined variable captured: {}", param_name));
                             new_env.borrow_mut().define(param_name.clone(), ref_val);
                         } else {
                             let val = arg_iter.next().unwrap_or(Value::Nil);
                             new_env.borrow_mut().define(param_name.clone(), val);
                         }
                     }
                     
                     // Scan for local assignments in block
                     let mut locals = HashSet::new();
                     collect_declarations(&closure.body, &mut locals);
                     for local in locals {
                         if !new_env.borrow().values.contains_key(&local) {
                             new_env.borrow_mut().define(local, Value::Uninitialized);
                         }
                     }
                     
                     // Swap env
                     let original_env = self.env.clone();
                     self.env = new_env;
                     
                     let result = self.eval(&closure.body);
                     
                     // Restore env
                     self.env = original_env;
                     
                     result
                } else {
                    panic!("No block given for yield");
                }
            }
            Expr::Array(elements) => {
                let vals = elements.iter().map(|e| self.eval(e)).collect();
                Value::Array(Rc::new(RefCell::new(vals)))
            }
            Expr::ArrayGenerator { generator, size } => {
                let gen_val = self.eval(generator);
                let size_val = self.eval(size);
                
                let n = match size_val {
                    Value::Integer(i) if i >= 0 => i as usize,
                    _ => panic!("Array size must be a non-negative integer"),
                };
                
                let mut vals = Vec::with_capacity(n);
                
                if let Value::Function { params, body, env: func_env } = gen_val {
                    // It's a generator function
                    for i in 0..n {
                        let new_env = Rc::new(RefCell::new(Environment::new(Some(func_env.clone()))));
                        if params.len() > 0 {
                            let (param_name, _) = &params[0];
                            new_env.borrow_mut().define(param_name.clone(), Value::Integer(i as i64));
                        }
                        
                        // Scan for local assignments
                        let mut locals = HashSet::new();
                        collect_declarations(&body, &mut locals);
                        for local in locals {
                            if new_env.borrow().get_raw_no_deref(&local).is_none() {
                                new_env.borrow_mut().define(local, Value::Uninitialized);
                            }
                        }

                        let original_env = self.env.clone();
                        self.env = new_env;
                        let result = self.eval(&body);
                        self.env = original_env;
                        
                        vals.push(result);
                    }
                } else {
                    // It's a static value
                    for _ in 0..n {
                        vals.push(gen_val.clone());
                    }
                }
                
                Value::Array(Rc::new(RefCell::new(vals)))
            }
            Expr::Map(entries) => {
                let mut map = HashMap::new();
                for (k_expr, v_expr) in entries {
                    let k_val = self.eval(k_expr);
                    let v_val = self.eval(v_expr);
                    let k_str = match k_val {
                        Value::String(s) => s,
                        _ => k_val.inspect(),
                    };
                    map.insert(k_str, v_val);
                }
                Value::Map(Rc::new(RefCell::new(map)))
            }
            Expr::Index { target, index } => {
                let target_val = self.eval(target);
                let index_val = self.eval(index);

                match target_val {
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
                            panic!("Array index must be an integer");
                        }
                    }
                    Value::Map(map) => {
                        let key = match index_val {
                             Value::String(s) => s,
                             _ => index_val.inspect(),
                        };
                        map.borrow().get(&key).cloned().unwrap_or(Value::Nil)
                    }
                    _ => panic!("Index operator not supported on this type"),
                }
            }
            Expr::Call { function, args, block } => {
                // Check for built-ins first (optimization + legacy support)
                if let Expr::Identifier(name) = &**function {
                     match name.as_str() {
                        "puts" | "print" => {
                            let mut last_val = Value::Nil;
                            for arg in args {
                                let val = self.eval(arg);
                                if name == "puts" {
                                    println!("{}", val);
                                } else {
                                    print!("{}", val);
                                    io::stdout().flush().unwrap();
                                }
                                last_val = val;
                            }
                            return last_val;
                        }
                        "len" => {
                            let val = self.eval(&args[0]);
                            return match val {
                                Value::String(s) => Value::Integer(s.len() as i64),
                                Value::Array(arr) => Value::Integer(arr.borrow().len() as i64),
                                Value::Map(map) => Value::Integer(map.borrow().len() as i64),
                                _ => Value::Integer(0),
                            };
                        }
                        "read_file" => {
                            let path = self.eval(&args[0]).to_string();
                            return match fs::read_to_string(&path) {
                                Ok(content) => Value::String(content),
                                Err(_) => Value::Nil,
                            };
                        }
                        "write_file" => {
                            let path = self.eval(&args[0]).to_string();
                            let content = self.eval(&args[1]).to_string();
                            return match fs::File::create(&path) {
                                Ok(mut file) => {
                                    write!(file, "{}", content).unwrap();
                                    Value::Boolean(true)
                                }
                                Err(_) => Value::Boolean(false),
                            };
                        }
                        _ => {} // Fall through to variable lookup
                     }
                }

                // Evaluate function expression (First-Class Functions)
                let func_val = self.eval(function);

                if let Value::Function { params: func_params, body: func_body, env: func_env } = func_val {
                    let mut arg_vals = Vec::new();

                    for (i, arg_expr) in args.iter().enumerate() {
                        let val = self.eval(arg_expr);
                        if i < func_params.len() {
                            let (_, is_ref) = func_params[i];
                            if is_ref {
                                if let Value::Reference(_) = val {
                                    arg_vals.push(val);
                                } else {
                                    panic!("Argument #{} expected to be a reference (&var), but got value", i + 1);
                                }
                            } else {
                                arg_vals.push(val);
                            }
                        } else {
                            arg_vals.push(val);
                        }
                    }

                    if arg_vals.len() < func_params.len() {
                        // CURRYING / PARTIAL APPLICATION
                        // Create new env extending the function's closure
                        // Use new_partial so that these parameters are visible to subsequent calls
                        let new_env = Rc::new(RefCell::new(Environment::new_partial(Some(func_env.clone()))));
                        
                        // Bind provided args
                        for ((param, _), val) in func_params.iter().zip(arg_vals.iter()) {
                             new_env.borrow_mut().define(param.clone(), val.clone());
                        }

                        // Return new function with remaining params
                        let remaining_params = func_params[arg_vals.len()..].to_vec();
                        
                        return Value::Function {
                            params: remaining_params,
                            body: func_body.clone(),
                            env: new_env,
                        };
                    } else if arg_vals.len() > func_params.len() {
                         panic!("Too many arguments");
                    }

                    // FULL CALL
                    let block_entry = if let Some(closure) = block {
                        Some((closure.clone(), self.env.clone()))
                    } else {
                        None
                    };
                    self.block_stack.push(block_entry);

                    let new_env = Rc::new(RefCell::new(Environment::new(Some(func_env.clone()))));
                    for ((param, _), val) in func_params.iter().zip(arg_vals.into_iter()) {
                        new_env.borrow_mut().define(param.clone(), val);
                    }
                    
                    // Scan for local assignments (Python-style scoping)
                    let mut locals = HashSet::new();
                    collect_declarations(&func_body, &mut locals);
                    for local in locals {
                        // Check if it's already defined (as a param in current env OR parent partial envs)
                        if new_env.borrow().get_raw_no_deref(&local).is_none() {
                            new_env.borrow_mut().define(local, Value::Uninitialized);
                        }
                    }

                    let original_env = self.env.clone();
                    self.env = new_env;
                    let result = self.eval(&func_body);
                    self.env = original_env;
                    self.block_stack.pop();
                    
                    result
                } else {
                    panic!("Tried to call a non-function value: {}", func_val);
                }
            }
            Expr::BinaryOp { left, op, right } => {
                let l = self.eval(left);
                let r = self.eval(right);
                match (l, r) {
                    (Value::Integer(i1), Value::Integer(i2)) => match op {
                        Op::Add => Value::Integer(i1 + i2),
                        Op::Subtract => Value::Integer(i1 - i2),
                        Op::Multiply => Value::Integer(i1 * i2),
                        Op::Divide => Value::Integer(i1 / i2),
                        Op::GreaterThan => Value::Boolean(i1 > i2),
                        Op::LessThan => Value::Boolean(i1 < i2),
                        Op::Equal => Value::Boolean(i1 == i2),
                        Op::NotEqual => Value::Boolean(i1 != i2),
                    },
                    (Value::String(s1), Value::String(s2)) => match op {
                        Op::Add => Value::String(format!("{}{}", s1, s2)),
                        Op::Equal => Value::Boolean(s1 == s2),
                        Op::NotEqual => Value::Boolean(s1 != s2),
                        _ => panic!("Invalid operation on two strings"),
                    },
                    (Value::String(s), v2) => match op {
                        Op::Add => Value::String(format!("{}{}", s, v2.inspect())),
                        Op::Equal => Value::Boolean(false),
                        Op::NotEqual => Value::Boolean(true),
                        _ => panic!("Invalid operation between String and {:?}", v2),
                    },
                    (v1, v2) => match op {
                        Op::Equal => Value::Boolean(false),
                        Op::NotEqual => Value::Boolean(true),
                        _ => panic!(
                            "Type mismatch: Cannot operate {:?} on {:?} and {:?}",
                            op, v1, v2
                        ),
                    },
                }
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let val = self.eval(condition);
                let is_truthy = match val {
                    Value::Boolean(false) | Value::Nil => false,
                    _ => true,
                };

                if is_truthy {
                    self.eval(then_branch)
                } else if let Some(else_expr) = else_branch {
                    self.eval(else_expr)
                } else {
                    Value::Nil
                }
            }
            Expr::While { condition, body } => {
                let mut last_val = Value::Nil;
                loop {
                    let cond_val = self.eval(condition);
                    let is_true = match cond_val {
                        Value::Boolean(false) | Value::Nil => false,
                        _ => true,
                    };
                    if !is_true {
                        break;
                    }
                    last_val = self.eval(body);
                }
                last_val
            }
            Expr::For { var, iterable, body } => {
                let iter_val = self.eval(iterable);
                let mut last_val = Value::Nil;

                match iter_val {
                    Value::Array(arr) => {
                        // We clone the vec to avoid holding a borrow across the loop body execution
                        // This allows the body to modify the array (if it has access to it) without panicking
                        let vec = arr.borrow().clone();
                        for item in vec {
                            self.env.borrow_mut().assign(var.clone(), item);
                            last_val = self.eval(body);
                        }
                    }
                    Value::Map(map) => {
                        let keys: Vec<String> = map.borrow().keys().cloned().collect();
                        for key in keys {
                            self.env.borrow_mut().assign(var.clone(), Value::String(key));
                            last_val = self.eval(body);
                        }
                    }
                    _ => panic!("Type is not iterable"),
                }
                last_val
            }
            Expr::Block(statements) => {
                let mut last = Value::Nil;
                for stmt in statements {
                    last = self.eval(stmt);
                }
                last
            }
        }
    }
}
