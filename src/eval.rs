use crate::ast::{Closure, Expr, Op};
use crate::value::{Environment, Value};
use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};
use std::process::Command;
use std::rc::Rc;
use std::cell::RefCell;

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
                self.env.borrow().get(name).expect(&format!("Undefined variable: {}", name))
            }
            Expr::Assignment { name, value } => {
                let val = self.eval(value);
                self.env.borrow_mut().set(name.clone(), val.clone());
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
            Expr::Yield(args) => {
                let block_data = self.block_stack.last().cloned();
                
                if let Some(Some((closure, saved_env))) = block_data {
                     let arg_vals: Vec<Value> = args.iter().map(|a| self.eval(a)).collect();
                     
                     // Create new env for block, parent = saved_env (Lexical Scoping)
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
                Value::Array(vals)
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
                Value::Map(map)
            }
            Expr::Index { target, index } => {
                let target_val = self.eval(target);
                let index_val = self.eval(index);

                match target_val {
                    Value::Array(arr) => {
                        if let Value::Integer(idx) = index_val {
                            let i = idx as usize; 
                            if idx >= 0 && i < arr.len() {
                                arr[i].clone()
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
                        map.get(&key).cloned().unwrap_or(Value::Nil)
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
                                Value::Array(arr) => Value::Integer(arr.len() as i64),
                                Value::Map(map) => Value::Integer(map.len() as i64),
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
                    let arg_vals: Vec<Value> = args.iter().map(|a| self.eval(a)).collect();

                    if arg_vals.len() < func_params.len() {
                        // CURRYING / PARTIAL APPLICATION
                        // Create new env extending the function's closure
                        let new_env = Rc::new(RefCell::new(Environment::new(Some(func_env.clone()))));
                        
                        // Bind provided args
                        for (param, val) in func_params.iter().zip(arg_vals.iter()) {
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
                    for (param, val) in func_params.iter().zip(arg_vals.into_iter()) {
                        new_env.borrow_mut().define(param.clone(), val);
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
                    (Value::String(s), Value::Integer(i)) => match op {
                        Op::Add => Value::String(format!("{}{}", s, i)),
                        _ => panic!("Cannot perform this operation between String and Integer"),
                    },
                    (Value::Boolean(b1), Value::Boolean(b2)) => match op {
                        Op::Equal => Value::Boolean(b1 == b2),
                        Op::NotEqual => Value::Boolean(b1 != b2),
                        _ => panic!("Invalid operation on booleans"),
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
                        for item in arr {
                            self.env.borrow_mut().assign(var.clone(), item);
                            last_val = self.eval(body);
                        }
                    }
                    Value::Map(map) => {
                        for key in map.keys() {
                            self.env.borrow_mut().assign(var.clone(), Value::String(key.clone()));
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
