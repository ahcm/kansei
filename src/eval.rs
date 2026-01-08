use crate::ast::{Expr, Op};
use crate::value::Value;
use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};
use std::process::Command;

#[derive(Clone)]
struct UserFunction {
    params: Vec<String>,
    body: Expr,
}

pub struct Interpreter {
    // Stack of environments. Index 0 is global.
    scopes: Vec<HashMap<String, Value>>,
    functions: HashMap<String, UserFunction>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            functions: HashMap::new(),
        }
    }

    fn get_var(&self, name: &str) -> Value {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return val.clone();
            }
        }
        panic!("Undefined variable: {}", name);
    }

    fn set_var(&mut self, name: String, val: Value) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, val);
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Integer(i) => Value::Integer(*i),
            Expr::String(s) => Value::String(s.clone()),
            Expr::Boolean(b) => Value::Boolean(*b),
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
            Expr::Identifier(name) => self.get_var(name),
            Expr::Assignment { name, value } => {
                let val = self.eval(value);
                self.set_var(name.clone(), val.clone());
                val
            }
            Expr::FunctionDef { name, params, body } => {
                let func = UserFunction {
                    params: params.clone(),
                    body: *body.clone(),
                };
                self.functions.insert(name.clone(), func);
                Value::Nil
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
                    // Force key to string for simplicity
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
                    Value::String(s) => {
                         // Optional: String indexing
                         if let Value::Integer(idx) = index_val {
                            let i = idx as usize;
                            if idx >= 0 && i < s.len() {
                                // Simplified: assumes ASCII/byte indexing or char?
                                // Rust strings are UTF-8. 
                                // Let's skip string indexing for now to avoid complexity or do chars().nth()
                                s.chars().nth(i).map(|c| Value::String(c.to_string())).unwrap_or(Value::Nil)
                            } else {
                                Value::Nil
                            }
                         } else {
                             panic!("String index must be an integer");
                         }
                    }
                    _ => panic!("Index operator not supported on this type"),
                }
            }
            Expr::Call { function, args } => {
                // 1. Check User Functions
                if let Some(func) = self.functions.get(function).cloned() {
                    let arg_vals: Vec<Value> = args.iter().map(|a| self.eval(a)).collect();

                    if arg_vals.len() != func.params.len() {
                        panic!(
                            "Arity mismatch for '{}': expected {}, got {}",
                            function,
                            func.params.len(),
                            arg_vals.len()
                        );
                    }

                    let mut new_scope = HashMap::new();
                    for (param, val) in func.params.iter().zip(arg_vals.into_iter()) {
                        new_scope.insert(param.clone(), val);
                    }

                    self.scopes.push(new_scope);
                    let result = self.eval(&func.body);
                    self.scopes.pop();
                    return result;
                }

                // 2. Built-ins
                match function.as_str() {
                    "puts" | "print" => {
                        let mut last_val = Value::Nil;
                        for arg in args {
                            let val = self.eval(arg);
                            if function == "puts" {
                                println!("{}", val);
                            } else {
                                print!("{}", val);
                                io::stdout().flush().unwrap();
                            }
                            last_val = val;
                        }
                        last_val
                    }
                    "len" => {
                        if args.len() != 1 {
                             panic!("len() requires 1 argument");
                        }
                        let val = self.eval(&args[0]);
                        match val {
                            Value::String(s) => Value::Integer(s.len() as i64),
                            Value::Array(arr) => Value::Integer(arr.len() as i64),
                            Value::Map(map) => Value::Integer(map.len() as i64),
                            _ => Value::Integer(0), // or panic
                        }
                    }
                    "read_file" => {
                        if args.is_empty() {
                            panic!("read_file requires 1 argument");
                        }
                        let path = self.eval(&args[0]).to_string();
                        match fs::read_to_string(&path) {
                            Ok(content) => Value::String(content),
                            Err(_) => Value::Nil,
                        }
                    }
                    "write_file" => {
                        if args.len() < 2 {
                            panic!("write_file requires 2 arguments");
                        }
                        let path = self.eval(&args[0]).to_string();
                        let content = self.eval(&args[1]).to_string();
                        match fs::File::create(&path) {
                            Ok(mut file) => {
                                write!(file, "{}", content).unwrap();
                                Value::Boolean(true)
                            }
                            Err(_) => Value::Boolean(false),
                        }
                    }
                    _ => panic!("Unknown function: {}", function),
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
                            self.set_var(var.clone(), item);
                            last_val = self.eval(body);
                        }
                    }
                    Value::Map(map) => {
                        for key in map.keys() {
                            self.set_var(var.clone(), Value::String(key.clone()));
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
