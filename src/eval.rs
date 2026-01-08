use crate::ast::{Expr, Op};
use crate::value::Value;
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::process::Command;

pub struct Interpreter
{
    variables: HashMap<String, Value>,
}

impl Interpreter
{
    pub fn new() -> Self
    {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Value
    {
        match expr
        {
            Expr::Integer(i) => Value::Integer(*i),
            Expr::String(s) => Value::String(s.clone()),
            Expr::Shell(cmd_str) =>
            {
                let output = if cfg!(target_os = "windows")
                {
                    Command::new("cmd").args(&["/C", cmd_str]).output()
                }
                else
                {
                    Command::new("sh").arg("-c").arg(cmd_str).output()
                };

                match output
                {
                    Ok(o) =>
                    {
                        let res = String::from_utf8_lossy(&o.stdout).to_string();
                        Value::String(res.trim().to_string())
                    }
                    Err(_) => Value::String("".to_string()),
                }
            }
            Expr::Identifier(name) => self
                .variables
                .get(name)
                .expect(&format!("Undefined variable: {}", name))
                .clone(),
            Expr::Assignment { name, value } =>
            {
                let val = self.eval(value);
                self.variables.insert(name.clone(), val.clone());
                val
            }
            Expr::Call { function, args } =>
            {
                match function.as_str()
                {
                    "puts" | "print" =>
                    {
                        let mut last_val = Value::Nil;

                        for arg in args
                        {
                            let val = self.eval(arg);
                            if function == "puts"
                            {
                                println!("{}", val); // Prints with newline
                            }
                            else
                            {
                                print!("{}", val);
                                use std::io::{self, Write};
                                io::stdout().flush().unwrap();
                            }
                            last_val = val;
                        }

                        return last_val; // Ruby's puts returns nil, but returning the last value is fine for us
                    }
                    "read_file" =>
                    {
                        let path = self.eval(&args[0]).to_string(); // Helper needed (see below)
                        match fs::read_to_string(&path)
                        {
                            Ok(content) => Value::String(content),
                            Err(_) => Value::Nil, // Return nil if file not found
                        }
                    }
                    "write_file" =>
                    {
                        let path = self.eval(&args[0]).to_string();
                        let content = self.eval(&args[1]).to_string();
                        match fs::File::create(&path)
                        {
                            Ok(mut file) =>
                            {
                                write!(file, "{}", content).unwrap();
                                Value::Boolean(true)
                            }
                            Err(_) => Value::Boolean(false),
                        }
                    }
                    _ => panic!("Unknown function: {}", function),
                }
            }
            Expr::BinaryOp { left, op, right } =>
            {
                let l = self.eval(left);
                let r = self.eval(right);

                match (l, r)
                {
                    // --- 1. INTEGER OPERATIONS ---
                    (Value::Integer(i1), Value::Integer(i2)) => match op
                    {
                        // Math
                        Op::Add => Value::Integer(i1 + i2),
                        Op::Subtract => Value::Integer(i1 - i2),
                        Op::Multiply => Value::Integer(i1 * i2),
                        Op::Divide => Value::Integer(i1 / i2),

                        // Comparisons (return Booleans)
                        Op::GreaterThan => Value::Boolean(i1 > i2),
                        Op::LessThan => Value::Boolean(i1 < i2),

                        // Equality (Integers)
                        Op::Equal => Value::Boolean(i1 == i2),
                        Op::NotEqual => Value::Boolean(i1 != i2),
                    },

                    // --- 2. STRING OPERATIONS ---
                    (Value::String(s1), Value::String(s2)) => match op
                    {
                        // Concatenation
                        Op::Add => Value::String(format!("{}{}", s1, s2)),

                        // Equality (Strings)
                        Op::Equal => Value::Boolean(s1 == s2),
                        Op::NotEqual => Value::Boolean(s1 != s2),

                        // You could add GreaterThan here for alphabetical sorting if you wanted
                        _ => panic!("Invalid operation on two strings"),
                    },

                    // --- 3. KANSEI SPECIAL: String + Integer ---
                    // "Age: " + 10 => "Age: 10"
                    (Value::String(s), Value::Integer(i)) => match op
                    {
                        Op::Add => Value::String(format!("{}{}", s, i)),
                        _ => panic!("Cannot perform this operation between String and Integer"),
                    },

                    // --- 4. BOOLEAN OPERATIONS ---
                    // mainly just equality checks
                    (Value::Boolean(b1), Value::Boolean(b2)) => match op
                    {
                        Op::Equal => Value::Boolean(b1 == b2),
                        Op::NotEqual => Value::Boolean(b1 != b2),
                        _ => panic!("Invalid operation on booleans"),
                    },

                    // --- 5. UNIVERSAL EQUALITY (Fallback) ---
                    // Handles cases like: 5 == "5" (False), or true == 1 (False)
                    (v1, v2) => match op
                    {
                        Op::Equal => Value::Boolean(false), // different types are never equal
                        Op::NotEqual => Value::Boolean(true), // different types are always not equal
                        _ => panic!(
                            "Type mismatch: Cannot operate {:?} on {:?} and {:?}",
                            op, v1, v2
                        ),
                    },
                }
            }
            Expr::Boolean(b) => Value::Boolean(*b),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } =>
            {
                let val = self.eval(condition);

                // Ruby-style truthiness: only 'false' and 'nil' are false.
                let is_truthy = match val
                {
                    Value::Boolean(false) | Value::Nil => false,
                    _ => true,
                };

                if is_truthy
                {
                    self.eval(then_branch)
                }
                else if let Some(else_expr) = else_branch
                {
                    self.eval(else_expr)
                }
                else
                {
                    Value::Nil
                }
            }
            Expr::While { condition, body } =>
            {
                // We default to Nil if the loop never runs
                let mut last_val = Value::Nil;

                loop
                {
                    let cond_val = self.eval(condition);

                    let is_true = match cond_val
                    {
                        Value::Boolean(false) | Value::Nil => false,
                        _ => true,
                    };

                    if !is_true
                    {
                        break;
                    }

                    last_val = self.eval(body);
                }

                last_val
            }
            Expr::Block(statements) =>
            {
                let mut last = Value::Nil;
                for stmt in statements
                {
                    last = self.eval(stmt);
                }
                last
            }
        }
    }
}
