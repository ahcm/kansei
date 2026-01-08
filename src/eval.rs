use crate::ast::{Expr, Op};
use crate::value::Value;
use std::collections::HashMap;
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

            // The Kansei Magic: Executing shell commands
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
                if function == "puts"
                {
                    let val = self.eval(&args[0]);
                    println!("{}", val);
                    return val; // puts returns the value in this dialect
                }
                panic!("Unknown function: {}", function);
            }

            Expr::BinaryOp { left, op, right } =>
            {
                let l = self.eval(left);
                let r = self.eval(right);
                match (l, r)
                {
                    // Math for Integers
                    (Value::Integer(i1), Value::Integer(i2)) => match op
                    {
                        Op::Add => Value::Integer(i1 + i2),
                        Op::Subtract => Value::Integer(i1 - i2),
                        Op::Multiply => Value::Integer(i1 * i2),
                        Op::Divide => Value::Integer(i1 / i2),
                    },
                    // Concatenation for Strings
                    (Value::String(s1), Value::String(s2)) => match op
                    {
                        Op::Add => Value::String(format!("{}{}", s1, s2)),
                        _ => panic!("You can only add strings!"),
                    },
                    // Mixed (Kansei intuition: convert int to string if added to string)
                    (Value::String(s), Value::Integer(i)) => match op
                    {
                        Op::Add => Value::String(format!("{}{}", s, i)),
                        _ => panic!("Invalid operation"),
                    },
                    _ => Value::Integer(0),
                }
            }
        }
    }
}
