use crate::ast::Expr;
use crate::eval::Interpreter;
use crate::intern;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::sexpr::{expr_to_sexpr, parse_sexpr, sexpr_to_expr, sexpr_to_value, value_to_sexpr};
use crate::source::expr_to_source;
use crate::value::{HostFunction, MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

fn parse_ast(source: &str) -> Result<Expr, String>
{
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    Ok(parser.parse())
}

fn native_ast_to_sexpr(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "ast.to_sexpr expects a value".to_string())?;
    let sexpr = match arg
    {
        Value::String(s) =>
        {
            let ast = parse_ast(s.as_str())?;
            expr_to_sexpr(&ast).to_string()
        }
        Value::Ast(ast) => expr_to_sexpr(ast.as_ref()).to_string(),
        _ => return Err("ast.to_sexpr expects a source string or Ast".to_string()),
    };
    Ok(Value::String(intern::intern_owned(sexpr)))
}

fn native_ast_from_sexpr(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "ast.from_sexpr expects a string".to_string())?;
    let sexpr_str = match arg
    {
        Value::String(s) => s.as_str(),
        _ => return Err("ast.from_sexpr expects a string".to_string()),
    };
    let sexpr = parse_sexpr(sexpr_str)?;
    let ast = sexpr_to_expr(&sexpr)?;
    Ok(Value::Ast(Rc::new(ast)))
}

fn native_ast_from_source(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "ast.from_source expects a string".to_string())?;
    let source = match arg
    {
        Value::String(s) => s.as_str(),
        _ => return Err("ast.from_source expects a string".to_string()),
    };
    let ast = parse_ast(source)?;
    Ok(Value::Ast(Rc::new(ast)))
}

fn native_ast_eval_in(interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>
{
    if args.len() < 2
    {
        return Err("ast.eval_in expects ast, env_map, and optional program".to_string());
    }
    let ast = match args.get(0)
    {
        Some(Value::Ast(ast)) => (*ast.as_ref()).clone(),
        Some(Value::String(s)) => parse_ast(s.as_str())?,
        _ => return Err("ast.eval_in expects Ast or source string".to_string()),
    };
    let env_map = match args.get(1)
    {
        Some(Value::Map(map)) => map.clone(),
        _ => return Err("ast.eval_in expects env_map as a map".to_string()),
    };
    let program_val = args.get(2);
    if let Some(val) = program_val
    {
        match val
        {
            Value::Nil =>
            {}
            Value::Reference(_) =>
            {}
            _ => return Err("ast.eval_in expects &program or nil as third argument".to_string()),
        }
    }

    interpreter
        .eval_ast_in(ast, env_map, program_val.cloned())
        .map_err(|err| err.message)
}

fn native_ast_to_source(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "ast.to_source expects a value".to_string())?;
    let source = match arg
    {
        Value::String(s) =>
        {
            let ast = parse_ast(s.as_str())?;
            expr_to_source(&ast)
        }
        Value::Ast(ast) => expr_to_source(ast.as_ref()),
        _ => return Err("ast.to_source expects a source string or Ast".to_string()),
    };
    Ok(Value::String(intern::intern_owned(source)))
}

fn native_value_to_sexpr(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "value.to_sexpr expects a value".to_string())?;
    let sexpr = value_to_sexpr(arg)?;
    Ok(Value::String(intern::intern_owned(sexpr.to_string())))
}

fn native_value_from_sexpr(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "value.from_sexpr expects a string".to_string())?;
    let sexpr_str = match arg
    {
        Value::String(s) => s.as_str(),
        _ => return Err("value.from_sexpr expects a string".to_string()),
    };
    let sexpr = parse_sexpr(sexpr_str)?;
    sexpr_to_value(&sexpr)
}

fn native_value_inspect(args: &[Value]) -> Result<Value, String>
{
    let arg = args
        .get(0)
        .ok_or_else(|| "value.inspect expects a value".to_string())?;
    Ok(Value::String(intern::intern_owned(arg.inspect())))
}

pub fn build_kansei_module() -> Value
{
    let mut ast_map = FxHashMap::default();
    ast_map.insert(intern::intern("to_sexpr"), Value::NativeFunction(native_ast_to_sexpr));
    ast_map.insert(intern::intern("from_sexpr"), Value::NativeFunction(native_ast_from_sexpr));
    ast_map.insert(intern::intern("from_source"), Value::NativeFunction(native_ast_from_source));
    ast_map.insert(intern::intern("to_source"), Value::NativeFunction(native_ast_to_source));
    ast_map
        .insert(intern::intern("eval_in"), Value::HostFunction(native_ast_eval_in as HostFunction));

    let mut value_map = FxHashMap::default();
    value_map.insert(intern::intern("to_sexpr"), Value::NativeFunction(native_value_to_sexpr));
    value_map.insert(intern::intern("from_sexpr"), Value::NativeFunction(native_value_from_sexpr));
    value_map.insert(intern::intern("inspect"), Value::NativeFunction(native_value_inspect));

    let mut root = FxHashMap::default();
    root.insert(intern::intern("ast"), Value::Map(Rc::new(RefCell::new(MapValue::new(ast_map)))));
    root.insert(
        intern::intern("value"),
        Value::Map(Rc::new(RefCell::new(MapValue::new(value_map)))),
    );
    Value::Map(Rc::new(RefCell::new(MapValue::new(root))))
}
