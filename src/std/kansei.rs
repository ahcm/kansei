use crate::ast::Expr;
use crate::intern;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::sexpr::{expr_to_sexpr, parse_sexpr, sexpr_to_expr, sexpr_to_value, value_to_sexpr};
use crate::source::expr_to_source;
use crate::value::{MapValue, Value};
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
    let arg = args.get(0).ok_or_else(|| "ast.to_sexpr expects a value".to_string())?;
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

fn native_ast_to_source(args: &[Value]) -> Result<Value, String>
{
    let arg = args.get(0).ok_or_else(|| "ast.to_source expects a value".to_string())?;
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

pub fn build_kansei_module() -> Value
{
    let mut ast_map = FxHashMap::default();
    ast_map.insert(intern::intern("to_sexpr"), Value::NativeFunction(native_ast_to_sexpr));
    ast_map.insert(
        intern::intern("from_sexpr"),
        Value::NativeFunction(native_ast_from_sexpr),
    );
    ast_map.insert(
        intern::intern("from_source"),
        Value::NativeFunction(native_ast_from_source),
    );
    ast_map.insert(
        intern::intern("to_source"),
        Value::NativeFunction(native_ast_to_source),
    );

    let mut value_map = FxHashMap::default();
    value_map.insert(
        intern::intern("to_sexpr"),
        Value::NativeFunction(native_value_to_sexpr),
    );
    value_map.insert(
        intern::intern("from_sexpr"),
        Value::NativeFunction(native_value_from_sexpr),
    );

    let mut root = FxHashMap::default();
    root.insert(
        intern::intern("ast"),
        Value::Map(Rc::new(RefCell::new(MapValue::new(ast_map)))),
    );
    root.insert(
        intern::intern("value"),
        Value::Map(Rc::new(RefCell::new(MapValue::new(value_map)))),
    );
    Value::Map(Rc::new(RefCell::new(MapValue::new(root))))
}
