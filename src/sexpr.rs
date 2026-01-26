use crate::ast::{
    Closure, Expr, ExprKind, FloatKind, FormatPart, FormatSpec, IntKind, Op, Param, ParamType,
    TypeRef,
};
use crate::intern::{self, SymbolId};
use crate::value::{BytesViewSource, EnvValue, Environment, FunctionData, MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum SExpr
{
    Atom(String),
    String(String),
    List(Vec<SExpr>),
}

impl SExpr
{
    pub fn to_string(&self) -> String
    {
        match self
        {
            SExpr::Atom(s) => s.clone(),
            SExpr::String(s) => format!("\"{}\"", escape_string(s)),
            SExpr::List(items) =>
            {
                let mut out = String::from("(");
                for (idx, item) in items.iter().enumerate()
                {
                    if idx > 0
                    {
                        out.push(' ');
                    }
                    out.push_str(&item.to_string());
                }
                out.push(')');
                out
            }
        }
    }
}

fn escape_string(input: &str) -> String
{
    let mut out = String::new();
    for ch in input.chars()
    {
        match ch
        {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out
}

pub fn parse_sexpr(input: &str) -> Result<SExpr, String>
{
    let mut parser = SExprParser::new(input);
    let expr = parser.parse_expr()?;
    parser.skip_ws();
    if !parser.is_eof()
    {
        return Err("Trailing data after S-Expr".to_string());
    }
    Ok(expr)
}

struct SExprParser<'a>
{
    input: &'a [u8],
    idx: usize,
}

impl<'a> SExprParser<'a>
{
    fn new(input: &'a str) -> Self
    {
        Self {
            input: input.as_bytes(),
            idx: 0,
        }
    }

    fn is_eof(&self) -> bool
    {
        self.idx >= self.input.len()
    }

    fn peek(&self) -> Option<u8>
    {
        self.input.get(self.idx).copied()
    }

    fn bump(&mut self) -> Option<u8>
    {
        let b = self.peek()?;
        self.idx += 1;
        Some(b)
    }

    fn skip_ws(&mut self)
    {
        while let Some(b) = self.peek()
        {
            if b.is_ascii_whitespace()
            {
                self.idx += 1;
            }
            else
            {
                break;
            }
        }
    }

    fn parse_expr(&mut self) -> Result<SExpr, String>
    {
        self.skip_ws();
        match self.peek()
        {
            Some(b'(') => self.parse_list(),
            Some(b'"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => Err("Unexpected end of input".to_string()),
        }
    }

    fn parse_list(&mut self) -> Result<SExpr, String>
    {
        self.bump();
        let mut items = Vec::new();
        loop
        {
            self.skip_ws();
            match self.peek()
            {
                Some(b')') =>
                {
                    self.bump();
                    break;
                }
                Some(_) =>
                {
                    let expr = self.parse_expr()?;
                    items.push(expr);
                }
                None => return Err("Unterminated list".to_string()),
            }
        }
        Ok(SExpr::List(items))
    }

    fn parse_string(&mut self) -> Result<SExpr, String>
    {
        self.bump();
        let mut out = String::new();
        while let Some(b) = self.bump()
        {
            match b
            {
                b'"' => return Ok(SExpr::String(out)),
                b'\\' =>
                {
                    let esc = self.bump().ok_or_else(|| "Invalid escape".to_string())?;
                    match esc
                    {
                        b'n' => out.push('\n'),
                        b'r' => out.push('\r'),
                        b't' => out.push('\t'),
                        b'\\' => out.push('\\'),
                        b'"' => out.push('"'),
                        _ => return Err("Invalid escape".to_string()),
                    }
                }
                _ => out.push(b as char),
            }
        }
        Err("Unterminated string".to_string())
    }

    fn parse_atom(&mut self) -> Result<SExpr, String>
    {
        let start = self.idx;
        while let Some(b) = self.peek()
        {
            if b.is_ascii_whitespace() || b == b'(' || b == b')'
            {
                break;
            }
            self.idx += 1;
        }
        let atom = std::str::from_utf8(&self.input[start..self.idx])
            .map_err(|_| "Invalid atom".to_string())?;
        if atom.is_empty()
        {
            return Err("Unexpected token".to_string());
        }
        Ok(SExpr::Atom(atom.to_string()))
    }
}

fn atom<T: Into<String>>(value: T) -> SExpr
{
    SExpr::Atom(value.into())
}

fn list(tag: &str, items: Vec<SExpr>) -> SExpr
{
    let mut out = Vec::with_capacity(items.len() + 1);
    out.push(atom(tag));
    out.extend(items);
    SExpr::List(out)
}

fn expect_list(expr: &SExpr) -> Result<&[SExpr], String>
{
    match expr
    {
        SExpr::List(items) => Ok(items.as_slice()),
        _ => Err("Expected list".to_string()),
    }
}

fn expect_atom(expr: &SExpr) -> Result<&str, String>
{
    match expr
    {
        SExpr::Atom(s) => Ok(s.as_str()),
        _ => Err("Expected atom".to_string()),
    }
}

fn expect_string(expr: &SExpr) -> Result<&str, String>
{
    match expr
    {
        SExpr::String(s) => Ok(s.as_str()),
        _ => Err("Expected string".to_string()),
    }
}

fn parse_bool(expr: &SExpr) -> Result<bool, String>
{
    let value = expect_atom(expr)?;
    match value
    {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => Err("Expected boolean".to_string()),
    }
}

fn parse_usize(expr: &SExpr) -> Result<usize, String>
{
    let value = expect_atom(expr)?;
    value
        .parse::<usize>()
        .map_err(|_| "Expected integer".to_string())
}

fn parse_int_kind(expr: &SExpr) -> Result<IntKind, String>
{
    let value = expect_atom(expr)?;
    match value
    {
        "I8" => Ok(IntKind::I8),
        "I16" => Ok(IntKind::I16),
        "I32" => Ok(IntKind::I32),
        "I64" => Ok(IntKind::I64),
        "I128" => Ok(IntKind::I128),
        "U8" => Ok(IntKind::U8),
        "U16" => Ok(IntKind::U16),
        "U32" => Ok(IntKind::U32),
        "U64" => Ok(IntKind::U64),
        "U128" => Ok(IntKind::U128),
        _ => Err("Unknown int kind".to_string()),
    }
}

fn parse_float_kind(expr: &SExpr) -> Result<FloatKind, String>
{
    let value = expect_atom(expr)?;
    match value
    {
        "F32" => Ok(FloatKind::F32),
        "F64" => Ok(FloatKind::F64),
        "F128" => Ok(FloatKind::F128),
        _ => Err("Unknown float kind".to_string()),
    }
}

fn op_to_atom(op: &Op) -> SExpr
{
    atom(match op
    {
        Op::Add => "Add",
        Op::Subtract => "Subtract",
        Op::Multiply => "Multiply",
        Op::Divide => "Divide",
        Op::Equal => "Equal",
        Op::NotEqual => "NotEqual",
        Op::LessThan => "LessThan",
        Op::GreaterThan => "GreaterThan",
    })
}

fn parse_op(expr: &SExpr) -> Result<Op, String>
{
    match expect_atom(expr)?
    {
        "Add" => Ok(Op::Add),
        "Subtract" => Ok(Op::Subtract),
        "Multiply" => Ok(Op::Multiply),
        "Divide" => Ok(Op::Divide),
        "Equal" => Ok(Op::Equal),
        "NotEqual" => Ok(Op::NotEqual),
        "LessThan" => Ok(Op::LessThan),
        "GreaterThan" => Ok(Op::GreaterThan),
        _ => Err("Unknown op".to_string()),
    }
}

fn symbol_to_atom(symbol: SymbolId) -> SExpr
{
    atom(intern::symbol_name(symbol).as_str().to_string())
}

fn parse_symbol(expr: &SExpr) -> Result<SymbolId, String>
{
    let value = expect_atom(expr)?;
    Ok(intern::intern_symbol(value))
}

fn path_to_sexpr(path: &[SymbolId]) -> SExpr
{
    let mut items = Vec::with_capacity(path.len() + 1);
    items.push(atom("Path"));
    for segment in path
    {
        items.push(symbol_to_atom(*segment));
    }
    SExpr::List(items)
}

fn parse_path(expr: &SExpr) -> Result<Vec<SymbolId>, String>
{
    let items = expect_list(expr)?;
    if items.is_empty() || expect_atom(&items[0])? != "Path"
    {
        return Err("Expected Path".to_string());
    }
    let mut out = Vec::with_capacity(items.len().saturating_sub(1));
    for item in &items[1..]
    {
        out.push(parse_symbol(item)?);
    }
    Ok(out)
}

fn type_ref_to_sexpr(type_ref: &TypeRef) -> SExpr
{
    list("TypeRef", vec![path_to_sexpr(&type_ref.path)])
}

fn parse_type_ref(expr: &SExpr) -> Result<TypeRef, String>
{
    let items = expect_list(expr)?;
    if items.len() != 2 || expect_atom(&items[0])? != "TypeRef"
    {
        return Err("Expected TypeRef".to_string());
    }
    let path = parse_path(&items[1])?;
    Ok(TypeRef { path })
}

fn format_spec_to_sexpr(spec: &Option<FormatSpec>) -> SExpr
{
    if let Some(spec) = spec
    {
        let precision = spec
            .precision
            .map(|v| atom(v.to_string()))
            .unwrap_or_else(|| atom("nil"));
        list("FormatSpec", vec![precision])
    }
    else
    {
        atom("nil")
    }
}

fn parse_format_spec(expr: &SExpr) -> Result<Option<FormatSpec>, String>
{
    if let SExpr::Atom(s) = expr
    {
        if s == "nil"
        {
            return Ok(None);
        }
    }
    let items = expect_list(expr)?;
    if items.len() != 2 || expect_atom(&items[0])? != "FormatSpec"
    {
        return Err("Expected FormatSpec".to_string());
    }
    let precision = if let SExpr::Atom(s) = &items[1]
    {
        if s == "nil"
        {
            None
        }
        else
        {
            Some(
                s.parse::<usize>()
                    .map_err(|_| "Invalid precision".to_string())?,
            )
        }
    }
    else
    {
        return Err("Invalid precision".to_string());
    };
    Ok(Some(FormatSpec { precision }))
}

fn format_part_to_sexpr(part: &FormatPart) -> SExpr
{
    match part
    {
        FormatPart::Literal(text) =>
        {
            list("Literal", vec![SExpr::String(text.as_str().to_string())])
        }
        FormatPart::Expr { expr, spec } =>
        {
            list("ExprPart", vec![expr_to_sexpr(expr), format_spec_to_sexpr(spec)])
        }
    }
}

fn parse_format_part(expr: &SExpr) -> Result<FormatPart, String>
{
    let items = expect_list(expr)?;
    if items.is_empty()
    {
        return Err("Empty format part".to_string());
    }
    match expect_atom(&items[0])?
    {
        "Literal" =>
        {
            if items.len() != 2
            {
                return Err("Invalid Literal part".to_string());
            }
            let text = expect_string(&items[1])?;
            Ok(FormatPart::Literal(Rc::new(text.to_string())))
        }
        "ExprPart" =>
        {
            if items.len() != 3
            {
                return Err("Invalid ExprPart".to_string());
            }
            let expr = parse_expr(&items[1])?;
            let spec = parse_format_spec(&items[2])?;
            Ok(FormatPart::Expr {
                expr: Box::new(expr),
                spec,
            })
        }
        _ => Err("Unknown format part".to_string()),
    }
}

fn param_type_to_sexpr(param_type: &ParamType) -> SExpr
{
    match param_type
    {
        ParamType::Struct(fields) =>
        {
            let mut items = Vec::with_capacity(fields.len() + 1);
            items.push(atom("StructType"));
            for (name, type_ref) in fields
            {
                items.push(list(
                    "FieldType",
                    vec![symbol_to_atom(*name), type_ref_to_sexpr(type_ref)],
                ));
            }
            SExpr::List(items)
        }
    }
}

fn parse_param_type(expr: &SExpr) -> Result<ParamType, String>
{
    let items = expect_list(expr)?;
    if items.is_empty() || expect_atom(&items[0])? != "StructType"
    {
        return Err("Expected StructType".to_string());
    }
    let mut fields = Vec::with_capacity(items.len().saturating_sub(1));
    for item in &items[1..]
    {
        let field_items = expect_list(item)?;
        if field_items.len() != 3 || expect_atom(&field_items[0])? != "FieldType"
        {
            return Err("Invalid FieldType".to_string());
        }
        let name = parse_symbol(&field_items[1])?;
        let type_ref = parse_type_ref(&field_items[2])?;
        fields.push((name, type_ref));
    }
    Ok(ParamType::Struct(fields))
}

fn param_to_sexpr(param: &Param) -> SExpr
{
    let type_ann = match &param.type_ann
    {
        Some(ann) => param_type_to_sexpr(ann),
        None => atom("nil"),
    };
    list(
        "Param",
        vec![
            symbol_to_atom(param.name),
            atom(param.is_ref.to_string()),
            type_ann,
        ],
    )
}

fn parse_param(expr: &SExpr) -> Result<Param, String>
{
    let items = expect_list(expr)?;
    if items.len() != 4 || expect_atom(&items[0])? != "Param"
    {
        return Err("Invalid Param".to_string());
    }
    let name = parse_symbol(&items[1])?;
    let is_ref = parse_bool(&items[2])?;
    let type_ann = match &items[3]
    {
        SExpr::Atom(s) if s == "nil" => None,
        other => Some(parse_param_type(other)?),
    };
    Ok(Param {
        name,
        is_ref,
        type_ann,
    })
}

fn closure_to_sexpr(closure: &Closure) -> SExpr
{
    let params = closure.params.iter().map(param_to_sexpr).collect();
    list("Closure", vec![SExpr::List(params), expr_to_sexpr(&closure.body)])
}

fn parse_closure(expr: &SExpr) -> Result<Closure, String>
{
    let items = expect_list(expr)?;
    if items.len() != 3 || expect_atom(&items[0])? != "Closure"
    {
        return Err("Invalid Closure".to_string());
    }
    let params_list = expect_list(&items[1])?;
    let mut params = Vec::with_capacity(params_list.len());
    for item in params_list
    {
        params.push(parse_param(item)?);
    }
    let body = parse_expr(&items[2])?;
    Ok(Closure {
        params,
        body: Box::new(body),
    })
}

pub fn expr_to_sexpr(expr: &Expr) -> SExpr
{
    let line_atom = atom(expr.line.to_string());
    match &expr.kind
    {
        ExprKind::Integer { value, kind } => list(
            "Integer",
            vec![
                line_atom,
                atom(format!("{:?}", kind)),
                atom(value.to_string()),
            ],
        ),
        ExprKind::Unsigned { value, kind } => list(
            "Unsigned",
            vec![
                line_atom,
                atom(format!("{:?}", kind)),
                atom(value.to_string()),
            ],
        ),
        ExprKind::Float { value, kind } => list(
            "Float",
            vec![
                line_atom,
                atom(format!("{:?}", kind)),
                atom(value.to_string()),
            ],
        ),
        ExprKind::Identifier { name, slot } =>
        {
            let slot_expr = slot
                .map(|s| atom(s.to_string()))
                .unwrap_or_else(|| atom("nil"));
            list("Identifier", vec![line_atom, symbol_to_atom(*name), slot_expr])
        }
        ExprKind::Reference(name) => list("Reference", vec![line_atom, symbol_to_atom(*name)]),
        ExprKind::String(s) =>
        {
            list("String", vec![line_atom, SExpr::String(s.as_str().to_string())])
        }
        ExprKind::Boolean(b) => list("Boolean", vec![line_atom, atom(b.to_string())]),
        ExprKind::Nil => list("Nil", vec![line_atom]),
        ExprKind::Shell(cmd) =>
        {
            list("Shell", vec![line_atom, SExpr::String(cmd.as_str().to_string())])
        }
        ExprKind::Clone(expr) => list("Clone", vec![line_atom, expr_to_sexpr(expr)]),
        ExprKind::EnvFreeze(expr) => list("EnvFreeze", vec![line_atom, expr_to_sexpr(expr)]),
        ExprKind::Not(expr) => list("Not", vec![line_atom, expr_to_sexpr(expr)]),
        ExprKind::And { left, right } =>
        {
            list("And", vec![line_atom, expr_to_sexpr(left), expr_to_sexpr(right)])
        }
        ExprKind::AndBool { left, right } =>
        {
            list("AndBool", vec![line_atom, expr_to_sexpr(left), expr_to_sexpr(right)])
        }
        ExprKind::Or { left, right } =>
        {
            list("Or", vec![line_atom, expr_to_sexpr(left), expr_to_sexpr(right)])
        }
        ExprKind::OrBool { left, right } =>
        {
            list("OrBool", vec![line_atom, expr_to_sexpr(left), expr_to_sexpr(right)])
        }
        ExprKind::BinaryOp { left, op, right } => list(
            "BinaryOp",
            vec![
                line_atom,
                op_to_atom(op),
                expr_to_sexpr(left),
                expr_to_sexpr(right),
            ],
        ),
        ExprKind::Assignment { name, value, slot } =>
        {
            let slot_expr = slot
                .map(|s| atom(s.to_string()))
                .unwrap_or_else(|| atom("nil"));
            list(
                "Assignment",
                vec![
                    line_atom,
                    symbol_to_atom(*name),
                    expr_to_sexpr(value),
                    slot_expr,
                ],
            )
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } => list(
            "IndexAssignment",
            vec![
                line_atom,
                expr_to_sexpr(target),
                expr_to_sexpr(index),
                expr_to_sexpr(value),
            ],
        ),
        ExprKind::Call {
            function,
            args,
            block,
            inlined_body,
        } =>
        {
            let args_list = SExpr::List(args.iter().map(expr_to_sexpr).collect());
            let block_expr = block
                .as_ref()
                .map(closure_to_sexpr)
                .unwrap_or_else(|| atom("nil"));
            let inlined_expr = inlined_body
                .borrow()
                .as_ref()
                .map(expr_to_sexpr)
                .unwrap_or_else(|| atom("nil"));
            list(
                "Call",
                vec![
                    line_atom,
                    expr_to_sexpr(function),
                    args_list,
                    block_expr,
                    inlined_expr,
                ],
            )
        }
        ExprKind::Yield(args) =>
        {
            let args_list = SExpr::List(args.iter().map(expr_to_sexpr).collect());
            list("Yield", vec![line_atom, args_list])
        }
        ExprKind::Return(expr) =>
        {
            let expr_val = expr
                .as_ref()
                .map(|e| expr_to_sexpr(e.as_ref()))
                .unwrap_or_else(|| atom("nil"));
            list("Return", vec![line_atom, expr_val])
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            let else_expr = else_branch
                .as_ref()
                .map(|e| expr_to_sexpr(e.as_ref()))
                .unwrap_or_else(|| atom("nil"));
            list(
                "If",
                vec![
                    line_atom,
                    expr_to_sexpr(condition),
                    expr_to_sexpr(then_branch),
                    else_expr,
                ],
            )
        }
        ExprKind::While { condition, body } =>
        {
            list("While", vec![line_atom, expr_to_sexpr(condition), expr_to_sexpr(body)])
        }
        ExprKind::For {
            var,
            var_slot,
            iterable,
            body,
        } =>
        {
            let slot_expr = var_slot
                .map(|s| atom(s.to_string()))
                .unwrap_or_else(|| atom("nil"));
            list(
                "For",
                vec![
                    line_atom,
                    symbol_to_atom(*var),
                    slot_expr,
                    expr_to_sexpr(iterable),
                    expr_to_sexpr(body),
                ],
            )
        }
        ExprKind::Loop {
            count,
            var,
            var_slot,
            body,
        } =>
        {
            let var_expr = var.map(symbol_to_atom).unwrap_or_else(|| atom("nil"));
            let slot_expr = var_slot
                .map(|s| atom(s.to_string()))
                .unwrap_or_else(|| atom("nil"));
            list(
                "Loop",
                vec![
                    line_atom,
                    expr_to_sexpr(count),
                    var_expr,
                    slot_expr,
                    expr_to_sexpr(body),
                ],
            )
        }
        ExprKind::Collect {
            count,
            into,
            var,
            var_slot,
            body,
        } =>
        {
            let into_expr = into
                .as_ref()
                .map(|expr| expr_to_sexpr(expr))
                .unwrap_or_else(|| atom("nil"));
            let var_expr = var.map(symbol_to_atom).unwrap_or_else(|| atom("nil"));
            let slot_expr = var_slot
                .map(|s| atom(s.to_string()))
                .unwrap_or_else(|| atom("nil"));
            list(
                "Collect",
                vec![
                    line_atom,
                    expr_to_sexpr(count),
                    into_expr,
                    var_expr,
                    slot_expr,
                    expr_to_sexpr(body),
                ],
            )
        }
        ExprKind::FunctionDef {
            name,
            params,
            body,
            slots,
        } =>
        {
            let params_list = SExpr::List(params.iter().map(param_to_sexpr).collect());
            let slots_expr = slots
                .as_ref()
                .map(|names| {
                    let items = names
                        .iter()
                        .map(|name| SExpr::String(name.as_str().to_string()))
                        .collect();
                    SExpr::List(items)
                })
                .unwrap_or_else(|| atom("nil"));
            list(
                "FunctionDef",
                vec![
                    line_atom,
                    symbol_to_atom(*name),
                    params_list,
                    expr_to_sexpr(body),
                    slots_expr,
                ],
            )
        }
        ExprKind::MethodDef {
            type_name,
            name,
            params,
            body,
            slots,
        } =>
        {
            let params_list = SExpr::List(params.iter().map(param_to_sexpr).collect());
            let slots_expr = slots
                .as_ref()
                .map(|names| {
                    let items = names
                        .iter()
                        .map(|name| SExpr::String(name.as_str().to_string()))
                        .collect();
                    SExpr::List(items)
                })
                .unwrap_or_else(|| atom("nil"));
            list(
                "MethodDef",
                vec![
                    line_atom,
                    symbol_to_atom(*type_name),
                    symbol_to_atom(*name),
                    params_list,
                    expr_to_sexpr(body),
                    slots_expr,
                ],
            )
        }
        ExprKind::AnonymousFunction {
            params,
            body,
            slots,
        } =>
        {
            let params_list = SExpr::List(params.iter().map(param_to_sexpr).collect());
            let slots_expr = slots
                .as_ref()
                .map(|names| {
                    let items = names
                        .iter()
                        .map(|name| SExpr::String(name.as_str().to_string()))
                        .collect();
                    SExpr::List(items)
                })
                .unwrap_or_else(|| atom("nil"));
            list("AnonymousFunction", vec![line_atom, params_list, expr_to_sexpr(body), slots_expr])
        }
        ExprKind::Array(elements) =>
        {
            let elems = SExpr::List(elements.iter().map(expr_to_sexpr).collect());
            list("Array", vec![line_atom, elems])
        }
        ExprKind::StructDef { name, fields } =>
        {
            let mut items = Vec::with_capacity(fields.len() + 2);
            items.push(line_atom);
            items.push(symbol_to_atom(*name));
            let mut field_items = Vec::with_capacity(fields.len());
            for (field_name, type_ref) in fields
            {
                field_items.push(list(
                    "Field",
                    vec![symbol_to_atom(*field_name), type_ref_to_sexpr(type_ref)],
                ));
            }
            items.push(SExpr::List(field_items));
            list("StructDef", items)
        }
        ExprKind::StructLiteral { name, fields } =>
        {
            let mut items = Vec::with_capacity(fields.len() + 2);
            items.push(line_atom);
            items.push(symbol_to_atom(*name));
            let mut field_items = Vec::with_capacity(fields.len());
            for (field_name, value) in fields
            {
                field_items.push(list(
                    "FieldValue",
                    vec![symbol_to_atom(*field_name), expr_to_sexpr(value)],
                ));
            }
            items.push(SExpr::List(field_items));
            list("StructLiteral", items)
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            list("ArrayGenerator", vec![line_atom, expr_to_sexpr(generator), expr_to_sexpr(size)])
        }
        ExprKind::Map(entries) =>
        {
            let mut pairs = Vec::with_capacity(entries.len());
            for (k, v) in entries
            {
                pairs.push(list("Pair", vec![expr_to_sexpr(k), expr_to_sexpr(v)]));
            }
            list("Map", vec![line_atom, SExpr::List(pairs)])
        }
        ExprKind::Index { target, index } =>
        {
            list("Index", vec![line_atom, expr_to_sexpr(target), expr_to_sexpr(index)])
        }
        ExprKind::Slice { target, start, end } => list(
            "Slice",
            vec![
                line_atom,
                expr_to_sexpr(target),
                expr_to_sexpr(start),
                expr_to_sexpr(end),
            ],
        ),
        ExprKind::Use(path) => list("Use", vec![line_atom, path_to_sexpr(path)]),
        ExprKind::Import { path, alias } =>
        {
            let alias_expr = alias.map(symbol_to_atom).unwrap_or_else(|| atom("nil"));
            list(
                "Import",
                vec![
                    line_atom,
                    SExpr::String(path.as_str().to_string()),
                    alias_expr,
                ],
            )
        }
        ExprKind::Export { namespace, names } =>
        {
            let namespace_expr = path_to_sexpr(namespace);
            let mut name_items = Vec::with_capacity(names.len() + 1);
            name_items.push(atom("Names"));
            for name in names
            {
                name_items.push(symbol_to_atom(*name));
            }
            list("Export", vec![line_atom, namespace_expr, SExpr::List(name_items)])
        }
        ExprKind::FilePublic(expr) => list("FilePublic", vec![line_atom, expr_to_sexpr(expr)]),
        ExprKind::FunctionPublic(expr) =>
        {
            list("FunctionPublic", vec![line_atom, expr_to_sexpr(expr)])
        }
        ExprKind::Block(exprs) =>
        {
            let expr_list = SExpr::List(exprs.iter().map(expr_to_sexpr).collect());
            list("Block", vec![line_atom, expr_list])
        }
        ExprKind::FormatString(parts) =>
        {
            let part_list = SExpr::List(parts.iter().map(format_part_to_sexpr).collect());
            list("FormatString", vec![line_atom, part_list])
        }
        ExprKind::Load(path) => list("Load", vec![line_atom, path_to_sexpr(path)]),
    }
}

fn parse_expr(expr: &SExpr) -> Result<Expr, String>
{
    let items = expect_list(expr)?;
    if items.len() < 2
    {
        return Err("Invalid expression".to_string());
    }
    let tag = expect_atom(&items[0])?;
    let line = parse_usize(&items[1])?;
    let kind = match tag
    {
        "Integer" =>
        {
            if items.len() != 4
            {
                return Err("Invalid Integer".to_string());
            }
            ExprKind::Integer {
                kind: parse_int_kind(&items[2])?,
                value: expect_atom(&items[3])?
                    .parse::<i128>()
                    .map_err(|_| "Invalid integer".to_string())?,
            }
        }
        "Unsigned" =>
        {
            if items.len() != 4
            {
                return Err("Invalid Unsigned".to_string());
            }
            ExprKind::Unsigned {
                kind: parse_int_kind(&items[2])?,
                value: expect_atom(&items[3])?
                    .parse::<u128>()
                    .map_err(|_| "Invalid unsigned".to_string())?,
            }
        }
        "Float" =>
        {
            if items.len() != 4
            {
                return Err("Invalid Float".to_string());
            }
            ExprKind::Float {
                kind: parse_float_kind(&items[2])?,
                value: expect_atom(&items[3])?
                    .parse::<f64>()
                    .map_err(|_| "Invalid float".to_string())?,
            }
        }
        "Identifier" =>
        {
            if items.len() != 4
            {
                return Err("Invalid Identifier".to_string());
            }
            let slot = match &items[3]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other => Some(parse_usize(other)?),
            };
            ExprKind::Identifier {
                name: parse_symbol(&items[2])?,
                slot,
            }
        }
        "Reference" =>
        {
            if items.len() != 3
            {
                return Err("Invalid Reference".to_string());
            }
            ExprKind::Reference(parse_symbol(&items[2])?)
        }
        "String" =>
        {
            if items.len() != 3
            {
                return Err("Invalid String".to_string());
            }
            ExprKind::String(Rc::new(expect_string(&items[2])?.to_string()))
        }
        "Boolean" =>
        {
            if items.len() != 3
            {
                return Err("Invalid Boolean".to_string());
            }
            ExprKind::Boolean(parse_bool(&items[2])?)
        }
        "Nil" =>
        {
            if items.len() != 2
            {
                return Err("Invalid Nil".to_string());
            }
            ExprKind::Nil
        }
        "Shell" =>
        {
            if items.len() != 3
            {
                return Err("Invalid Shell".to_string());
            }
            ExprKind::Shell(Rc::new(expect_string(&items[2])?.to_string()))
        }
        "Clone" =>
        {
            if items.len() != 3
            {
                return Err("Invalid Clone".to_string());
            }
            ExprKind::Clone(Box::new(parse_expr(&items[2])?))
        }
        "EnvFreeze" =>
        {
            if items.len() != 3
            {
                return Err("Invalid EnvFreeze".to_string());
            }
            ExprKind::EnvFreeze(Box::new(parse_expr(&items[2])?))
        }
        "Not" =>
        {
            if items.len() != 3
            {
                return Err("Invalid Not".to_string());
            }
            ExprKind::Not(Box::new(parse_expr(&items[2])?))
        }
        "And" =>
        {
            if items.len() != 4
            {
                return Err("Invalid And".to_string());
            }
            ExprKind::And {
                left: Box::new(parse_expr(&items[2])?),
                right: Box::new(parse_expr(&items[3])?),
            }
        }
        "AndBool" =>
        {
            if items.len() != 4
            {
                return Err("Invalid AndBool".to_string());
            }
            ExprKind::AndBool {
                left: Box::new(parse_expr(&items[2])?),
                right: Box::new(parse_expr(&items[3])?),
            }
        }
        "Or" =>
        {
            if items.len() != 4
            {
                return Err("Invalid Or".to_string());
            }
            ExprKind::Or {
                left: Box::new(parse_expr(&items[2])?),
                right: Box::new(parse_expr(&items[3])?),
            }
        }
        "OrBool" =>
        {
            if items.len() != 4
            {
                return Err("Invalid OrBool".to_string());
            }
            ExprKind::OrBool {
                left: Box::new(parse_expr(&items[2])?),
                right: Box::new(parse_expr(&items[3])?),
            }
        }
        "BinaryOp" =>
        {
            if items.len() != 5
            {
                return Err("Invalid BinaryOp".to_string());
            }
            ExprKind::BinaryOp {
                op: parse_op(&items[2])?,
                left: Box::new(parse_expr(&items[3])?),
                right: Box::new(parse_expr(&items[4])?),
            }
        }
        "Assignment" =>
        {
            if items.len() != 5
            {
                return Err("Invalid Assignment".to_string());
            }
            let slot = match &items[4]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other => Some(parse_usize(other)?),
            };
            ExprKind::Assignment {
                name: parse_symbol(&items[2])?,
                value: Box::new(parse_expr(&items[3])?),
                slot,
            }
        }
        "IndexAssignment" =>
        {
            if items.len() != 5
            {
                return Err("Invalid IndexAssignment".to_string());
            }
            ExprKind::IndexAssignment {
                target: Box::new(parse_expr(&items[2])?),
                index: Box::new(parse_expr(&items[3])?),
                value: Box::new(parse_expr(&items[4])?),
            }
        }
        "Call" =>
        {
            if items.len() != 6
            {
                return Err("Invalid Call".to_string());
            }
            let args_items = expect_list(&items[3])?;
            let mut args = Vec::with_capacity(args_items.len());
            for arg in args_items
            {
                args.push(parse_expr(arg)?);
            }
            let block = match &items[4]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other => Some(parse_closure(other)?),
            };
            let inlined = match &items[5]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other => Some(parse_expr(other)?),
            };
            ExprKind::Call {
                function: Box::new(parse_expr(&items[2])?),
                args,
                block,
                inlined_body: Rc::new(RefCell::new(inlined)),
            }
        }
        "Yield" =>
        {
            if items.len() != 3
            {
                return Err("Invalid Yield".to_string());
            }
            let args_items = expect_list(&items[2])?;
            let mut args = Vec::with_capacity(args_items.len());
            for arg in args_items
            {
                args.push(parse_expr(arg)?);
            }
            ExprKind::Yield(args)
        }
        "Return" =>
        {
            if items.len() != 3
            {
                return Err("Invalid Return".to_string());
            }
            let expr = match &items[2]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other => Some(Box::new(parse_expr(other)?)),
            };
            ExprKind::Return(expr)
        }
        "If" =>
        {
            if items.len() != 5
            {
                return Err("Invalid If".to_string());
            }
            let else_branch = match &items[4]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other => Some(Box::new(parse_expr(other)?)),
            };
            ExprKind::If {
                condition: Box::new(parse_expr(&items[2])?),
                then_branch: Box::new(parse_expr(&items[3])?),
                else_branch,
            }
        }
        "While" =>
        {
            if items.len() != 4
            {
                return Err("Invalid While".to_string());
            }
            ExprKind::While {
                condition: Box::new(parse_expr(&items[2])?),
                body: Box::new(parse_expr(&items[3])?),
            }
        }
        "For" =>
        {
            if items.len() != 6
            {
                return Err("Invalid For".to_string());
            }
            let var_slot = match &items[3]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other => Some(parse_usize(other)?),
            };
            ExprKind::For {
                var: parse_symbol(&items[2])?,
                var_slot,
                iterable: Box::new(parse_expr(&items[4])?),
                body: Box::new(parse_expr(&items[5])?),
            }
        }
        "Loop" =>
        {
            if items.len() != 6
            {
                return Err("Invalid Loop".to_string());
            }
            let var = match &items[3]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other => Some(parse_symbol(other)?),
            };
            let var_slot = match &items[4]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other => Some(parse_usize(other)?),
            };
            ExprKind::Loop {
                count: Box::new(parse_expr(&items[2])?),
                var,
                var_slot,
                body: Box::new(parse_expr(&items[5])?),
            }
        }
        "Collect" =>
        {
            if items.len() != 6 && items.len() != 7
            {
                return Err("Invalid Collect".to_string());
            }
            if items.len() == 6
            {
                let var = match &items[3]
                {
                    SExpr::Atom(s) if s == "nil" => None,
                    other => Some(parse_symbol(other)?),
                };
                let var_slot = match &items[4]
                {
                    SExpr::Atom(s) if s == "nil" => None,
                    other => Some(parse_usize(other)?),
                };
                ExprKind::Collect {
                    count: Box::new(parse_expr(&items[2])?),
                    into: None,
                    var,
                    var_slot,
                    body: Box::new(parse_expr(&items[5])?),
                }
            }
            else
            {
                let into = match &items[3]
                {
                    SExpr::Atom(s) if s == "nil" => None,
                    other => Some(Box::new(parse_expr(other)?)),
                };
                let var = match &items[4]
                {
                    SExpr::Atom(s) if s == "nil" => None,
                    other => Some(parse_symbol(other)?),
                };
                let var_slot = match &items[5]
                {
                    SExpr::Atom(s) if s == "nil" => None,
                    other => Some(parse_usize(other)?),
                };
                ExprKind::Collect {
                    count: Box::new(parse_expr(&items[2])?),
                    into,
                    var,
                    var_slot,
                    body: Box::new(parse_expr(&items[6])?),
                }
            }
        }
        "FunctionDef" =>
        {
            if items.len() != 6
            {
                return Err("Invalid FunctionDef".to_string());
            }
            let params_items = expect_list(&items[3])?;
            let mut params = Vec::with_capacity(params_items.len());
            for param in params_items
            {
                params.push(parse_param(param)?);
            }
            let slots = match &items[5]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other =>
                {
                    let slot_items = expect_list(other)?;
                    let mut names = Vec::with_capacity(slot_items.len());
                    for item in slot_items
                    {
                        let name = expect_string(item)?;
                        names.push(Rc::new(name.to_string()));
                    }
                    Some(Rc::new(names))
                }
            };
            ExprKind::FunctionDef {
                name: parse_symbol(&items[2])?,
                params,
                body: Box::new(parse_expr(&items[4])?),
                slots,
            }
        }
        "MethodDef" =>
        {
            if items.len() != 7
            {
                return Err("Invalid MethodDef".to_string());
            }
            let params_items = expect_list(&items[4])?;
            let mut params = Vec::with_capacity(params_items.len());
            for param in params_items
            {
                params.push(parse_param(param)?);
            }
            let slots = match &items[6]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other =>
                {
                    let slot_items = expect_list(other)?;
                    let mut names = Vec::with_capacity(slot_items.len());
                    for item in slot_items
                    {
                        let name = expect_string(item)?;
                        names.push(Rc::new(name.to_string()));
                    }
                    Some(Rc::new(names))
                }
            };
            ExprKind::MethodDef {
                type_name: parse_symbol(&items[2])?,
                name: parse_symbol(&items[3])?,
                params,
                body: Box::new(parse_expr(&items[5])?),
                slots,
            }
        }
        "AnonymousFunction" =>
        {
            if items.len() != 5
            {
                return Err("Invalid AnonymousFunction".to_string());
            }
            let params_items = expect_list(&items[2])?;
            let mut params = Vec::with_capacity(params_items.len());
            for param in params_items
            {
                params.push(parse_param(param)?);
            }
            let slots = match &items[4]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other =>
                {
                    let slot_items = expect_list(other)?;
                    let mut names = Vec::with_capacity(slot_items.len());
                    for item in slot_items
                    {
                        let name = expect_string(item)?;
                        names.push(Rc::new(name.to_string()));
                    }
                    Some(Rc::new(names))
                }
            };
            ExprKind::AnonymousFunction {
                params,
                body: Box::new(parse_expr(&items[3])?),
                slots,
            }
        }
        "Array" =>
        {
            if items.len() != 3
            {
                return Err("Invalid Array".to_string());
            }
            let elements = expect_list(&items[2])?;
            let mut values = Vec::with_capacity(elements.len());
            for element in elements
            {
                values.push(parse_expr(element)?);
            }
            ExprKind::Array(values)
        }
        "StructDef" =>
        {
            if items.len() != 4
            {
                return Err("Invalid StructDef".to_string());
            }
            let field_items = expect_list(&items[3])?;
            let mut fields = Vec::with_capacity(field_items.len());
            for item in field_items
            {
                let field = expect_list(item)?;
                if field.len() != 3 || expect_atom(&field[0])? != "Field"
                {
                    return Err("Invalid Field".to_string());
                }
                fields.push((parse_symbol(&field[1])?, parse_type_ref(&field[2])?));
            }
            ExprKind::StructDef {
                name: parse_symbol(&items[2])?,
                fields,
            }
        }
        "StructLiteral" =>
        {
            if items.len() != 4
            {
                return Err("Invalid StructLiteral".to_string());
            }
            let field_items = expect_list(&items[3])?;
            let mut fields = Vec::with_capacity(field_items.len());
            for item in field_items
            {
                let field = expect_list(item)?;
                if field.len() != 3 || expect_atom(&field[0])? != "FieldValue"
                {
                    return Err("Invalid FieldValue".to_string());
                }
                fields.push((parse_symbol(&field[1])?, parse_expr(&field[2])?));
            }
            ExprKind::StructLiteral {
                name: parse_symbol(&items[2])?,
                fields,
            }
        }
        "ArrayGenerator" =>
        {
            if items.len() != 4
            {
                return Err("Invalid ArrayGenerator".to_string());
            }
            ExprKind::ArrayGenerator {
                generator: Box::new(parse_expr(&items[2])?),
                size: Box::new(parse_expr(&items[3])?),
            }
        }
        "Map" =>
        {
            if items.len() != 3
            {
                return Err("Invalid Map".to_string());
            }
            let pair_items = expect_list(&items[2])?;
            let mut entries = Vec::with_capacity(pair_items.len());
            for item in pair_items
            {
                let pair = expect_list(item)?;
                if pair.len() != 3 || expect_atom(&pair[0])? != "Pair"
                {
                    return Err("Invalid Pair".to_string());
                }
                entries.push((parse_expr(&pair[1])?, parse_expr(&pair[2])?));
            }
            ExprKind::Map(entries)
        }
        "Index" =>
        {
            if items.len() != 4
            {
                return Err("Invalid Index".to_string());
            }
            ExprKind::Index {
                target: Box::new(parse_expr(&items[2])?),
                index: Box::new(parse_expr(&items[3])?),
            }
        }
        "Slice" =>
        {
            if items.len() != 5
            {
                return Err("Invalid Slice".to_string());
            }
            ExprKind::Slice {
                target: Box::new(parse_expr(&items[2])?),
                start: Box::new(parse_expr(&items[3])?),
                end: Box::new(parse_expr(&items[4])?),
            }
        }
        "Use" =>
        {
            if items.len() != 3
            {
                return Err("Invalid Use".to_string());
            }
            ExprKind::Use(parse_path(&items[2])?)
        }
        "Import" =>
        {
            if items.len() != 4
            {
                return Err("Invalid Import".to_string());
            }
            let alias = match &items[3]
            {
                SExpr::Atom(s) if s == "nil" => None,
                other => Some(parse_symbol(other)?),
            };
            ExprKind::Import {
                path: Rc::new(expect_string(&items[2])?.to_string()),
                alias,
            }
        }
        "Export" =>
        {
            if items.len() != 4
            {
                return Err("Invalid Export".to_string());
            }
            let namespace = parse_path(&items[2])?;
            let names_items = expect_list(&items[3])?;
            if names_items.is_empty() || expect_atom(&names_items[0])? != "Names"
            {
                return Err("Invalid Export names".to_string());
            }
            let mut names = Vec::with_capacity(names_items.len().saturating_sub(1));
            for item in &names_items[1..]
            {
                names.push(parse_symbol(item)?);
            }
            ExprKind::Export { namespace, names }
        }
        "FilePublic" =>
        {
            if items.len() != 3
            {
                return Err("Invalid FilePublic".to_string());
            }
            ExprKind::FilePublic(Box::new(parse_expr(&items[2])?))
        }
        "FunctionPublic" =>
        {
            if items.len() != 3
            {
                return Err("Invalid FunctionPublic".to_string());
            }
            ExprKind::FunctionPublic(Box::new(parse_expr(&items[2])?))
        }
        "Block" =>
        {
            if items.len() != 3
            {
                return Err("Invalid Block".to_string());
            }
            let expr_items = expect_list(&items[2])?;
            let mut exprs = Vec::with_capacity(expr_items.len());
            for item in expr_items
            {
                exprs.push(parse_expr(item)?);
            }
            ExprKind::Block(exprs)
        }
        "FormatString" =>
        {
            if items.len() != 3
            {
                return Err("Invalid FormatString".to_string());
            }
            let part_items = expect_list(&items[2])?;
            let mut parts = Vec::with_capacity(part_items.len());
            for item in part_items
            {
                parts.push(parse_format_part(item)?);
            }
            ExprKind::FormatString(parts)
        }
        "Load" =>
        {
            if items.len() != 3
            {
                return Err("Invalid Load".to_string());
            }
            ExprKind::Load(parse_path(&items[2])?)
        }
        _ => return Err("Unknown expression tag".to_string()),
    };
    Ok(Expr { kind, line })
}

pub fn sexpr_to_expr(expr: &SExpr) -> Result<Expr, String>
{
    parse_expr(expr)
}

pub fn value_to_sexpr(value: &Value) -> Result<SExpr, String>
{
    match value
    {
        Value::Nil => Ok(atom("nil")),
        Value::Boolean(b) => Ok(atom(b.to_string())),
        Value::Integer { value, kind } =>
        {
            Ok(list("int", vec![atom(format!("{:?}", kind)), atom(value.to_string())]))
        }
        Value::Unsigned { value, kind } =>
        {
            Ok(list("uint", vec![atom(format!("{:?}", kind)), atom(value.to_string())]))
        }
        Value::Float { value, kind } =>
        {
            Ok(list("float", vec![atom(format!("{:?}", kind)), atom(value.to_string())]))
        }
        Value::String(s) => Ok(SExpr::String(s.as_str().to_string())),
        Value::Array(arr) =>
        {
            let mut items = Vec::with_capacity(arr.borrow().len() + 1);
            items.push(atom("array"));
            for item in arr.borrow().iter()
            {
                items.push(value_to_sexpr(item)?);
            }
            Ok(SExpr::List(items))
        }
        Value::F32Array(arr) =>
        {
            let mut items = Vec::with_capacity(arr.borrow().len() + 1);
            items.push(atom("f32array"));
            for item in arr.borrow().iter()
            {
                items.push(atom(item.to_string()));
            }
            Ok(SExpr::List(items))
        }
        Value::F64Array(arr) =>
        {
            let mut items = Vec::with_capacity(arr.borrow().len() + 1);
            items.push(atom("f64array"));
            for item in arr.borrow().iter()
            {
                items.push(atom(item.to_string()));
            }
            Ok(SExpr::List(items))
        }
        Value::I32Array(arr) =>
        {
            let mut items = Vec::with_capacity(arr.borrow().len() + 1);
            items.push(atom("i32array"));
            for item in arr.borrow().iter()
            {
                items.push(atom(item.to_string()));
            }
            Ok(SExpr::List(items))
        }
        Value::I64Array(arr) =>
        {
            let mut items = Vec::with_capacity(arr.borrow().len() + 1);
            items.push(atom("i64array"));
            for item in arr.borrow().iter()
            {
                items.push(atom(item.to_string()));
            }
            Ok(SExpr::List(items))
        }
        Value::Bytes(bytes) =>
        {
            let mut items = Vec::with_capacity(bytes.len() + 1);
            items.push(atom("bytes"));
            for b in bytes.iter()
            {
                items.push(atom(b.to_string()));
            }
            Ok(SExpr::List(items))
        }
        Value::ByteBuf(buf) =>
        {
            let data = buf.borrow();
            let mut items = Vec::with_capacity(data.len() + 1);
            items.push(atom("bytebuf"));
            for b in data.iter()
            {
                items.push(atom(b.to_string()));
            }
            Ok(SExpr::List(items))
        }
        Value::BytesView(view) =>
        {
            let data = match &view.source
            {
                BytesViewSource::Mmap(mmap) =>
                {
                    let end = view.offset.saturating_add(view.len);
                    mmap[view.offset..end].to_vec()
                }
                BytesViewSource::MmapMut(mmap) =>
                {
                    let data = mmap.borrow();
                    let end = view.offset.saturating_add(view.len);
                    data[view.offset..end].to_vec()
                }
            };
            let mut items = Vec::with_capacity(data.len() + 1);
            items.push(atom("bytesview"));
            for b in data.iter()
            {
                items.push(atom(b.to_string()));
            }
            Ok(SExpr::List(items))
        }
        Value::Map(map) =>
        {
            let map_ref = map.borrow();
            let mut entries = Vec::with_capacity(map_ref.data.len() + 1);
            entries.push(atom("map"));
            for (key, value) in map_ref.data.iter()
            {
                entries.push(list(
                    "entry",
                    vec![
                        SExpr::String(key.as_str().to_string()),
                        value_to_sexpr(value)?,
                    ],
                ));
            }
            Ok(SExpr::List(entries))
        }
        Value::Env(env) =>
        {
            let mut entries = Vec::with_capacity(env.data.len() + 1);
            entries.push(atom("env"));
            for (key, value) in env.data.iter()
            {
                entries.push(list(
                    "entry",
                    vec![
                        SExpr::String(key.as_str().to_string()),
                        value_to_sexpr(value)?,
                    ],
                ));
            }
            Ok(SExpr::List(entries))
        }
        Value::Ast(ast) => Ok(list("ast", vec![expr_to_sexpr(ast.as_ref())])),
        Value::Function(data) =>
        {
            let params_list = SExpr::List(data.params.iter().map(param_to_sexpr).collect());
            Ok(list("function", vec![params_list, expr_to_sexpr(&data.body)]))
        }
        Value::Reference(r) => value_to_sexpr(&r.borrow()),
        _ => Err("value.to_sexpr does not support this type".to_string()),
    }
}

pub fn sexpr_to_value(expr: &SExpr) -> Result<Value, String>
{
    match expr
    {
        SExpr::Atom(s) if s == "nil" => Ok(Value::Nil),
        SExpr::Atom(s) if s == "true" => Ok(Value::Boolean(true)),
        SExpr::Atom(s) if s == "false" => Ok(Value::Boolean(false)),
        SExpr::Atom(s) =>
        {
            if let Ok(value) = s.parse::<i128>()
            {
                return Ok(Value::Integer {
                    value,
                    kind: IntKind::I64,
                });
            }
            if let Ok(value) = s.parse::<f64>()
            {
                return Ok(Value::Float {
                    value,
                    kind: FloatKind::F64,
                });
            }
            Ok(Value::String(Rc::new(s.to_string())))
        }
        SExpr::String(s) => Ok(Value::String(Rc::new(s.to_string()))),
        SExpr::List(items) =>
        {
            if items.is_empty()
            {
                return Err("Empty list".to_string());
            }
            let tag = expect_atom(&items[0])?;
            match tag
            {
                "int" =>
                {
                    if items.len() != 3
                    {
                        return Err("Invalid int".to_string());
                    }
                    let kind = parse_int_kind(&items[1])?;
                    let value = expect_atom(&items[2])?
                        .parse::<i128>()
                        .map_err(|_| "Invalid int value".to_string())?;
                    Ok(Value::Integer { value, kind })
                }
                "uint" =>
                {
                    if items.len() != 3
                    {
                        return Err("Invalid uint".to_string());
                    }
                    let kind = parse_int_kind(&items[1])?;
                    let value = expect_atom(&items[2])?
                        .parse::<u128>()
                        .map_err(|_| "Invalid uint value".to_string())?;
                    Ok(Value::Unsigned { value, kind })
                }
                "float" =>
                {
                    if items.len() != 3
                    {
                        return Err("Invalid float".to_string());
                    }
                    let kind = parse_float_kind(&items[1])?;
                    let value = expect_atom(&items[2])?
                        .parse::<f64>()
                        .map_err(|_| "Invalid float value".to_string())?;
                    Ok(Value::Float { value, kind })
                }
                "array" =>
                {
                    let mut elems = Vec::with_capacity(items.len().saturating_sub(1));
                    for item in &items[1..]
                    {
                        elems.push(sexpr_to_value(item)?);
                    }
                    Ok(Value::Array(Rc::new(RefCell::new(elems))))
                }
                "f32array" =>
                {
                    let mut elems = Vec::with_capacity(items.len().saturating_sub(1));
                    for item in &items[1..]
                    {
                        let value = expect_atom(item)?
                            .parse::<f32>()
                            .map_err(|_| "Invalid f32 value".to_string())?;
                        elems.push(value);
                    }
                    Ok(Value::F32Array(Rc::new(RefCell::new(elems))))
                }
                "f64array" =>
                {
                    let mut elems = Vec::with_capacity(items.len().saturating_sub(1));
                    for item in &items[1..]
                    {
                        let value = expect_atom(item)?
                            .parse::<f64>()
                            .map_err(|_| "Invalid f64 value".to_string())?;
                        elems.push(value);
                    }
                    Ok(Value::F64Array(Rc::new(RefCell::new(elems))))
                }
                "i32array" =>
                {
                    let mut elems = Vec::with_capacity(items.len().saturating_sub(1));
                    for item in &items[1..]
                    {
                        let value = expect_atom(item)?
                            .parse::<i32>()
                            .map_err(|_| "Invalid i32 value".to_string())?;
                        elems.push(value);
                    }
                    Ok(Value::I32Array(Rc::new(RefCell::new(elems))))
                }
                "i64array" =>
                {
                    let mut elems = Vec::with_capacity(items.len().saturating_sub(1));
                    for item in &items[1..]
                    {
                        let value = expect_atom(item)?
                            .parse::<i64>()
                            .map_err(|_| "Invalid i64 value".to_string())?;
                        elems.push(value);
                    }
                    Ok(Value::I64Array(Rc::new(RefCell::new(elems))))
                }
                "bytes" | "bytebuf" | "bytesview" =>
                {
                    let mut elems = Vec::with_capacity(items.len().saturating_sub(1));
                    for item in &items[1..]
                    {
                        let value = expect_atom(item)?
                            .parse::<u8>()
                            .map_err(|_| "Invalid byte value".to_string())?;
                        elems.push(value);
                    }
                    if tag == "bytes"
                    {
                        Ok(Value::Bytes(Rc::new(elems)))
                    }
                    else
                    {
                        Ok(Value::ByteBuf(Rc::new(RefCell::new(elems))))
                    }
                }
                "map" =>
                {
                    let mut map = FxHashMap::default();
                    for item in &items[1..]
                    {
                        let entry = expect_list(item)?;
                        if entry.len() != 3 || expect_atom(&entry[0])? != "entry"
                        {
                            return Err("Invalid map entry".to_string());
                        }
                        let key = expect_string(&entry[1])?;
                        let value = sexpr_to_value(&entry[2])?;
                        map.insert(intern::intern_owned(key.to_string()), value);
                    }
                    Ok(Value::Map(Rc::new(RefCell::new(MapValue::new(map)))))
                }
                "env" =>
                {
                    let mut map = FxHashMap::default();
                    for item in &items[1..]
                    {
                        let entry = expect_list(item)?;
                        if entry.len() != 3 || expect_atom(&entry[0])? != "entry"
                        {
                            return Err("Invalid env entry".to_string());
                        }
                        let key = expect_string(&entry[1])?;
                        let value = sexpr_to_value(&entry[2])?;
                        map.insert(intern::intern_owned(key.to_string()), value);
                    }
                    Ok(Value::Env(Rc::new(EnvValue::new(map))))
                }
                "ast" =>
                {
                    if items.len() != 2
                    {
                        return Err("Invalid ast value".to_string());
                    }
                    let expr = sexpr_to_expr(&items[1])?;
                    Ok(Value::Ast(Rc::new(expr)))
                }
                "function" =>
                {
                    if items.len() != 3
                    {
                        return Err("Invalid function".to_string());
                    }
                    let params_list = expect_list(&items[1])?;
                    let mut params = Vec::with_capacity(params_list.len());
                    let mut decls = Vec::with_capacity(params_list.len());
                    for item in params_list
                    {
                        params.push(parse_param(item)?);
                        decls.push(Rc::new(String::new())); // Dummy declaration for slot allocation
                    }
                    let body = parse_expr(&items[2])?;
                    let func_data = FunctionData {
                        params,
                        body,
                        declarations: Rc::new(decls),
                        param_offset: 0,
                        is_simple: false,
                        uses_env: true,
                        code: None,
                        reg_code: None,
                        fast_reg_code: None,
                        const_pool: Rc::new(Vec::new()),
                        bound_args: Rc::new(Vec::new()),
                        env: Rc::new(RefCell::new(Environment::new(None))),
                    };
                    Ok(Value::Function(Rc::new(func_data)))
                }
                _ => Err("Unknown value tag".to_string()),
            }
        }
    }
}
