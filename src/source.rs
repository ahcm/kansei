use crate::ast::{
    Closure, Expr, ExprKind, FloatKind, FormatPart, IntKind, Op, Param, ParamType, TypeRef,
};
use crate::intern;

const INDENT: &str = "  ";

pub fn expr_to_source(expr: &Expr) -> String
{
    let mut out = format_stmt(expr, 0);
    if out.ends_with('\n')
    {
        out.pop();
    }
    out
}

fn format_stmt(expr: &Expr, indent: usize) -> String
{
    match &expr.kind
    {
        ExprKind::Block(stmts) =>
        {
            let mut out = String::new();
            for (idx, stmt) in stmts.iter().enumerate()
            {
                if idx > 0
                {
                    out.push('\n');
                }
                out.push_str(&format_stmt(stmt, indent));
            }
            out
        }
        ExprKind::FunctionDef {
            name, params, body, ..
        } =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("fn ");
            out.push_str(intern::symbol_name(*name).as_str());
            out.push('(');
            out.push_str(&format_params(params));
            out.push_str(")\n");
            out.push_str(&format_stmt(body, indent + 1));
            out.push('\n');
            out.push_str(&indent_str(indent));
            out.push_str("end");
            out
        }
        ExprKind::MethodDef {
            type_name,
            name,
            params,
            body,
            ..
        } =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("fn ");
            out.push_str(intern::symbol_name(*type_name).as_str());
            out.push('.');
            out.push_str(intern::symbol_name(*name).as_str());
            out.push('(');
            out.push_str(&format_params(params));
            out.push_str(")\n");
            out.push_str(&format_stmt(body, indent + 1));
            out.push('\n');
            out.push_str(&indent_str(indent));
            out.push_str("end");
            out
        }
        ExprKind::AnonymousFunction { params, body, .. } =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("fn(");
            out.push_str(&format_params(params));
            out.push_str(")\n");
            out.push_str(&format_stmt(body, indent + 1));
            out.push('\n');
            out.push_str(&indent_str(indent));
            out.push_str("end");
            out
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("if ");
            out.push_str(&format_expr(condition, indent));
            out.push('\n');
            out.push_str(&format_stmt(then_branch, indent + 1));
            match else_branch.as_ref()
            {
                Some(else_expr) =>
                {
                    out.push('\n');
                    out.push_str(&indent_str(indent));
                    out.push_str("else\n");
                    out.push_str(&format_stmt(else_expr, indent + 1));
                    out.push('\n');
                    out.push_str(&indent_str(indent));
                    out.push_str("end");
                }
                None =>
                {
                    out.push('\n');
                    out.push_str(&indent_str(indent));
                    out.push_str("end");
                }
            }
            out
        }
        ExprKind::While { condition, body } =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("while ");
            out.push_str(&format_expr(condition, indent));
            out.push('\n');
            out.push_str(&format_stmt(body, indent + 1));
            out.push('\n');
            out.push_str(&indent_str(indent));
            out.push_str("end");
            out
        }
        ExprKind::For {
            var,
            iterable,
            body,
            ..
        } =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("for ");
            out.push_str(intern::symbol_name(*var).as_str());
            out.push_str(" in ");
            out.push_str(&format_expr(iterable, indent));
            out.push('\n');
            out.push_str(&format_stmt(body, indent + 1));
            out.push('\n');
            out.push_str(&indent_str(indent));
            out.push_str("end");
            out
        }
        ExprKind::Loop {
            count, var, body, ..
        } =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("loop ");
            out.push_str(&format_expr(count, indent));
            if let Some(var) = var
            {
                out.push(' ');
                out.push('|');
                out.push_str(intern::symbol_name(*var).as_str());
                out.push('|');
            }
            out.push('\n');
            out.push_str(&format_stmt(body, indent + 1));
            out.push('\n');
            out.push_str(&indent_str(indent));
            out.push_str("end");
            out
        }
        ExprKind::Collect {
            count,
            into,
            var,
            body,
            ..
        } =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("collect ");
            out.push_str(&format_expr(count, indent));
            if let Some(into) = into
            {
                out.push_str(" into ");
                out.push_str(&format_expr(into, indent));
            }
            if let Some(var) = var
            {
                out.push(' ');
                out.push('|');
                out.push_str(intern::symbol_name(*var).as_str());
                out.push('|');
            }
            out.push('\n');
            out.push_str(&format_stmt(body, indent + 1));
            out.push('\n');
            out.push_str(&indent_str(indent));
            out.push_str("end");
            out
        }
        ExprKind::StructDef { name, fields } =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("struct ");
            out.push_str(intern::symbol_name(*name).as_str());
            out.push_str(" {");
            if !fields.is_empty()
            {
                out.push(' ');
                out.push_str(
                    &fields
                        .iter()
                        .map(|(field, ty)| {
                            format!(
                                "{}: {}",
                                intern::symbol_name(*field).as_str(),
                                format_type_ref(ty)
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", "),
                );
                out.push(' ');
            }
            out.push('}');
            out
        }
        ExprKind::FilePublic(inner) =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("@file ");
            out.push_str(&format_expr(inner, indent));
            out
        }
        ExprKind::FunctionPublic(inner) =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("@function ");
            out.push_str(&format_expr(inner, indent));
            out
        }
        ExprKind::Use(path) =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("use ");
            out.push_str(&format_path(path));
            out
        }
        ExprKind::Load(path) =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("load ");
            out.push_str(&format_path(path));
            out
        }
        ExprKind::Import { path, alias } =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("import ");
            out.push_str(&format_string_literal(path.as_str()));
            if let Some(alias) = alias
            {
                out.push_str(" as ");
                out.push_str(intern::symbol_name(*alias).as_str());
            }
            out
        }
        ExprKind::Export { namespace, names } =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str("export ");
            out.push_str(&format_path(namespace));
            out.push_str("::[");
            out.push_str(
                &names
                    .iter()
                    .map(|name| intern::symbol_name(*name).as_str().to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            );
            out.push(']');
            out
        }
        _ =>
        {
            let mut out = String::new();
            out.push_str(&indent_str(indent));
            out.push_str(&format_expr(expr, indent));
            out
        }
    }
}

fn format_expr(expr: &Expr, indent: usize) -> String
{
    match &expr.kind
    {
        ExprKind::Integer { value, kind } => format_int(*value, *kind),
        ExprKind::Unsigned { value, kind } => format_uint(*value, *kind),
        ExprKind::Float { value, kind } => format_float(*value, *kind),
        ExprKind::Identifier { name, .. } => intern::symbol_name(*name).as_str().to_string(),
        ExprKind::Reference(name) => format!("&{}", intern::symbol_name(*name).as_str()),
        ExprKind::String(s) => format_string_literal(s.as_str()),
        ExprKind::Boolean(b) => b.to_string(),
        ExprKind::Nil => "nil".to_string(),
        ExprKind::Shell(cmd) => format_shell_literal(cmd.as_str()),
        ExprKind::Clone(inner) => format!("clone {}", format_expr(inner, indent)),
        ExprKind::EnvFreeze(inner) => format!("%{}", format_expr(inner, indent)),
        ExprKind::Not(inner) => format!("not {}", format_expr(inner, indent)),
        ExprKind::And { left, right } =>
        {
            format!("({} and {})", format_expr(left, indent), format_expr(right, indent))
        }
        ExprKind::AndBool { left, right } =>
        {
            format!("({} && {})", format_expr(left, indent), format_expr(right, indent))
        }
        ExprKind::Or { left, right } =>
        {
            format!("({} or {})", format_expr(left, indent), format_expr(right, indent))
        }
        ExprKind::OrBool { left, right } =>
        {
            format!("({} || {})", format_expr(left, indent), format_expr(right, indent))
        }
        ExprKind::BinaryOp { left, op, right } =>
        {
            format!(
                "({} {} {})",
                format_expr(left, indent),
                format_op(op),
                format_expr(right, indent)
            )
        }
        ExprKind::Assignment { name, value, .. } =>
        {
            format!("{} = {}", intern::symbol_name(*name).as_str(), format_expr(value, indent))
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            format!(
                "{}[{}] = {}",
                format_expr(target, indent),
                format_expr(index, indent),
                format_expr(value, indent)
            )
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            let mut out = String::new();
            out.push_str(&format_expr(function, indent));
            out.push('(');
            out.push_str(
                &args
                    .iter()
                    .map(|arg| format_expr(arg, indent))
                    .collect::<Vec<_>>()
                    .join(", "),
            );
            out.push(')');
            if let Some(block) = block
            {
                out.push(' ');
                out.push_str(&format_closure(block, indent));
            }
            out
        }
        ExprKind::Yield(args) =>
        {
            let mut out = String::new();
            out.push_str("yield(");
            out.push_str(
                &args
                    .iter()
                    .map(|arg| format_expr(arg, indent))
                    .collect::<Vec<_>>()
                    .join(", "),
            );
            out.push(')');
            out
        }
        ExprKind::Return(expr) =>
        {
            if let Some(expr) = expr
            {
                format!("return {}", format_expr(expr, indent))
            }
            else
            {
                "return".to_string()
            }
        }
        ExprKind::If { .. }
        | ExprKind::While { .. }
        | ExprKind::For { .. }
        | ExprKind::Loop { .. }
        | ExprKind::Collect { .. }
        | ExprKind::FunctionDef { .. }
        | ExprKind::MethodDef { .. }
        | ExprKind::AnonymousFunction { .. }
        | ExprKind::StructDef { .. }
        | ExprKind::Block(_) => format_stmt(expr, indent),
        ExprKind::Array(elements) =>
        {
            format!(
                "[{}]",
                elements
                    .iter()
                    .map(|elem| format_expr(elem, indent))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
        ExprKind::StructLiteral { name, fields } =>
        {
            let mut out = String::new();
            out.push_str(intern::symbol_name(*name).as_str());
            out.push_str(" {");
            if !fields.is_empty()
            {
                out.push(' ');
                out.push_str(
                    &fields
                        .iter()
                        .map(|(field, value)| {
                            format!(
                                "{}: {}",
                                intern::symbol_name(*field).as_str(),
                                format_expr(value, indent)
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", "),
                );
                out.push(' ');
            }
            out.push('}');
            out
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            format!("[{}; {}]", format_expr(generator, indent), format_expr(size, indent))
        }
        ExprKind::Map(entries) =>
        {
            let mut out = String::new();
            out.push('{');
            if !entries.is_empty()
            {
                out.push(' ');
                out.push_str(
                    &entries
                        .iter()
                        .map(|(k, v)| {
                            format!("{}: {}", format_expr(k, indent), format_expr(v, indent))
                        })
                        .collect::<Vec<_>>()
                        .join(", "),
                );
                out.push(' ');
            }
            out.push('}');
            out
        }
        ExprKind::Index { target, index } =>
        {
            format!("{}[{}]", format_expr(target, indent), format_expr(index, indent))
        }
        ExprKind::Slice { target, start, end } =>
        {
            format!(
                "{}[{}, {}]",
                format_expr(target, indent),
                format_expr(start, indent),
                format_expr(end, indent)
            )
        }
        ExprKind::Use(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. }
        | ExprKind::Load(_)
        | ExprKind::FilePublic(_)
        | ExprKind::FunctionPublic(_) => format_stmt(expr, indent),
        ExprKind::FormatString(parts) => format_format_string(parts, indent),
    }
}

fn format_params(params: &[Param]) -> String
{
    params
        .iter()
        .map(|param| {
            let mut out = String::new();
            if param.is_ref
            {
                out.push('&');
            }
            out.push_str(intern::symbol_name(param.name).as_str());
            if let Some(type_ann) = &param.type_ann
            {
                out.push(' ');
                out.push_str(&format_param_type(type_ann));
            }
            out
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_param_type(param_type: &ParamType) -> String
{
    match param_type
    {
        ParamType::Struct(fields) =>
        {
            let mut out = String::new();
            out.push('{');
            if !fields.is_empty()
            {
                out.push(' ');
                out.push_str(
                    &fields
                        .iter()
                        .map(|(name, ty)| {
                            format!(
                                "{}: {}",
                                intern::symbol_name(*name).as_str(),
                                format_type_ref(ty)
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", "),
                );
                out.push(' ');
            }
            out.push('}');
            out
        }
    }
}

fn format_closure(closure: &Closure, indent: usize) -> String
{
    let mut out = String::new();
    out.push('{');
    out.push(' ');
    out.push('|');
    out.push_str(&format_params(&closure.params));
    out.push('|');
    let body = format_stmt(&closure.body, indent + 1);
    if body.contains('\n')
    {
        out.push('\n');
        out.push_str(&body);
        out.push('\n');
        out.push_str(&indent_str(indent));
        out.push('}');
    }
    else
    {
        out.push(' ');
        out.push_str(body.trim());
        out.push(' ');
        out.push('}');
    }
    out
}

fn format_type_ref(type_ref: &TypeRef) -> String
{
    format_path(&type_ref.path)
}

fn format_path(path: &[intern::SymbolId]) -> String
{
    path.iter()
        .map(|seg| intern::symbol_name(*seg).as_str().to_string())
        .collect::<Vec<_>>()
        .join("::")
}

fn format_op(op: &Op) -> &'static str
{
    match op
    {
        Op::Add => "+",
        Op::Subtract => "-",
        Op::Multiply => "*",
        Op::Divide => "/",
        Op::Equal => "==",
        Op::NotEqual => "!=",
        Op::LessThan => "<",
        Op::GreaterThan => ">",
    }
}

fn format_int(value: i128, kind: IntKind) -> String
{
    let suffix = match kind
    {
        IntKind::I8 => "i8",
        IntKind::I16 => "i16",
        IntKind::I32 => "i32",
        IntKind::I64 => "",
        IntKind::I128 => "i128",
        IntKind::U8 | IntKind::U16 | IntKind::U32 | IntKind::U64 | IntKind::U128 => "",
    };
    if suffix.is_empty()
    {
        value.to_string()
    }
    else
    {
        format!("{value}{suffix}")
    }
}

fn format_uint(value: u128, kind: IntKind) -> String
{
    let suffix = match kind
    {
        IntKind::U8 => "u8",
        IntKind::U16 => "u16",
        IntKind::U32 => "u32",
        IntKind::U64 => "u64",
        IntKind::U128 => "u128",
        _ => "u64",
    };
    format!("{value}{suffix}")
}

fn format_float(value: f64, kind: FloatKind) -> String
{
    let base = {
        let mut s = value.to_string();
        if !s.contains('.') && !s.contains('e') && !s.contains('E')
        {
            s.push_str(".0");
        }
        s
    };
    let suffix = match kind
    {
        FloatKind::F32 => "f32",
        FloatKind::F64 => "",
        FloatKind::F128 => "f128",
    };
    if suffix.is_empty()
    {
        base
    }
    else
    {
        format!("{base}{suffix}")
    }
}

fn format_string_literal(input: &str) -> String
{
    let mut out = String::new();
    out.push('"');
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
    out.push('"');
    out
}

fn format_shell_literal(input: &str) -> String
{
    let mut out = String::new();
    out.push('`');
    for ch in input.chars()
    {
        match ch
        {
            '`' => out.push_str("\\`"),
            '\\' => out.push_str("\\\\"),
            _ => out.push(ch),
        }
    }
    out.push('`');
    out
}

fn format_format_string(parts: &[FormatPart], indent: usize) -> String
{
    let mut out = String::new();
    out.push_str("f\"");
    for part in parts
    {
        match part
        {
            FormatPart::Literal(text) =>
            {
                let mut literal = text.as_str().to_string();
                literal = literal.replace('{', "{{").replace('}', "}}");
                out.push_str(&escape_format_literal(&literal));
            }
            FormatPart::Expr { expr, spec } =>
            {
                out.push('{');
                out.push_str(&format_expr(expr, indent));
                if let Some(spec) = spec
                {
                    if let Some(precision) = spec.precision
                    {
                        out.push_str(&format!(":.{precision}"));
                    }
                }
                out.push('}');
            }
        }
    }
    out.push('"');
    out
}

fn escape_format_literal(input: &str) -> String
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

fn indent_str(level: usize) -> String
{
    INDENT.repeat(level)
}
