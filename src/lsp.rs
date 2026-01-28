use crate::lexer::Lexer;
use crate::parser::Parser;
use serde_json::json;
use std::collections::HashMap;
use std::io::{self, BufRead, Write};

fn parse_source(source: &str) -> Result<(), String>
{
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        parser.parse()
    }));
    match parse_result
    {
        Ok(_) => Ok(()),
        Err(e) =>
        {
            if let Some(s) = e.downcast_ref::<&str>()
            {
                Err(s.to_string())
            }
            else if let Some(s) = e.downcast_ref::<String>()
            {
                Err(s.clone())
            }
            else
            {
                Err("Syntax Error".to_string())
            }
        }
    }
}

fn parse_error_location(message: &str) -> (usize, usize, String)
{
    let mut line = 0usize;
    let col = 0usize;
    if let Some(idx) = message.find("line ")
    {
        let rest = &message[idx + 5..];
        let mut digits = String::new();
        for ch in rest.chars()
        {
            if ch.is_ascii_digit()
            {
                digits.push(ch);
            }
            else
            {
                break;
            }
        }
        if let Ok(parsed) = digits.parse::<usize>()
        {
            line = parsed.saturating_sub(1);
        }
    }
    (line, col, message.to_string())
}

fn send_response(
    stdout: &mut dyn Write,
    id: &serde_json::Value,
    result: serde_json::Value,
) -> io::Result<()>
{
    let response = json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": result
    });
    let payload = response.to_string();
    write!(stdout, "Content-Length: {}\r\n\r\n", payload.len())?;
    stdout.write_all(payload.as_bytes())?;
    stdout.flush()?;
    Ok(())
}

fn send_notification(
    stdout: &mut dyn Write,
    method: &str,
    params: serde_json::Value,
) -> io::Result<()>
{
    let notif = json!({
        "jsonrpc": "2.0",
        "method": method,
        "params": params
    });
    let payload = notif.to_string();
    write!(stdout, "Content-Length: {}\r\n\r\n", payload.len())?;
    stdout.write_all(payload.as_bytes())?;
    stdout.flush()?;
    Ok(())
}

fn publish_diagnostics(stdout: &mut dyn Write, uri: &str, message: Option<String>) -> io::Result<()>
{
    let diagnostics = if let Some(message) = message
    {
        let (line, col, msg) = parse_error_location(&message);
        vec![json!({
            "range": {
                "start": { "line": line, "character": col },
                "end": { "line": line, "character": col + 1 }
            },
            "severity": 1,
            "source": "kansei",
            "message": msg
        })]
    }
    else
    {
        Vec::new()
    };

    send_notification(
        stdout,
        "textDocument/publishDiagnostics",
        json!({
            "uri": uri,
            "diagnostics": diagnostics
        }),
    )
}

fn read_message(reader: &mut dyn BufRead) -> Option<String>
{
    let mut content_length = None;
    let mut line = String::new();
    loop
    {
        line.clear();
        if reader.read_line(&mut line).ok()? == 0
        {
            return None;
        }
        let line_trim = line.trim_end_matches(&['\r', '\n'][..]);
        if line_trim.is_empty()
        {
            break;
        }
        let lower = line_trim.to_ascii_lowercase();
        if let Some(rest) = lower.strip_prefix("content-length:")
        {
            let len_str = rest.trim();
            if let Ok(len) = len_str.parse::<usize>()
            {
                content_length = Some(len);
            }
        }
    }
    let len = content_length?;
    let mut buf = vec![0u8; len];
    reader.read_exact(&mut buf).ok()?;
    String::from_utf8(buf).ok()
}

pub fn run_lsp() -> i32
{
    std::panic::set_hook(Box::new(|_| {}));
    let stdin = io::stdin();
    let mut reader = io::BufReader::new(stdin.lock());
    let mut stdout = io::stdout();
    let mut docs: HashMap<String, String> = HashMap::new();
    let mut doc_symbols: HashMap<String, HashMap<String, (usize, usize)>> = HashMap::new();

    loop
    {
        let msg = match read_message(&mut reader)
        {
            Some(msg) => msg,
            None => break,
        };
        let value: serde_json::Value = match serde_json::from_str(&msg)
        {
            Ok(val) => val,
            Err(_) => continue,
        };
        let method = value.get("method").and_then(|m| m.as_str());
        let id = value.get("id").cloned();

        let handle_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            handle_request(
                &mut stdout,
                &value,
                method,
                id,
                &mut docs,
                &mut doc_symbols,
            )
        }));

        match handle_result
        {
            Ok(Ok(LoopControl::Continue)) => {}
            Ok(Ok(LoopControl::Break)) => break,
            Ok(Err(err)) =>
            {
                if err.kind() == io::ErrorKind::BrokenPipe
                {
                    break;
                }
            }
            Err(_) => {}
        }
    }

    let _ = stdout.flush();
    0
}

fn parse_ast_quiet(source: &str) -> Result<crate::ast::Expr, String>
{
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        parser.parse()
    }));
    match parse_result
    {
        Ok(ast) => Ok(ast),
        Err(e) =>
        {
            if let Some(s) = e.downcast_ref::<&str>()
            {
                Err(s.to_string())
            }
            else if let Some(s) = e.downcast_ref::<String>()
            {
                Err(s.clone())
            }
            else
            {
                Err("Syntax Error".to_string())
            }
        }
    }
}

fn collect_symbols(ast: &crate::ast::Expr) -> HashMap<String, (usize, usize)>
{
    use crate::ast::ExprKind;
    let mut symbols = HashMap::new();
    fn visit(expr: &crate::ast::Expr, symbols: &mut HashMap<String, (usize, usize)>)
    {
        match &expr.kind
        {
            ExprKind::FunctionDef { name, .. } =>
            {
                symbols.insert(
                    crate::intern::symbol_name(*name).to_string(),
                    (
                        expr.line.saturating_sub(1),
                        expr.column.saturating_sub(1),
                    ),
                );
            }
            ExprKind::MethodDef { name, .. } =>
            {
                symbols.insert(
                    crate::intern::symbol_name(*name).to_string(),
                    (
                        expr.line.saturating_sub(1),
                        expr.column.saturating_sub(1),
                    ),
                );
            }
            ExprKind::StructDef { name, .. } =>
            {
                symbols.insert(
                    crate::intern::symbol_name(*name).to_string(),
                    (
                        expr.line.saturating_sub(1),
                        expr.column.saturating_sub(1),
                    ),
                );
            }
            ExprKind::Block(stmts) =>
            {
                for stmt in stmts
                {
                    visit(stmt, symbols);
                }
            }
            ExprKind::If { then_branch, else_branch, condition } =>
            {
                visit(condition, symbols);
                visit(then_branch, symbols);
                if let Some(else_branch) = else_branch
                {
                    visit(else_branch, symbols);
                }
            }
            ExprKind::While { condition, body } =>
            {
                visit(condition, symbols);
                visit(body, symbols);
            }
            ExprKind::For { iterable, body, .. } =>
            {
                visit(iterable, symbols);
                visit(body, symbols);
            }
            ExprKind::Call { function, args, block, .. } =>
            {
                visit(function, symbols);
                for arg in args
                {
                    visit(arg, symbols);
                }
                if let Some(block) = block
                {
                    visit(&block.body, symbols);
                }
            }
            ExprKind::Assignment { value, .. }
            | ExprKind::IndexAssignment { value, .. }
            | ExprKind::Not(value)
            | ExprKind::Clone(value)
            | ExprKind::EnvFreeze(value)
            | ExprKind::ErrorRaise(value) => visit(value, symbols),
            ExprKind::Return(value) =>
            {
                if let Some(value) = value
                {
                    visit(value, symbols);
                }
            }
            ExprKind::BinaryOp { left, right, .. }
            | ExprKind::And { left, right }
            | ExprKind::AndBool { left, right }
            | ExprKind::Or { left, right }
            | ExprKind::OrBool { left, right } =>
            {
                visit(left, symbols);
                visit(right, symbols);
            }
            ExprKind::Array(items) =>
            {
                for item in items
                {
                    visit(item, symbols);
                }
            }
            ExprKind::ArrayGenerator { generator, size } =>
            {
                visit(generator, symbols);
                visit(size, symbols);
            }
            ExprKind::Map(map_items) =>
            {
                for (_, value) in map_items
                {
                    visit(value, symbols);
                }
            }
            ExprKind::FunctionPublic(inner)
            | ExprKind::FilePublic(inner) => visit(inner, symbols),
            ExprKind::Index { target, index } =>
            {
                visit(target, symbols);
                visit(index, symbols);
            }
            ExprKind::Slice { target, start, end } =>
            {
                visit(target, symbols);
                visit(start, symbols);
                visit(end, symbols);
            }
            ExprKind::Yield(items) =>
            {
                for item in items
                {
                    visit(item, symbols);
                }
            }
            ExprKind::Result { body, else_expr, .. } =>
            {
                visit(body, symbols);
                visit(else_expr, symbols);
            }
            ExprKind::Loop { count, body, .. }
            | ExprKind::Collect { count, body, .. } =>
            {
                visit(count, symbols);
                visit(body, symbols);
            }
            ExprKind::FormatString(parts) =>
            {
                for part in parts
                {
                    if let crate::ast::FormatPart::Expr { expr, .. } = part
                    {
                        visit(expr, symbols);
                    }
                }
            }
            _ => {}
        }
    }
    visit(ast, &mut symbols);
    symbols
}

fn word_at_position(source: &str, line: usize, character: usize) -> Option<String>
{
    let mut current_line = 0usize;
    for line_str in source.lines()
    {
        if current_line == line
        {
            let bytes = line_str.as_bytes();
            if character >= bytes.len()
            {
                return None;
            }
            let mut start = character;
            while start > 0 && is_ident_byte(bytes[start - 1])
            {
                start -= 1;
            }
            let mut end = character;
            while end < bytes.len() && is_ident_byte(bytes[end])
            {
                end += 1;
            }
            if start < end
            {
                return Some(line_str[start..end].to_string());
            }
            return None;
        }
        current_line += 1;
    }
    None
}

fn is_ident_byte(b: u8) -> bool
{
    b.is_ascii_alphanumeric() || b == b'_'
}

enum LoopControl
{
    Continue,
    Break,
}

fn handle_request(
    stdout: &mut dyn Write,
    value: &serde_json::Value,
    method: Option<&str>,
    id: Option<serde_json::Value>,
    docs: &mut HashMap<String, String>,
    doc_symbols: &mut HashMap<String, HashMap<String, (usize, usize)>>,
) -> io::Result<LoopControl>
{
    match method
    {
        Some("initialize") =>
        {
            if let Some(id) = id
            {
                send_response(
                    stdout,
                    &id,
                    json!({
                        "capabilities": {
                            "textDocumentSync": 1,
                            "hoverProvider": true
                        }
                    }),
                )?;
            }
        }
        Some("shutdown") =>
        {
            if let Some(id) = id
            {
                send_response(stdout, &id, json!(null))?;
            }
        }
        Some("exit") => return Ok(LoopControl::Break),
        Some("textDocument/didOpen") =>
        {
            let params = value.get("params").cloned().unwrap_or(json!({}));
            if let Some(text_doc) = params.get("textDocument")
            {
                if let (Some(uri), Some(text)) = (text_doc.get("uri"), text_doc.get("text"))
                {
                    let uri = uri.as_str().unwrap_or_default().to_string();
                    let text = text.as_str().unwrap_or_default().to_string();
                    let diag = parse_source(&text).err();
                    docs.insert(uri.clone(), text);
                    if let Ok(ast) = parse_ast_quiet(docs.get(&uri).unwrap())
                    {
                        let symbols = collect_symbols(&ast);
                        doc_symbols.insert(uri.clone(), symbols);
                    }
                    publish_diagnostics(stdout, &uri, diag)?;
                }
            }
        }
        Some("textDocument/didChange") =>
        {
            let params = value.get("params").cloned().unwrap_or(json!({}));
            let uri = params
                .get("textDocument")
                .and_then(|doc| doc.get("uri"))
                .and_then(|u| u.as_str())
                .unwrap_or_default()
                .to_string();
            if let Some(changes) = params.get("contentChanges").and_then(|c| c.as_array())
            {
                if let Some(change) = changes.last()
                {
                    if let Some(text) = change.get("text").and_then(|t| t.as_str())
                    {
                        let diag = parse_source(text).err();
                        docs.insert(uri.clone(), text.to_string());
                        if let Ok(ast) = parse_ast_quiet(docs.get(&uri).unwrap())
                        {
                            let symbols = collect_symbols(&ast);
                            doc_symbols.insert(uri.clone(), symbols);
                        }
                        publish_diagnostics(stdout, &uri, diag)?;
                    }
                }
            }
        }
        Some("textDocument/hover") =>
        {
            let params = value.get("params").cloned().unwrap_or(json!({}));
            let uri = params
                .get("textDocument")
                .and_then(|doc| doc.get("uri"))
                .and_then(|u| u.as_str())
                .unwrap_or_default()
                .to_string();
            let pos = params.get("position").cloned().unwrap_or(json!({}));
            let line = pos.get("line").and_then(|l| l.as_u64()).unwrap_or(0) as usize;
            let character = pos
                .get("character")
                .and_then(|c| c.as_u64())
                .unwrap_or(0) as usize;
            let symbol = docs
                .get(&uri)
                .and_then(|text| word_at_position(text, line, character));
            let mut contents = None;
            if let Some(symbol) = symbol
            {
                if let Some(symbols) = doc_symbols.get(&uri)
                {
                    if let Some((def_line, def_col)) = symbols.get(&symbol)
                    {
                        contents = Some(format!(
                            "`{symbol}` defined at line {}:{}",
                            def_line + 1,
                            def_col + 1
                        ));
                    }
                }
                if contents.is_none()
                {
                    contents = Some(format!("`{symbol}`"));
                }
            }
            if let Some(id) = id
            {
                let result = if let Some(contents) = contents
                {
                    json!({
                        "contents": {
                            "kind": "markdown",
                            "value": contents
                        }
                    })
                }
                else
                {
                    json!(null)
                };
                send_response(stdout, &id, result)?;
            }
        }
        _ =>
        {
            if let Some(id) = id
            {
                send_response(stdout, &id, json!(null))?;
            }
        }
    }
    Ok(LoopControl::Continue)
}
