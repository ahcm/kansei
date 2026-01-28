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

fn send_response(stdout: &mut dyn Write, id: &serde_json::Value, result: serde_json::Value)
{
    let response = json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": result
    });
    let payload = response.to_string();
    let _ = write!(stdout, "Content-Length: {}\r\n\r\n", payload.len());
    let _ = stdout.write_all(payload.as_bytes());
    let _ = stdout.flush();
}

fn send_notification(stdout: &mut dyn Write, method: &str, params: serde_json::Value)
{
    let notif = json!({
        "jsonrpc": "2.0",
        "method": method,
        "params": params
    });
    let payload = notif.to_string();
    let _ = write!(stdout, "Content-Length: {}\r\n\r\n", payload.len());
    let _ = stdout.write_all(payload.as_bytes());
    let _ = stdout.flush();
}

fn publish_diagnostics(stdout: &mut dyn Write, uri: &str, message: Option<String>)
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
    );
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
        if let Some(rest) = line_trim.strip_prefix("Content-Length:")
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
    let stdin = io::stdin();
    let mut reader = io::BufReader::new(stdin.lock());
    let mut stdout = io::stdout();
    let mut docs: HashMap<String, String> = HashMap::new();

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

        match method
        {
            Some("initialize") =>
            {
                if let Some(id) = id
                {
                    send_response(
                        &mut stdout,
                        &id,
                        json!({
                            "capabilities": {
                                "textDocumentSync": 1
                            }
                        }),
                    );
                }
            }
            Some("shutdown") =>
            {
                if let Some(id) = id
                {
                    send_response(&mut stdout, &id, json!(null));
                }
            }
            Some("exit") => break,
            Some("textDocument/didOpen") =>
            {
                let params = value.get("params").cloned().unwrap_or(json!({}));
                if let Some(text_doc) = params.get("textDocument")
                {
                    if let (Some(uri), Some(text)) =
                        (text_doc.get("uri"), text_doc.get("text"))
                    {
                        let uri = uri.as_str().unwrap_or_default().to_string();
                        let text = text.as_str().unwrap_or_default().to_string();
                        let diag = parse_source(&text).err();
                        docs.insert(uri.clone(), text);
                        publish_diagnostics(&mut stdout, &uri, diag);
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
                            publish_diagnostics(&mut stdout, &uri, diag);
                        }
                    }
                }
            }
            _ =>
            {
                if let Some(id) = id
                {
                    send_response(&mut stdout, &id, json!(null));
                }
            }
        }
    }

    let _ = stdout.flush();
    0
}
