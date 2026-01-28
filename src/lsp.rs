use serde_json::json;
use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::fs::OpenOptions;
use std::sync::atomic::{AtomicBool, Ordering};
use std::path::{Path, PathBuf};
use libc::{SIGPIPE, SIG_IGN};
use libc::{SIGINT, SIGTERM, SIGHUP};

static LSP_SHUTDOWN: AtomicBool = AtomicBool::new(false);

extern "C" fn lsp_signal_handler(_sig: i32)
{
    LSP_SHUTDOWN.store(true, Ordering::SeqCst);
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

fn read_message(reader: &mut dyn BufRead, log: &mut LspLog) -> Option<String>
{
    let mut content_length = None;
    loop
    {
        let mut line = String::new();
        let bytes = reader.read_line(&mut line).ok()?;
        if bytes == 0
        {
            log.write("lsp: read_line returned 0 (stdin closed)\n");
            return None;
        }
        let line_trim = line.trim_end_matches(&['\r', '\n'][..]);
        log.write(&format!("lsp: header line: {line_trim}\n"));
        if line_trim.is_empty()
        {
            if let Some(len) = content_length
            {
                let mut buf = vec![0u8; len];
                if reader.read_exact(&mut buf).is_err()
                {
                    log.write("lsp: failed to read body\n");
                    return None;
                }
                log.write(&format!("lsp: read body len {len}\n"));
                return String::from_utf8(buf).ok();
            }
            log.write("lsp: header end without content-length\n");
            content_length = None;
            continue;
        }
        let lower = line_trim.to_ascii_lowercase();
        if let Some(rest) = lower.strip_prefix("content-length:")
        {
            let len_str = rest.trim();
            if let Ok(len) = len_str.parse::<usize>()
            {
                content_length = Some(len);
                log.write(&format!("lsp: content-length {len}\n"));
            }
        }
    }
}

pub fn run_lsp() -> i32
{
    std::panic::set_hook(Box::new(|_| {}));
    unsafe {
        libc::signal(SIGPIPE, SIG_IGN);
        libc::signal(SIGTERM, lsp_signal_handler as *const () as libc::sighandler_t);
        libc::signal(SIGINT, lsp_signal_handler as *const () as libc::sighandler_t);
        libc::signal(SIGHUP, lsp_signal_handler as *const () as libc::sighandler_t);
    }
    let stdin = io::stdin();
    let mut reader = io::BufReader::new(stdin.lock());
    let mut stdout = io::stdout();
    let mut docs: HashMap<String, String> = HashMap::new();
    let mut doc_symbols: HashMap<String, HashMap<String, (usize, usize)>> = HashMap::new();
    let mut workspace_root: Option<PathBuf> = None;
    let mut workspace_index = WorkspaceIndex::new();
    let mut log = LspLog::new();

    loop
    {
        if LSP_SHUTDOWN.load(Ordering::SeqCst)
        {
            log.write("lsp: shutdown signal received\n");
            break;
        }
        let msg = match read_message(&mut reader, &mut log)
        {
            Some(msg) => msg,
            None =>
            {
                log.write("lsp: read_message returned None\n");
                break;
            }
        };
        let value: serde_json::Value = match serde_json::from_str(&msg)
        {
            Ok(val) => val,
            Err(err) =>
            {
                log.write(&format!("lsp: json parse error: {err}\n"));
                continue;
            }
        };
        let method = value.get("method").and_then(|m| m.as_str());
        let id = value.get("id").cloned();
        if let Some(method) = method
        {
            log.write(&format!("lsp: method {method}\n"));
        }

        let handle_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            handle_request(
                &mut stdout,
                &value,
                method,
                id,
                &mut docs,
                &mut doc_symbols,
                &mut workspace_root,
                &mut workspace_index,
                &mut log,
            )
        }));

        match handle_result
        {
            Ok(Ok(LoopControl::Continue)) =>
            {
                log.write("lsp: loop continue\n");
            }
            Ok(Ok(LoopControl::Break)) =>
            {
                log.write("lsp: exit (LoopControl::Break)\n");
                break;
            }
            Ok(Err(err)) =>
            {
                log.write(&format!("lsp: write error: {err}\n"));
                if err.kind() == io::ErrorKind::BrokenPipe
                {
                    log.write("lsp: exit (BrokenPipe)\n");
                    break;
                }
            }
            Err(_) =>
            {
                log.write("lsp: panic in request handler\n");
            }
        }

    }

    let _ = stdout.flush();
    0
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

fn scan_symbols(source: &str) -> HashMap<String, (usize, usize)>
{
    let mut symbols = HashMap::new();
    for (line_idx, line) in source.lines().enumerate()
    {
        let trimmed = line.trim_start();
        let leading = line.len().saturating_sub(trimmed.len());
        if let Some(rest) = trimmed.strip_prefix("fn ")
        {
            if let Some(pos) = rest.find('(')
            {
                let name_part = rest[..pos].trim();
                if !name_part.is_empty()
                {
                    let name = name_part
                        .replace("::", ".")
                        .rsplit('.')
                        .next()
                        .unwrap_or(name_part)
                        .to_string();
                    let col = leading + trimmed.find(name_part).unwrap_or(0);
                    symbols.insert(name, (line_idx, col));
                }
            }
        }
        if let Some(rest) = trimmed.strip_prefix("struct ")
        {
            let name_part = rest
                .split(|c: char| c.is_whitespace() || c == '{')
                .next()
                .unwrap_or("")
                .trim();
            if !name_part.is_empty()
            {
                let col = leading + trimmed.find(name_part).unwrap_or(0);
                symbols.insert(name_part.to_string(), (line_idx, col));
            }
        }
    }
    symbols
}

fn find_workspace_definition(
    symbol: &str,
    root: Option<&Path>,
    index: &mut WorkspaceIndex,
) -> Option<Location>
{
    if !index.built
    {
        if let Some(root) = root
        {
            index.symbols = build_workspace_index(root);
        }
        index.built = true;
    }
    index.symbols.get(symbol).and_then(|list| list.first()).cloned()
}

fn build_workspace_index(root: &Path) -> HashMap<String, Vec<Location>>
{
    let mut out: HashMap<String, Vec<Location>> = HashMap::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(path) = stack.pop()
    {
        if should_skip_dir(&path)
        {
            continue;
        }
        if path.is_dir()
        {
            if let Ok(entries) = std::fs::read_dir(&path)
            {
                for entry in entries.flatten()
                {
                    stack.push(entry.path());
                }
            }
            continue;
        }
        if path.extension().and_then(|s| s.to_str()) != Some("ks")
        {
            continue;
        }
        let content = match std::fs::read_to_string(&path)
        {
            Ok(content) => content,
            Err(_) => continue,
        };
        let symbols = scan_symbols(&content);
        let uri = path_to_uri(&path);
        for (name, (line, col)) in symbols
        {
            out.entry(name)
                .or_default()
                .push(Location { uri: uri.clone(), line, col });
        }
    }
    out
}

fn should_skip_dir(path: &Path) -> bool
{
    let s = path.to_string_lossy();
    s.contains("/.git/")
        || s.contains("/target/")
        || s.contains("/wasm/target/")
        || s.contains("/node_modules/")
}

fn path_to_uri(path: &Path) -> String
{
    let abs = if path.is_absolute()
    {
        path.to_path_buf()
    }
    else
    {
        std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")).join(path)
    };
    let mut uri = format!("file://{}", abs.to_string_lossy());
    uri = uri.replace(' ', "%20");
    uri
}

fn uri_to_path(uri: &str) -> Option<PathBuf>
{
    if let Some(rest) = uri.strip_prefix("file://")
    {
        Some(PathBuf::from(rest.replace("%20", " ")))
    }
    else
    {
        None
    }
}

fn location_to_json(loc: Location) -> serde_json::Value
{
    json!({
        "uri": loc.uri,
        "range": {
            "start": { "line": loc.line, "character": loc.col },
            "end": { "line": loc.line, "character": loc.col + 1 }
        }
    })
}

enum LoopControl
{
    Continue,
    Break,
}

struct LspLog
{
    file: Option<std::fs::File>,
}


impl LspLog
{
    fn new() -> Self
    {
        let path = std::env::var("KANSEI_LSP_LOG").ok();
        let file = path.and_then(|p| {
            OpenOptions::new().create(true).append(true).open(p).ok()
        });
        Self { file }
    }

    fn write(&mut self, msg: &str)
    {
        if let Some(file) = self.file.as_mut()
        {
            let _ = file.write_all(msg.as_bytes());
            let _ = file.flush();
        }
    }
}

struct WorkspaceIndex
{
    built: bool,
    symbols: HashMap<String, Vec<Location>>,
}

impl WorkspaceIndex
{
    fn new() -> Self
    {
        Self {
            built: false,
            symbols: HashMap::new(),
        }
    }
}

#[derive(Clone)]
struct Location
{
    uri: String,
    line: usize,
    col: usize,
}

fn handle_request(
    stdout: &mut dyn Write,
    value: &serde_json::Value,
    method: Option<&str>,
    id: Option<serde_json::Value>,
    docs: &mut HashMap<String, String>,
    doc_symbols: &mut HashMap<String, HashMap<String, (usize, usize)>>,
    workspace_root: &mut Option<PathBuf>,
    workspace_index: &mut WorkspaceIndex,
    log: &mut LspLog,
) -> io::Result<LoopControl>
{
    match method
    {
        Some("initialize") =>
        {
            if let Some(id) = id
            {
                if let Some(params) = value.get("params")
                {
                    if let Some(root) = params
                        .get("rootUri")
                        .and_then(|u| u.as_str())
                        .and_then(uri_to_path)
                    {
                        *workspace_root = Some(root);
                    }
                    else if let Some(root) = params
                        .get("rootPath")
                        .and_then(|u| u.as_str())
                    {
                        *workspace_root = Some(PathBuf::from(root));
                    }
                }
                log.write("lsp: sending initialize response\n");
                send_response(
                    stdout,
                    &id,
                    json!({
                            "capabilities": {
                                "textDocumentSync": 1,
                                "hoverProvider": true,
                                "definitionProvider": true
                            }
                        }),
                    )?;
                }
            }
        Some("initialized") =>
        {
            log.write("lsp: initialized notification\n");
        }
        Some("shutdown") =>
        {
            if let Some(id) = id
            {
                log.write("lsp: sending shutdown response\n");
                send_response(stdout, &id, json!(null))?;
            }
        }
        Some("exit") => return Ok(LoopControl::Break),
        Some("textDocument/didOpen") =>
        {
            log.write("lsp: didOpen handler start\n");
            let params = value.get("params").cloned().unwrap_or(json!({}));
            if let Some(text_doc) = params.get("textDocument")
            {
                log.write("lsp: didOpen has textDocument\n");
                if let (Some(uri), Some(text)) = (text_doc.get("uri"), text_doc.get("text"))
                {
                    log.write("lsp: didOpen has uri/text\n");
                    let uri = uri.as_str().unwrap_or_default().to_string();
                    let text = text.as_str().unwrap_or_default().to_string();
                    docs.insert(uri.clone(), text.clone());
                    doc_symbols.insert(uri.clone(), scan_symbols(&text));
                    let _ = publish_diagnostics(stdout, &uri, None);
                }
                else
                {
                    log.write("lsp: didOpen missing uri/text\n");
                }
            }
            else
            {
                log.write("lsp: didOpen missing textDocument\n");
            }
            return Ok(LoopControl::Continue);
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
                        let text = text.to_string();
                        docs.insert(uri.clone(), text.clone());
                        doc_symbols.insert(uri.clone(), scan_symbols(&text));
                        let _ = publish_diagnostics(stdout, &uri, None);
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
                log.write("lsp: sending hover response\n");
                send_response(stdout, &id, result)?;
            }
        }
        Some("textDocument/definition") =>
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
            let location = if let Some(symbol) = symbol
            {
                if let Some(symbols) = doc_symbols.get(&uri)
                {
                    if let Some((def_line, def_col)) = symbols.get(&symbol)
                    {
                        Some(json!({
                            "uri": uri,
                            "range": {
                                "start": { "line": def_line, "character": def_col },
                                "end": { "line": def_line, "character": def_col + 1 }
                            }
                        }))
                    }
                    else
                    {
                        find_workspace_definition(
                            &symbol,
                            workspace_root.as_deref(),
                            workspace_index,
                        )
                        .map(|loc| location_to_json(loc))
                    }
                }
                else
                {
                    find_workspace_definition(
                        &symbol,
                        workspace_root.as_deref(),
                        workspace_index,
                    )
                    .map(|loc| location_to_json(loc))
                }
            }
            else
            {
                None
            };

            if let Some(id) = id
            {
                let result = if let Some(location) = location
                {
                    json!(location)
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
                log.write("lsp: sending default response\n");
                send_response(stdout, &id, json!(null))?;
            }
        }
    }
    Ok(LoopControl::Continue)
}
