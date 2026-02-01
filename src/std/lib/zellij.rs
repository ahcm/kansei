use super::LibMap;
use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::rc::Rc;
use zellij_client::os_input_output::{get_cli_client_os_input, get_client_os_input, ClientOsApi};
use zellij_client::{start_client, ClientInfo};
use zellij_utils::cli::CliArgs;
use zellij_utils::consts::{VERSION, ZELLIJ_SOCK_DIR};
use zellij_utils::envs;
use zellij_utils::ipc::ClientToServerMsg;
use zellij_utils::setup::Setup;

#[cfg(unix)]
use std::os::unix::fs::FileTypeExt;

fn str_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn opt_str_arg(args: &[Value], idx: usize, name: &str) -> Result<Option<String>, String>
{
    match args.get(idx)
    {
        None | Some(Value::Nil) => Ok(None),
        Some(Value::String(s)) => Ok(Some(s.as_str().to_string())),
        _ => Err(format!("{name} expects a string or nil")),
    }
}

#[cfg(unix)]
fn is_session_socket(entry: &fs::DirEntry) -> bool
{
    entry.file_type().map(|ty| ty.is_socket()).unwrap_or(false)
}

#[cfg(not(unix))]
fn is_session_socket(_entry: &fs::DirEntry) -> bool
{
    true
}

fn session_socket_path(session: &str) -> PathBuf
{
    let mut sock_dir = ZELLIJ_SOCK_DIR.clone();
    sock_dir.push(session);
    sock_dir
}

fn list_session_names() -> Vec<String>
{
    let mut sessions = Vec::new();
    let entries = match fs::read_dir(&*ZELLIJ_SOCK_DIR)
    {
        Ok(entries) => entries,
        Err(_) => return sessions,
    };
    for entry in entries.flatten()
    {
        if !is_session_socket(&entry)
        {
            continue;
        }
        let name = entry.file_name().to_string_lossy().to_string();
        if !name.is_empty()
        {
            sessions.push(name);
        }
    }
    sessions.sort();
    sessions
}

fn resolve_session_name(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    if let Some(session_name) = opt_str_arg(args, idx, name)?
    {
        return Ok(session_name);
    }
    if let Ok(session_name) = envs::get_session_name()
    {
        return Ok(session_name);
    }
    let sessions = list_session_names();
    match sessions.len()
    {
        0 => Err("Zellij.attach: no active sessions found".to_string()),
        1 => Ok(sessions[0].clone()),
        _ => Err("Zellij.attach: multiple sessions found; pass a session name".to_string()),
    }
}

fn attach_session(session: &str) -> Result<Value, String>
{
    if !session_socket_path(session).exists()
    {
        return Err(format!("Zellij.attach: session '{session}' not found"));
    }

    let cli_args = CliArgs {
        session: Some(session.to_string()),
        ..Default::default()
    };
    let (config, _layout, config_options, _, _) =
        Setup::from_cli_args(&cli_args).map_err(|e| format!("Zellij.attach failed: {e}"))?;

    let os_input = get_client_os_input()
        .map_err(|e| format!("Zellij.attach failed: {e}"))?;
    let attach_options = config_options.clone();
    let _ = start_client(
        Box::new(os_input),
        cli_args,
        config,
        config_options,
        ClientInfo::Attach(session.to_string(), attach_options),
        None,
        None,
        None,
        false,
        false,
        false,
    );

    Ok(Value::Nil)
}

fn native_zellij_attach(args: &[Value]) -> Result<Value, String>
{
    let session = resolve_session_name(args, 0, "Zellij.attach")?;
    attach_session(&session)
}

fn native_zellij_list_sessions(_args: &[Value]) -> Result<Value, String>
{
    let sessions = list_session_names()
        .into_iter()
        .map(|name| Value::String(intern::intern_owned(name)))
        .collect::<Vec<_>>();
    Ok(Value::Array(Rc::new(RefCell::new(sessions))))
}

fn native_zellij_new_session(args: &[Value]) -> Result<Value, String>
{
    let session = str_arg(args, 0, "Zellij.new_session")?;
    if session_socket_path(&session).exists()
    {
        return attach_session(&session);
    }

    match Command::new("zellij").arg("-s").arg(&session).status()
    {
        Ok(status) if status.success() => Ok(Value::Nil),
        Ok(status) => Err(format!(
            "Zellij.new_session failed: zellij exited with {status}"
        )),
        Err(err) => Err(format!(
            "Zellij.new_session failed: could not run 'zellij' ({err})"
        )),
    }
}

fn native_zellij_kill_session(args: &[Value]) -> Result<Value, String>
{
    let session = str_arg(args, 0, "Zellij.kill_session")?;
    let socket = session_socket_path(&session);
    if !socket.exists()
    {
        return Err(format!("Zellij.kill_session: session '{session}' not found"));
    }

    let mut os_input = get_cli_client_os_input()
        .map_err(|e| format!("Zellij.kill_session failed: {e}"))?;
    envs::set_session_name(session.to_string());
    os_input.update_session_name(session.to_string());
    os_input.connect_to_server(&socket);
    os_input.send_to_server(ClientToServerMsg::KillSession);

    Ok(Value::Boolean(true))
}

fn native_zellij_version(_args: &[Value]) -> Result<Value, String>
{
    Ok(Value::String(intern::intern_owned(VERSION.to_string())))
}

pub fn build_zellij_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(intern::intern("attach"), Value::NativeFunction(native_zellij_attach));
    map.insert(
        intern::intern("list_sessions"),
        Value::NativeFunction(native_zellij_list_sessions),
    );
    map.insert(
        intern::intern("new_session"),
        Value::NativeFunction(native_zellij_new_session),
    );
    map.insert(
        intern::intern("kill_session"),
        Value::NativeFunction(native_zellij_kill_session),
    );
    map.insert(intern::intern("version"), Value::NativeFunction(native_zellij_version));
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    map.insert(intern::intern("zellij"), build_zellij_module());
}
