use super::LibMap;
use crate::ast::{IntKind, TypeRef};
use crate::intern;
use crate::value::{MapValue, StructField, StructInstance, StructType, Value};
use russh::client::{AuthResult, Handler as ClientHandlerTrait};
use russh::server::Handler as ServerHandlerTrait;
use russh::{Channel, ChannelId, ChannelMsg, Disconnect};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, Sender};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::Duration;
use tokio::io::AsyncWriteExt;
use tokio::runtime::{Builder, Runtime};
use tokio::sync::watch;

thread_local! {
    static RUSSH_CLIENT_TYPE: RefCell<Option<Rc<StructType>>> = RefCell::new(None);
    static RUSSH_CHANNEL_TYPE: RefCell<Option<Rc<StructType>>> = RefCell::new(None);
    static RUSSH_SERVER_TYPE: RefCell<Option<Rc<StructType>>> = RefCell::new(None);
}

fn any_type_ref() -> TypeRef
{
    TypeRef {
        path: vec![intern::intern_symbol("Any")],
    }
}

fn int_value(value: i128) -> Value
{
    Value::Integer {
        value,
        kind: IntKind::I64,
    }
}

fn bool_arg(args: &[Value], idx: usize, name: &str, default: bool) -> Result<bool, String>
{
    match args.get(idx)
    {
        Some(Value::Boolean(b)) => Ok(*b),
        Some(Value::Nil) | None => Ok(default),
        _ => Err(format!("{name} expects a boolean")),
    }
}

fn int_arg(args: &[Value], idx: usize, name: &str) -> Result<i128, String>
{
    match args.get(idx)
    {
        Some(Value::Integer { value, .. }) => Ok(*value),
        Some(Value::Unsigned { value, .. }) => Ok(*value as i128),
        _ => Err(format!("{name} expects an integer")),
    }
}

fn string_arg(args: &[Value], idx: usize, name: &str) -> Result<String, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_str().to_string()),
        Some(v) => Ok(v.to_string()),
        _ => Err(format!("{name} expects a string")),
    }
}

fn bytes_arg(args: &[Value], idx: usize, name: &str) -> Result<Vec<u8>, String>
{
    match args.get(idx)
    {
        Some(Value::String(s)) => Ok(s.as_bytes().to_vec()),
        Some(Value::Bytes(b)) => Ok(b.as_ref().clone()),
        Some(Value::ByteBuf(b)) => Ok(b.borrow().clone()),
        Some(Value::BytesView(view)) =>
        {
            let end = view.offset.saturating_add(view.len);
            match &view.source
            {
                crate::value::BytesViewSource::Mmap(mmap) => Ok(mmap[view.offset..end].to_vec()),
                crate::value::BytesViewSource::MmapMut(mmap) =>
                {
                    let data = mmap.borrow();
                    Ok(data[view.offset..end].to_vec())
                }
            }
        }
        _ => Err(format!("{name} expects bytes or string")),
    }
}

fn map_value(entries: Vec<(&str, Value)>) -> Value
{
    let mut map = FxHashMap::default();
    for (k, v) in entries
    {
        map.insert(intern::intern(k), v);
    }
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

fn runtime() -> Result<&'static Runtime, String>
{
    static RT: OnceLock<Runtime> = OnceLock::new();
    if let Some(rt) = RT.get()
    {
        return Ok(rt);
    }

    let rt = Builder::new_multi_thread()
        .enable_all()
        .build()
        .map_err(|e| format!("russh runtime init failed: {e}"))?;
    let _ = RT.set(rt);
    RT.get()
        .ok_or_else(|| "russh runtime init failed".to_string())
}

struct ClientHandler
{
    accept_any_host_key: bool,
}

impl ClientHandlerTrait for ClientHandler
{
    type Error = russh::Error;

    fn check_server_key(
        &mut self,
        _server_public_key: &russh::keys::PublicKey,
    ) -> impl std::future::Future<Output = Result<bool, Self::Error>> + Send
    {
        let accept = self.accept_any_host_key;
        async move { Ok(accept) }
    }
}

type ClientHandle = russh::client::Handle<ClientHandler>;
type ClientChannel = Channel<russh::client::Msg>;
type ServerChannel = Channel<russh::server::Msg>;

enum ChannelHandle
{
    Client(Arc<Mutex<ClientChannel>>),
    Server(Arc<Mutex<ServerChannel>>),
}

#[derive(Clone)]
struct AuthPolicy
{
    username: Option<String>,
    password: Option<String>,
    allow_none: bool,
}

impl AuthPolicy
{
    fn allows_password(&self, user: &str, password: &str) -> bool
    {
        let user_ok = self.username.as_ref().map(|u| u == user).unwrap_or(true);
        let pass_ok = self
            .password
            .as_ref()
            .map(|p| p == password)
            .unwrap_or(true);
        user_ok && pass_ok
    }

    fn allows_none(&self, user: &str) -> bool
    {
        if !self.allow_none
        {
            return false;
        }
        self.username.as_ref().map(|u| u == user).unwrap_or(true)
    }
}

enum ServerEvent
{
    Error(String),
    Connected
    {
        conn_id: u64,
    },
    Disconnected
    {
        conn_id: u64,
    },
    AuthSucceeded
    {
        conn_id: u64,
    },
    ChannelOpenSession
    {
        conn_id: u64,
        channel_handle: u64,
    },
    ExecRequest
    {
        conn_id: u64,
        channel_handle: u64,
        command: Vec<u8>,
    },
    ShellRequest
    {
        conn_id: u64,
        channel_handle: u64,
    },
    Data
    {
        conn_id: u64,
        channel_handle: u64,
        data: Vec<u8>,
    },
    Eof
    {
        conn_id: u64,
        channel_handle: u64,
    },
    Close
    {
        conn_id: u64,
        channel_handle: u64,
    },
}

struct ServerShared
{
    auth_policy: AuthPolicy,
    next_conn_id: AtomicU64,
    conn_channels: Mutex<HashMap<u64, Vec<u64>>>,
    channel_lookup: Mutex<HashMap<(u64, u32), u64>>,
    event_tx: Sender<ServerEvent>,
}

impl ServerShared
{
    fn push_event(&self, event: ServerEvent)
    {
        let _ = self.event_tx.send(event);
    }

    fn register_channel(&self, conn_id: u64, channel: ServerChannel) -> u64
    {
        let channel_id: u32 = channel.id().into();
        let handle_id = {
            let mut state = state().lock().expect("russh state lock poisoned");
            let id = state.next_id();
            state
                .channels
                .insert(id, ChannelHandle::Server(Arc::new(Mutex::new(channel))));
            id
        };
        self.channel_lookup
            .lock()
            .expect("russh channel lookup mutex poisoned")
            .insert((conn_id, channel_id), handle_id);
        self.conn_channels
            .lock()
            .expect("russh conn channels mutex poisoned")
            .entry(conn_id)
            .or_default()
            .push(handle_id);
        handle_id
    }

    fn find_channel(&self, conn_id: u64, channel: ChannelId) -> Option<u64>
    {
        let channel_id: u32 = channel.into();
        self.channel_lookup
            .lock()
            .expect("russh channel lookup mutex poisoned")
            .get(&(conn_id, channel_id))
            .copied()
    }

    fn unregister_channel(&self, conn_id: u64, channel: ChannelId)
    {
        let channel_id: u32 = channel.into();
        let handle = self
            .channel_lookup
            .lock()
            .expect("russh channel lookup mutex poisoned")
            .remove(&(conn_id, channel_id));
        if let Some(handle_id) = handle
        {
            if let Ok(mut state) = state().lock()
            {
                state.channels.remove(&handle_id);
            }
            if let Some(items) = self
                .conn_channels
                .lock()
                .expect("russh conn channels mutex poisoned")
                .get_mut(&conn_id)
            {
                items.retain(|id| *id != handle_id);
            }
        }
    }

    fn unregister_connection(&self, conn_id: u64)
    {
        let handles = self
            .conn_channels
            .lock()
            .expect("russh conn channels mutex poisoned")
            .remove(&conn_id)
            .unwrap_or_default();
        if let Ok(mut state) = state().lock()
        {
            for handle_id in handles
            {
                state.channels.remove(&handle_id);
            }
        }
        self.channel_lookup
            .lock()
            .expect("russh channel lookup mutex poisoned")
            .retain(|(cid, _), _| *cid != conn_id);
    }
}

struct ServerHandler
{
    conn_id: u64,
    shared: Arc<ServerShared>,
}

impl ServerHandlerTrait for ServerHandler
{
    type Error = russh::Error;

    fn auth_none(
        &mut self,
        user: &str,
    ) -> impl std::future::Future<Output = Result<russh::server::Auth, Self::Error>> + Send
    {
        let ok = self.shared.auth_policy.allows_none(user);
        async move {
            if ok
            {
                Ok(russh::server::Auth::Accept)
            }
            else
            {
                Ok(russh::server::Auth::reject())
            }
        }
    }

    fn auth_password(
        &mut self,
        user: &str,
        password: &str,
    ) -> impl std::future::Future<Output = Result<russh::server::Auth, Self::Error>> + Send
    {
        let ok = self.shared.auth_policy.allows_password(user, password);
        async move {
            if ok
            {
                Ok(russh::server::Auth::Accept)
            }
            else
            {
                Ok(russh::server::Auth::reject())
            }
        }
    }

    fn auth_succeeded(
        &mut self,
        _session: &mut russh::server::Session,
    ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
    {
        self.shared.push_event(ServerEvent::AuthSucceeded {
            conn_id: self.conn_id,
        });
        async { Ok(()) }
    }

    fn channel_open_session(
        &mut self,
        channel: Channel<russh::server::Msg>,
        _session: &mut russh::server::Session,
    ) -> impl std::future::Future<Output = Result<bool, Self::Error>> + Send
    {
        let handle = self.shared.register_channel(self.conn_id, channel);
        self.shared.push_event(ServerEvent::ChannelOpenSession {
            conn_id: self.conn_id,
            channel_handle: handle,
        });
        async { Ok(true) }
    }

    fn shell_request(
        &mut self,
        channel: ChannelId,
        session: &mut russh::server::Session,
    ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
    {
        let conn_id = self.conn_id;
        let handle = self.shared.find_channel(conn_id, channel).unwrap_or(0);
        let _ = session.channel_success(channel);
        self.shared.push_event(ServerEvent::ShellRequest {
            conn_id,
            channel_handle: handle,
        });
        async { Ok(()) }
    }

    fn exec_request(
        &mut self,
        channel: ChannelId,
        data: &[u8],
        session: &mut russh::server::Session,
    ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
    {
        let conn_id = self.conn_id;
        let handle = self.shared.find_channel(conn_id, channel).unwrap_or(0);
        let command = data.to_vec();
        let _ = session.channel_success(channel);
        self.shared.push_event(ServerEvent::ExecRequest {
            conn_id,
            channel_handle: handle,
            command,
        });
        async { Ok(()) }
    }

    fn data(
        &mut self,
        channel: ChannelId,
        data: &[u8],
        _session: &mut russh::server::Session,
    ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
    {
        let conn_id = self.conn_id;
        if let Some(handle) = self.shared.find_channel(conn_id, channel)
        {
            self.shared.push_event(ServerEvent::Data {
                conn_id,
                channel_handle: handle,
                data: data.to_vec(),
            });
        }
        async { Ok(()) }
    }

    fn channel_eof(
        &mut self,
        channel: ChannelId,
        _session: &mut russh::server::Session,
    ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
    {
        let conn_id = self.conn_id;
        if let Some(handle) = self.shared.find_channel(conn_id, channel)
        {
            self.shared.push_event(ServerEvent::Eof {
                conn_id,
                channel_handle: handle,
            });
        }
        async { Ok(()) }
    }

    fn channel_close(
        &mut self,
        channel: ChannelId,
        _session: &mut russh::server::Session,
    ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
    {
        let conn_id = self.conn_id;
        if let Some(handle) = self.shared.find_channel(conn_id, channel)
        {
            self.shared.push_event(ServerEvent::Close {
                conn_id,
                channel_handle: handle,
            });
        }
        self.shared.unregister_channel(conn_id, channel);
        async { Ok(()) }
    }
}

struct ServerInstance
{
    stop_tx: watch::Sender<bool>,
    event_rx: Receiver<ServerEvent>,
}

struct RusshState
{
    next_id: u64,
    clients: HashMap<u64, Arc<Mutex<ClientHandle>>>,
    channels: HashMap<u64, ChannelHandle>,
    servers: HashMap<u64, Arc<Mutex<ServerInstance>>>,
}

impl RusshState
{
    fn next_id(&mut self) -> u64
    {
        let id = self.next_id;
        self.next_id = self.next_id.wrapping_add(1);
        id
    }
}

fn state() -> &'static Mutex<RusshState>
{
    static STATE: OnceLock<Mutex<RusshState>> = OnceLock::new();
    STATE.get_or_init(|| {
        Mutex::new(RusshState {
            next_id: 1,
            clients: HashMap::new(),
            channels: HashMap::new(),
            servers: HashMap::new(),
        })
    })
}

fn client_type() -> Result<Rc<StructType>, String>
{
    RUSSH_CLIENT_TYPE.with(|cell| {
        cell.borrow()
            .clone()
            .ok_or_else(|| "russh Client type not initialized".to_string())
    })
}

fn channel_type() -> Result<Rc<StructType>, String>
{
    RUSSH_CHANNEL_TYPE.with(|cell| {
        cell.borrow()
            .clone()
            .ok_or_else(|| "russh Channel type not initialized".to_string())
    })
}

fn server_type() -> Result<Rc<StructType>, String>
{
    RUSSH_SERVER_TYPE.with(|cell| {
        cell.borrow()
            .clone()
            .ok_or_else(|| "russh Server type not initialized".to_string())
    })
}

fn id_from_instance(value: &Value, name: &str) -> Result<u64, String>
{
    let inst = match value
    {
        Value::StructInstance(inst) => inst,
        _ => return Err(format!("{name} expects a receiver")),
    };
    let fields = inst.fields.borrow();
    match fields.get(0)
    {
        Some(Value::Integer { value, .. }) if *value >= 0 => Ok(*value as u64),
        Some(Value::Unsigned { value, .. }) => Ok(*value as u64),
        _ => Err(format!("{name} invalid receiver handle")),
    }
}

fn make_instance(ty: Rc<StructType>, id: u64) -> Value
{
    Value::StructInstance(Rc::new(StructInstance {
        ty,
        fields: RefCell::new(vec![int_value(id as i128)]),
    }))
}

fn channel_event_to_value(msg: ChannelMsg) -> Value
{
    match msg
    {
        ChannelMsg::Data { data } => map_value(vec![
            ("type", Value::String(intern::intern("data"))),
            ("data", Value::Bytes(Rc::new(data.to_vec()))),
        ]),
        ChannelMsg::ExtendedData { data, ext } => map_value(vec![
            ("type", Value::String(intern::intern("extended_data"))),
            ("ext", int_value(ext as i128)),
            ("data", Value::Bytes(Rc::new(data.to_vec()))),
        ]),
        ChannelMsg::Eof => map_value(vec![("type", Value::String(intern::intern("eof")))]),
        ChannelMsg::Close => map_value(vec![("type", Value::String(intern::intern("close")))]),
        ChannelMsg::ExitStatus { exit_status } => map_value(vec![
            ("type", Value::String(intern::intern("exit_status"))),
            ("code", int_value(exit_status as i128)),
        ]),
        ChannelMsg::ExitSignal {
            signal_name,
            core_dumped,
            error_message,
            lang_tag,
        } => map_value(vec![
            ("type", Value::String(intern::intern("exit_signal"))),
            ("signal", Value::String(intern::intern(&format!("{signal_name:?}")))),
            ("core_dumped", Value::Boolean(core_dumped)),
            ("error", Value::String(intern::intern(&error_message))),
            ("lang", Value::String(intern::intern(&lang_tag))),
        ]),
        ChannelMsg::Success => map_value(vec![("type", Value::String(intern::intern("success")))]),
        ChannelMsg::Failure => map_value(vec![("type", Value::String(intern::intern("failure")))]),
        ChannelMsg::WindowAdjusted { new_size } => map_value(vec![
            ("type", Value::String(intern::intern("window_adjusted"))),
            ("size", int_value(new_size as i128)),
        ]),
        ChannelMsg::Open {
            id,
            max_packet_size,
            window_size,
        } =>
        {
            let idv: u32 = id.into();
            map_value(vec![
                ("type", Value::String(intern::intern("open"))),
                ("channel_id", int_value(idv as i128)),
                ("max_packet_size", int_value(max_packet_size as i128)),
                ("window_size", int_value(window_size as i128)),
            ])
        }
        ChannelMsg::OpenFailure(reason) => map_value(vec![
            ("type", Value::String(intern::intern("open_failure"))),
            ("reason", Value::String(intern::intern(&format!("{reason:?}")))),
        ]),
        other => map_value(vec![
            ("type", Value::String(intern::intern("message"))),
            ("name", Value::String(intern::intern(&format!("{other:?}")))),
        ]),
    }
}

fn server_event_to_value(event: ServerEvent) -> Result<Value, String>
{
    let channel_ty = channel_type()?;
    let value = match event
    {
        ServerEvent::Error(message) => map_value(vec![
            ("type", Value::String(intern::intern("error"))),
            ("message", Value::String(intern::intern(&message))),
        ]),
        ServerEvent::Connected { conn_id } => map_value(vec![
            ("type", Value::String(intern::intern("connected"))),
            ("conn_id", int_value(conn_id as i128)),
        ]),
        ServerEvent::Disconnected { conn_id } => map_value(vec![
            ("type", Value::String(intern::intern("disconnected"))),
            ("conn_id", int_value(conn_id as i128)),
        ]),
        ServerEvent::AuthSucceeded { conn_id } => map_value(vec![
            ("type", Value::String(intern::intern("auth_succeeded"))),
            ("conn_id", int_value(conn_id as i128)),
        ]),
        ServerEvent::ChannelOpenSession {
            conn_id,
            channel_handle,
        } => map_value(vec![
            ("type", Value::String(intern::intern("channel_open_session"))),
            ("conn_id", int_value(conn_id as i128)),
            ("channel", make_instance(channel_ty, channel_handle)),
        ]),
        ServerEvent::ExecRequest {
            conn_id,
            channel_handle,
            command,
        } => map_value(vec![
            ("type", Value::String(intern::intern("exec_request"))),
            ("conn_id", int_value(conn_id as i128)),
            ("channel", make_instance(channel_ty.clone(), channel_handle)),
            ("command", Value::Bytes(Rc::new(command))),
        ]),
        ServerEvent::ShellRequest {
            conn_id,
            channel_handle,
        } => map_value(vec![
            ("type", Value::String(intern::intern("shell_request"))),
            ("conn_id", int_value(conn_id as i128)),
            ("channel", make_instance(channel_ty.clone(), channel_handle)),
        ]),
        ServerEvent::Data {
            conn_id,
            channel_handle,
            data,
        } => map_value(vec![
            ("type", Value::String(intern::intern("data"))),
            ("conn_id", int_value(conn_id as i128)),
            ("channel", make_instance(channel_ty.clone(), channel_handle)),
            ("data", Value::Bytes(Rc::new(data))),
        ]),
        ServerEvent::Eof {
            conn_id,
            channel_handle,
        } => map_value(vec![
            ("type", Value::String(intern::intern("eof"))),
            ("conn_id", int_value(conn_id as i128)),
            ("channel", make_instance(channel_ty.clone(), channel_handle)),
        ]),
        ServerEvent::Close {
            conn_id,
            channel_handle,
        } => map_value(vec![
            ("type", Value::String(intern::intern("close"))),
            ("conn_id", int_value(conn_id as i128)),
            ("channel", make_instance(channel_ty, channel_handle)),
        ]),
    };
    Ok(value)
}

fn russh_client_connect(args: &[Value]) -> Result<Value, String>
{
    let host = string_arg(args, 0, "russh.client_connect")?;
    let port = int_arg(args, 1, "russh.client_connect")?;
    let accept_any_host_key = bool_arg(args, 2, "russh.client_connect", true)?;
    let addr = format!("{host}:{}", port as u16);

    let rt = runtime()?;
    let config = Arc::new(russh::client::Config::default());
    let handle = rt
        .block_on(russh::client::connect(
            config,
            addr,
            ClientHandler {
                accept_any_host_key,
            },
        ))
        .map_err(|e| format!("russh.client_connect failed: {e}"))?;

    let id = {
        let mut state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        let id = state.next_id();
        state.clients.insert(id, Arc::new(Mutex::new(handle)));
        id
    };
    Ok(make_instance(client_type()?, id))
}

fn russh_server_start(args: &[Value]) -> Result<Value, String>
{
    let addr = string_arg(args, 0, "russh.server_start")?;
    let host_key_path = string_arg(args, 1, "russh.server_start")?;
    let username = match args.get(2)
    {
        Some(Value::Nil) | None => None,
        _ => Some(string_arg(args, 2, "russh.server_start")?),
    };
    let password = match args.get(3)
    {
        Some(Value::Nil) | None => None,
        _ => Some(string_arg(args, 3, "russh.server_start")?),
    };
    let allow_none = bool_arg(args, 4, "russh.server_start", false)?;

    let key = russh::keys::load_secret_key(Path::new(&host_key_path), None)
        .map_err(|e| format!("russh.server_start key load failed: {e}"))?;

    let (event_tx, event_rx) = mpsc::channel();
    let (stop_tx, stop_rx) = watch::channel(false);

    let shared = Arc::new(ServerShared {
        auth_policy: AuthPolicy {
            username,
            password,
            allow_none,
        },
        next_conn_id: AtomicU64::new(1),
        conn_channels: Mutex::new(HashMap::new()),
        channel_lookup: Mutex::new(HashMap::new()),
        event_tx,
    });

    let rt = runtime()?;
    let addr_clone = addr.clone();
    let shared_clone = shared.clone();
    let mut config = russh::server::Config::default();
    config.keys.push(key);
    let config = Arc::new(config);

    rt.spawn(async move {
        let listener = match tokio::net::TcpListener::bind(&addr_clone).await
        {
            Ok(v) => v,
            Err(e) =>
            {
                shared_clone.push_event(ServerEvent::Error(format!(
                    "russh.server_start bind failed: {e}"
                )));
                return;
            }
        };

        let mut stop_rx = stop_rx;
        loop
        {
            tokio::select! {
                _ = stop_rx.changed() => {
                    if *stop_rx.borrow() {
                        break;
                    }
                }
                incoming = listener.accept() => {
                    match incoming {
                        Ok((socket, _peer)) => {
                            let conn_id = shared_clone.next_conn_id.fetch_add(1, Ordering::Relaxed);
                            shared_clone.push_event(ServerEvent::Connected { conn_id });
                            let shared_session = shared_clone.clone();
                            let config_session = config.clone();
                            tokio::spawn(async move {
                                let handler = ServerHandler {
                                    conn_id,
                                    shared: shared_session.clone(),
                                };
                                match russh::server::run_stream(config_session, socket, handler).await {
                                    Ok(session) => {
                                        if let Err(e) = session.await {
                                            shared_session.push_event(ServerEvent::Error(format!(
                                                "server session error (conn {conn_id}): {e}"
                                            )));
                                        }
                                    }
                                    Err(e) => {
                                        shared_session.push_event(ServerEvent::Error(format!(
                                            "server accept error (conn {conn_id}): {e}"
                                        )));
                                    }
                                }
                                shared_session.unregister_connection(conn_id);
                                shared_session.push_event(ServerEvent::Disconnected { conn_id });
                            });
                        }
                        Err(e) => {
                            shared_clone.push_event(ServerEvent::Error(format!(
                                "russh.server_start accept failed: {e}"
                            )));
                        }
                    }
                }
            }
        }
    });

    let id = {
        let mut state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        let id = state.next_id();
        state
            .servers
            .insert(id, Arc::new(Mutex::new(ServerInstance { stop_tx, event_rx })));
        id
    };

    Ok(make_instance(server_type()?, id))
}

fn client_auth_none(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Client.auth_none expects a receiver".to_string())?,
        "Russh.Client.auth_none",
    )?;
    let user = string_arg(args, 1, "Russh.Client.auth_none")?;

    let client = {
        let state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        state
            .clients
            .get(&id)
            .cloned()
            .ok_or_else(|| "Russh.Client.auth_none invalid client".to_string())?
    };

    let rt = runtime()?;
    let mut guard = client
        .lock()
        .map_err(|_| "russh client lock poisoned".to_string())?;
    let ok = matches!(
        rt.block_on(guard.authenticate_none(user))
            .map_err(|e| format!("Russh.Client.auth_none failed: {e}"))?,
        AuthResult::Success
    );
    Ok(Value::Boolean(ok))
}

fn client_auth_password(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Client.auth_password expects a receiver".to_string())?,
        "Russh.Client.auth_password",
    )?;
    let user = string_arg(args, 1, "Russh.Client.auth_password")?;
    let password = string_arg(args, 2, "Russh.Client.auth_password")?;

    let client = {
        let state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        state
            .clients
            .get(&id)
            .cloned()
            .ok_or_else(|| "Russh.Client.auth_password invalid client".to_string())?
    };

    let rt = runtime()?;
    let mut guard = client
        .lock()
        .map_err(|_| "russh client lock poisoned".to_string())?;
    let ok = matches!(
        rt.block_on(guard.authenticate_password(user, password))
            .map_err(|e| format!("Russh.Client.auth_password failed: {e}"))?,
        AuthResult::Success
    );
    Ok(Value::Boolean(ok))
}

fn client_open_session(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Client.open_session expects a receiver".to_string())?,
        "Russh.Client.open_session",
    )?;

    let client = {
        let state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        state
            .clients
            .get(&id)
            .cloned()
            .ok_or_else(|| "Russh.Client.open_session invalid client".to_string())?
    };

    let rt = runtime()?;
    let guard = client
        .lock()
        .map_err(|_| "russh client lock poisoned".to_string())?;
    let channel = rt
        .block_on(guard.channel_open_session())
        .map_err(|e| format!("Russh.Client.open_session failed: {e}"))?;

    let channel_id = {
        let mut state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        let channel_id = state.next_id();
        state
            .channels
            .insert(channel_id, ChannelHandle::Client(Arc::new(Mutex::new(channel))));
        channel_id
    };
    Ok(make_instance(channel_type()?, channel_id))
}

fn client_disconnect(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Client.disconnect expects a receiver".to_string())?,
        "Russh.Client.disconnect",
    )?;
    let description = match args.get(1)
    {
        Some(Value::Nil) | None => "closed by client".to_string(),
        _ => string_arg(args, 1, "Russh.Client.disconnect")?,
    };

    let client = {
        let state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        state
            .clients
            .get(&id)
            .cloned()
            .ok_or_else(|| "Russh.Client.disconnect invalid client".to_string())?
    };

    let rt = runtime()?;
    let guard = client
        .lock()
        .map_err(|_| "russh client lock poisoned".to_string())?;
    rt.block_on(guard.disconnect(Disconnect::ByApplication, &description, ""))
        .map_err(|e| format!("Russh.Client.disconnect failed: {e}"))?;
    Ok(Value::Nil)
}

fn client_is_closed(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Client.is_closed expects a receiver".to_string())?,
        "Russh.Client.is_closed",
    )?;

    let client = {
        let state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        state
            .clients
            .get(&id)
            .cloned()
            .ok_or_else(|| "Russh.Client.is_closed invalid client".to_string())?
    };
    let guard = client
        .lock()
        .map_err(|_| "russh client lock poisoned".to_string())?;
    Ok(Value::Boolean(guard.is_closed()))
}

fn channel_exec(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Channel.exec expects a receiver".to_string())?,
        "Russh.Channel.exec",
    )?;
    let command = string_arg(args, 1, "Russh.Channel.exec")?;
    let want_reply = bool_arg(args, 2, "Russh.Channel.exec", true)?;

    let channel = {
        let state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        state
            .channels
            .get(&id)
            .ok_or_else(|| "Russh.Channel.exec invalid channel".to_string())?
            .as_client()
            .cloned()
            .ok_or_else(|| "Russh.Channel.exec only works for client channels".to_string())?
    };

    let rt = runtime()?;
    let guard = channel
        .lock()
        .map_err(|_| "russh channel lock poisoned".to_string())?;
    rt.block_on(guard.exec(want_reply, command.into_bytes()))
        .map_err(|e| format!("Russh.Channel.exec failed: {e}"))?;
    Ok(Value::Nil)
}

fn channel_write(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Channel.write expects a receiver".to_string())?,
        "Russh.Channel.write",
    )?;
    let data = bytes_arg(args, 1, "Russh.Channel.write")?;

    let channel = {
        let state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        state
            .channels
            .get(&id)
            .ok_or_else(|| "Russh.Channel.write invalid channel".to_string())?
            .as_any()
    };

    let rt = runtime()?;
    match channel
    {
        AnyChannel::Client(channel) =>
        {
            let guard = channel
                .lock()
                .map_err(|_| "russh channel lock poisoned".to_string())?;
            rt.block_on(async {
                let mut writer = guard.make_writer();
                writer.write_all(&data).await?;
                writer.flush().await
            })
            .map_err(|e: std::io::Error| format!("Russh.Channel.write failed: {e}"))?;
        }
        AnyChannel::Server(channel) =>
        {
            let guard = channel
                .lock()
                .map_err(|_| "russh channel lock poisoned".to_string())?;
            rt.block_on(async {
                let mut writer = guard.make_writer();
                writer.write_all(&data).await?;
                writer.flush().await
            })
            .map_err(|e: std::io::Error| format!("Russh.Channel.write failed: {e}"))?;
        }
    }

    Ok(int_value(data.len() as i128))
}

fn channel_eof(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Channel.eof expects a receiver".to_string())?,
        "Russh.Channel.eof",
    )?;
    let channel = {
        let state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        state
            .channels
            .get(&id)
            .ok_or_else(|| "Russh.Channel.eof invalid channel".to_string())?
            .as_any()
    };

    let rt = runtime()?;
    match channel
    {
        AnyChannel::Client(channel) =>
        {
            let guard = channel
                .lock()
                .map_err(|_| "russh channel lock poisoned".to_string())?;
            rt.block_on(guard.eof())
                .map_err(|e| format!("Russh.Channel.eof failed: {e}"))?;
        }
        AnyChannel::Server(channel) =>
        {
            let guard = channel
                .lock()
                .map_err(|_| "russh channel lock poisoned".to_string())?;
            rt.block_on(guard.eof())
                .map_err(|e| format!("Russh.Channel.eof failed: {e}"))?;
        }
    }
    Ok(Value::Nil)
}

fn channel_close(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Channel.close expects a receiver".to_string())?,
        "Russh.Channel.close",
    )?;
    let channel = {
        let state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        state
            .channels
            .get(&id)
            .ok_or_else(|| "Russh.Channel.close invalid channel".to_string())?
            .as_any()
    };

    let rt = runtime()?;
    match channel
    {
        AnyChannel::Client(channel) =>
        {
            let guard = channel
                .lock()
                .map_err(|_| "russh channel lock poisoned".to_string())?;
            rt.block_on(guard.close())
                .map_err(|e| format!("Russh.Channel.close failed: {e}"))?;
        }
        AnyChannel::Server(channel) =>
        {
            let guard = channel
                .lock()
                .map_err(|_| "russh channel lock poisoned".to_string())?;
            rt.block_on(guard.close())
                .map_err(|e| format!("Russh.Channel.close failed: {e}"))?;
        }
    }
    Ok(Value::Nil)
}

fn channel_wait(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Channel.wait expects a receiver".to_string())?,
        "Russh.Channel.wait",
    )?;
    let channel = {
        let state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        state
            .channels
            .get(&id)
            .ok_or_else(|| "Russh.Channel.wait invalid channel".to_string())?
            .as_client()
            .cloned()
            .ok_or_else(|| "Russh.Channel.wait only works for client channels".to_string())?
    };

    let rt = runtime()?;
    let mut guard = channel
        .lock()
        .map_err(|_| "russh channel lock poisoned".to_string())?;
    let msg = rt.block_on(guard.wait());
    Ok(match msg
    {
        Some(msg) => channel_event_to_value(msg),
        None => Value::Nil,
    })
}

fn channel_handle_id(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Channel.handle_id expects a receiver".to_string())?,
        "Russh.Channel.handle_id",
    )?;
    Ok(int_value(id as i128))
}

fn server_poll_event(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Server.poll_event expects a receiver".to_string())?,
        "Russh.Server.poll_event",
    )?;

    let timeout_ms = match args.get(1)
    {
        Some(Value::Nil) | None => 0,
        _ => int_arg(args, 1, "Russh.Server.poll_event")? as u64,
    };

    let server = {
        let state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        state
            .servers
            .get(&id)
            .cloned()
            .ok_or_else(|| "Russh.Server.poll_event invalid server".to_string())?
    };

    let guard = server
        .lock()
        .map_err(|_| "russh server lock poisoned".to_string())?;
    let event = if timeout_ms == 0
    {
        guard.event_rx.try_recv().ok()
    }
    else
    {
        match guard
            .event_rx
            .recv_timeout(Duration::from_millis(timeout_ms))
        {
            Ok(event) => Some(event),
            Err(RecvTimeoutError::Timeout) => None,
            Err(RecvTimeoutError::Disconnected) => None,
        }
    };

    match event
    {
        Some(ev) => server_event_to_value(ev),
        None => Ok(Value::Nil),
    }
}

fn server_stop(args: &[Value]) -> Result<Value, String>
{
    let id = id_from_instance(
        args.get(0)
            .ok_or_else(|| "Russh.Server.stop expects a receiver".to_string())?,
        "Russh.Server.stop",
    )?;

    let server = {
        let state = state()
            .lock()
            .map_err(|_| "russh state lock poisoned".to_string())?;
        state
            .servers
            .get(&id)
            .cloned()
            .ok_or_else(|| "Russh.Server.stop invalid server".to_string())?
    };

    let guard = server
        .lock()
        .map_err(|_| "russh server lock poisoned".to_string())?;
    let _ = guard.stop_tx.send(true);
    Ok(Value::Nil)
}

enum AnyChannel
{
    Client(Arc<Mutex<ClientChannel>>),
    Server(Arc<Mutex<ServerChannel>>),
}

trait ChannelHandleExt
{
    fn as_client(&self) -> Option<&Arc<Mutex<ClientChannel>>>;
    fn as_any(&self) -> AnyChannel;
}

impl ChannelHandleExt for ChannelHandle
{
    fn as_client(&self) -> Option<&Arc<Mutex<ClientChannel>>>
    {
        match self
        {
            ChannelHandle::Client(ch) => Some(ch),
            ChannelHandle::Server(_) => None,
        }
    }

    fn as_any(&self) -> AnyChannel
    {
        match self
        {
            ChannelHandle::Client(ch) => AnyChannel::Client(ch.clone()),
            ChannelHandle::Server(ch) => AnyChannel::Server(ch.clone()),
        }
    }
}

pub fn build_russh_module() -> Value
{
    let mut client_methods = FxHashMap::default();
    client_methods.insert(intern::intern("auth_none"), Value::NativeFunction(client_auth_none));
    client_methods
        .insert(intern::intern("auth_password"), Value::NativeFunction(client_auth_password));
    client_methods
        .insert(intern::intern("open_session"), Value::NativeFunction(client_open_session));
    client_methods.insert(intern::intern("disconnect"), Value::NativeFunction(client_disconnect));
    client_methods.insert(intern::intern("is_closed"), Value::NativeFunction(client_is_closed));

    let client_ty = Rc::new(StructType {
        name: intern::intern("RusshClient"),
        fields: vec![StructField {
            name: intern::intern("_handle"),
            type_ref: any_type_ref(),
        }],
        field_map: {
            let mut map = FxHashMap::default();
            map.insert(intern::intern("_handle"), 0);
            map
        },
        methods: RefCell::new(client_methods),
    });

    let mut channel_methods = FxHashMap::default();
    channel_methods.insert(intern::intern("exec"), Value::NativeFunction(channel_exec));
    channel_methods.insert(intern::intern("write"), Value::NativeFunction(channel_write));
    channel_methods.insert(intern::intern("eof"), Value::NativeFunction(channel_eof));
    channel_methods.insert(intern::intern("close"), Value::NativeFunction(channel_close));
    channel_methods.insert(intern::intern("wait"), Value::NativeFunction(channel_wait));
    channel_methods.insert(intern::intern("handle_id"), Value::NativeFunction(channel_handle_id));

    let channel_ty = Rc::new(StructType {
        name: intern::intern("RusshChannel"),
        fields: vec![StructField {
            name: intern::intern("_handle"),
            type_ref: any_type_ref(),
        }],
        field_map: {
            let mut map = FxHashMap::default();
            map.insert(intern::intern("_handle"), 0);
            map
        },
        methods: RefCell::new(channel_methods),
    });

    let mut server_methods = FxHashMap::default();
    server_methods.insert(intern::intern("poll_event"), Value::NativeFunction(server_poll_event));
    server_methods.insert(intern::intern("stop"), Value::NativeFunction(server_stop));

    let server_ty = Rc::new(StructType {
        name: intern::intern("RusshServer"),
        fields: vec![StructField {
            name: intern::intern("_handle"),
            type_ref: any_type_ref(),
        }],
        field_map: {
            let mut map = FxHashMap::default();
            map.insert(intern::intern("_handle"), 0);
            map
        },
        methods: RefCell::new(server_methods),
    });

    RUSSH_CLIENT_TYPE.with(|cell| {
        *cell.borrow_mut() = Some(client_ty.clone());
    });
    RUSSH_CHANNEL_TYPE.with(|cell| {
        *cell.borrow_mut() = Some(channel_ty.clone());
    });
    RUSSH_SERVER_TYPE.with(|cell| {
        *cell.borrow_mut() = Some(server_ty.clone());
    });

    let mut map = FxHashMap::default();
    map.insert(intern::intern("Client"), Value::StructType(client_ty));
    map.insert(intern::intern("Channel"), Value::StructType(channel_ty));
    map.insert(intern::intern("Server"), Value::StructType(server_ty));
    map.insert(intern::intern("client_connect"), Value::NativeFunction(russh_client_connect));
    map.insert(intern::intern("server_start"), Value::NativeFunction(russh_server_start));

    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}

pub fn register(map: &mut LibMap)
{
    let module = build_russh_module();
    map.insert(intern::intern("russh"), module.clone());
    map.insert(intern::intern("Russh"), module);
}
