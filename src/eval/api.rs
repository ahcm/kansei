//! High-level entry points for Rust hosts.
use super::*;
use crate::parser::{ParseError, parse_source};

/// An error encountered while parsing or executing source.
#[derive(Debug, Clone)]
pub enum Error
{
    Parse(ParseError),
    Runtime(RuntimeError),
}

impl std::fmt::Display for RuntimeError
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        write!(f, "{} (line {}, column {})", self.message, self.line, self.column)
    }
}

impl std::error::Error for RuntimeError {}

impl std::fmt::Display for Error
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        match self
        {
            Self::Parse(error) => error.fmt(f),
            Self::Runtime(error) => error.fmt(f),
        }
    }
}

impl std::error::Error for Error
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)>
    {
        match self
        {
            Self::Parse(error) => Some(error),
            Self::Runtime(error) => Some(error),
        }
    }
}

/// Host-supplied script metadata. Arguments exclude the executable name.
///
/// The environment is empty by default; the host chooses what to expose.
#[derive(Debug, Clone, Default)]
pub struct Program
{
    pub name: String,
    pub args: Vec<String>,
    pub env: std::collections::HashMap<String, String>,
}

impl Default for Interpreter
{
    fn default() -> Self
    {
        Self::new()
    }
}

impl Interpreter
{
    /// Parse, resolve, and execute source, retaining globals for later calls.
    ///
    /// Returns the last expression's value. Parse and runtime failures retain
    /// their structured details. Execution is not transactional: assignments
    /// completed before a runtime failure remain visible.
    pub fn eval_source(&mut self, source: &str) -> Result<Value, Error>
    {
        let mut ast = parse_source(source).map_err(Error::Parse)?;
        resolve_slots(&mut ast);
        self.eval(&ast, &mut []).map_err(Error::Runtime)
    }

    /// Define or replace a global without managing interned symbol IDs.
    pub fn set_global(&mut self, name: &str, value: Value)
    {
        self.define_global(intern::intern_symbol(name), value);
    }

    /// Look up a global by its script-visible name.
    pub fn get_global(&self, name: &str) -> Option<Value>
    {
        self.get_global_value(intern::intern_symbol(name))
    }

    /// Register a Rust function callable from scripts.
    pub fn register_native(&mut self, name: &str, function: crate::value::NativeFunction)
    {
        self.set_global(name, Value::NativeFunction(function));
    }

    /// Call a global function, preserving structured runtime errors.
    pub fn call(&mut self, name: &str, args: &[Value]) -> EvalResult
    {
        let function = self.get_global(name).ok_or_else(|| {
            RuntimeError::simple(format!("Unknown global '{name}'"), 0)
        })?;
        self.call_value(function, args.iter().cloned().collect(), 0, None)
    }

    /// Replace `program` with host-supplied metadata and available WASM backends.
    ///
    /// Does not install `program.exit`; process termination belongs to the CLI.
    pub fn set_program(&mut self, program: Program)
    {
        let string = |s: String| Value::String(intern::intern_owned(s));
        let array = |items: Vec<String>| {
            Value::Array(Rc::new(RefCell::new(items.into_iter().map(string).collect())))
        };
        let env = program.env.into_iter()
            .map(|(key, value)| (intern::intern_owned(key), string(value)))
            .collect();
        let fields = FxHashMap::from_iter([
            (intern::intern("name"), string(program.name)),
            (intern::intern("args"), array(program.args)),
            (intern::intern("env"), Value::Map(Rc::new(RefCell::new(MapValue::new(env))))),
            (intern::intern("wasm_backend"), string("wasmtime".to_string())),
            (intern::intern("wasm_backends"), array(crate::wasm::available_wasm_backends()
                .into_iter().map(str::to_string).collect())),
        ]);
        self.set_global("program", Value::Map(Rc::new(RefCell::new(MapValue::new(fields)))));
    }

    /// Redirect `puts` and `print`. The default is the process's stdout.
    pub fn set_stdout(&mut self, writer: impl Write + 'static)
    {
        self.stdout = Box::new(writer);
    }

    /// Redirect `eputs` and `eprint`. Logging has its own configuration.
    pub fn set_stderr(&mut self, writer: impl Write + 'static)
    {
        self.stderr = Box::new(writer);
    }
}
