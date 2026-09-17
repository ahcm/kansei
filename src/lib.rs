#![feature(portable_simd)]
//! Embed the Kansei interpreter in a Rust application.
//!
//! ```
//! use kansei::Interpreter;
//!
//! let mut interpreter = Interpreter::new();
//! let value = interpreter.eval_source("answer = 6 * 7\nanswer")?;
//! assert_eq!(value.to_string(), "42");
//! # Ok::<(), kansei::Error>(())
//! ```
//!
//! Interpreters and values use `Rc`/`RefCell` and must stay on their creating
//! thread. Create independent interpreters inside worker threads; do not move
//! values or interned symbol IDs between threads. Builds require the nightly
//! toolchain selected by this repository for portable SIMD.
//!
//! [`Interpreter::eval_source`] returns structured errors and retains globals.
//! [`Program`] supplies script arguments and environment explicitly. Process
//! argument parsing and `program.exit` are supplied only by [`cli`]. Language
//! output can be redirected with [`Interpreter::set_stdout`] and
//! [`Interpreter::set_stderr`]; subprocess I/O and logging are separate.
//!
//! The root exports are the embedding API. Public modules also expose lower-level
//! AST, runtime, and WASM facilities; those representations may evolve.

pub mod ast;
pub mod eval;
pub mod intern;
#[path = "std/mod.rs"]
mod kansei_std;
mod lexer;
mod lsp;
pub mod parser;
mod pm;
mod sexpr;
mod source;
mod formatter;
mod source_files;
mod test_runner;
pub mod value;
pub mod wasm;
mod wasm_pm;

pub mod cli;

pub use eval::{BytecodeMode, Error, Interpreter, Program, RuntimeError};
pub use parser::ParseError;
pub use value::{HostFunction, NativeFunction, Value};
