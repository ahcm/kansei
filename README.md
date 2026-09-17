# Kansei (感性)

Kansei is a scripting language implemented in Rust, in the spirit of Ruby 1.8.
It supports currying, mutable collections, structural parameters, parallel work,
SIMD operations, and WASM modules, with a functional flavour.

The language specification in [LANGUAGE.md](LANGUAGE.md) is also periodically
synced to [kansei-language](https://github.com/ahcm/kansei-language/blob/main/LANGUAGE.md).

## Build and run

Install Rust through rustup and a native C/C++ build toolchain. The repository's
`rust-toolchain.toml` selects the tested nightly compiler required by portable
SIMD; `Cargo.lock` pins dependencies. The current CI platform is Linux.

```sh
cargo build --locked --release
./target/release/kansei -e 'puts "Hello, world!"'
./target/release/kansei tests/regression/output.ks
```

Run `./target/release/kansei` without a script to enter the REPL. A syntax error
reports its location and leaves the REPL available for another input.
`ks` is an equivalent binary with its own entry point; `cargo run` defaults to
`kansei`.

```ruby
fn add(x, y)
  x + y
end

add10 = add(10)
puts add10(5) # 15
```

## Embedding in Rust

The package also exposes a `kansei` library. From another project, add a local
checkout as a dependency (adjust the path):

```toml
[dependencies]
kansei = { path = "../kansei", default-features = false }
```

Use the nightly compiler specified in this repository's `rust-toolchain.toml`;
a dependency's toolchain file does not select the compiler for its consumer.
Enable optional standard library features as needed.

```rust
use kansei::{Interpreter, Program};

fn main() -> Result<(), kansei::Error> {
    let mut interpreter = Interpreter::new();
    interpreter.set_program(Program {
        name: "my-app".into(),
        args: vec!["hello".into()],
        ..Program::default()
    });
    interpreter.register_native("identity", |args| {
        args.first().cloned().ok_or_else(|| "expected an argument".into())
    });
    let result = interpreter.eval_source("identity(6 * 7)")?;
    assert_eq!(result.to_string(), "42");
    Ok(())
}
```

`eval_source` parses and resolves source, returns its last value, and retains
globals between evaluations. `Error::Parse` and `Error::Runtime` preserve error
locations; runtime errors also carry trace frames. Evaluation is not
transactional: changes made before a runtime error remain. Use `set_global`,
`get_global`, and `call` to exchange values and call script functions from Rust.
`Value::HostFunction` additionally lets a registered function access the interpreter.

Call `set_program` to supply `program.name`, `program.args`, and `program.env`.
The library does not read process arguments or populate that metadata implicitly.
`Program::default()` has no arguments or environment entries. The CLI supplies
its process environment and adds `program.exit`; the embedding API does not
install that process-exit function. Set `set_main_path` when relative module
imports should be resolved from a script's location.

`set_stdout` and `set_stderr` accept owned `std::io::Write + 'static` writers for
`puts`/`print` and `eputs`/`eprint`. Output failures become runtime errors. For
in-memory capture, a writer wrapping `Rc<RefCell<Vec<u8>>>` lets the host retain
access to the buffer; see [the embedding tests](tests/embedding.rs). Logging has
its existing independent file/stderr configuration, and these writers do not
redirect subprocess or native-extension output.

Interpreters and values use `Rc`/`RefCell` and are neither `Send` nor `Sync`.
Create an independent interpreter inside each host thread and keep its values
and interned symbol IDs on that thread. AST/runtime representations in the
public low-level modules may change; prefer the root exports for embedding.
CLI dependencies and Wasmtime remain part of the core build for now.

Run the [embedding example](examples/embed.rs) with
`cargo run --locked --no-default-features --example embed`, or generate API
documentation with `cargo doc --locked --no-deps --lib`. The `kansei` and `ks`
binaries both call `kansei::cli::run()`; Rust hosts use `Interpreter` directly.

## Build features

The default `std-lib-all` feature includes the optional standard library modules,
including GUI, terminal UI, dataframes, image handling, SQLite, and SSH (`lib-russh`). For a smaller
build, disable defaults and select the modules you need:

```sh
cargo build --locked --release --no-default-features
cargo build --locked --release --no-default-features --features lib-bytes,lib-serde,lib-sqlite
```

Feature names are listed in [Cargo.toml](Cargo.toml). Disabling a module also
removes its optional dependencies and associated runtime value variants.
`lib-yaml` and `lib-toml` enable their shared serialization helpers automatically.
Core facilities still include Wasmtime, parallel execution, CLI/editor support,
JSON for the LSP, and TOML for package manifests. `wasmi` adds a second WASM backend.

Release builds use a portable CPU target. For benchmarks on the build machine:

```sh
RUSTFLAGS="-C target-cpu=native" cargo build --locked --release
```

Do not distribute that native CPU build as a portable binary.

## Execution and tools

`--bytecode off` uses AST evaluation. `simple` (the default) and `advanced` enable
compilation for eligible functions, with AST fallback; their current eligibility
rule is the same. Use `--dump-bytecode` to inspect what actually compiles.

- `kansei fmt <path>` formats indentation while preserving comments and literal contents.
- `kansei fmt --stdin` reads source from stdin and writes formatted source to stdout.
- `kansei check <path>` checks complete `.ks` files and exits nonzero on syntax errors.
- `kansei test <path>` runs tests in all three execution modes, with a timeout per run.
- `kansei install [path]` installs local modules or dependencies from `kansei.toml`.
- `kansei wasm install <name>` builds and installs a WASM module.
- `kansei lsp` starts the language server over stdio, with syntax diagnostics, hover, and definitions.

WAT generation supports two executable targets: `wasip1` emits a core module
with WASI Preview 1 imports; `wasip2` emits a component with WASI 0.2 interfaces
and a `wasi:cli/run` entry point. Both share the same runtime for output and
command-line arguments. For example, with Wasmtime installed:

```sh
kansei --dump-wat --wasi wasip2 example.ks > example.wat
wasmtime run example.wat first-argument
```

WASIp2 uses the [Bytecode Alliance command adapter](https://docs.rs/wasi-preview1-component-adapter-provider/41.0.4/)
bundled at build time; generation requires no external tools or downloads.
The compiler still supports a subset of the language; unsupported constructs
produce an error rather than a partially generated program.

WASM installation uses `../kansei-wasm-modules` when present, otherwise
`https://github.com/ahcm/kansei-wasm-modules`, and targets `wasm32-wasip1`.
Override either setting with:

```sh
kansei wasm install <name> --wasm-modules-repo <path-or-url> --wasm-target <target>
```

Logging defaults to stderr. Use `-l/--log <path>` or `std::log` to configure it.
See [INTERPRETER.md](INTERPRETER.md) for CLI details and editor configuration,
and [LANGUAGE.md](LANGUAGE.md) for language syntax and standard library APIs.

## Tests and development

```sh
cargo test --locked --no-default-features
cargo build --locked --no-default-features
python3 scripts/test-regressions.py target/debug/kansei
```

The Python harness uses only the standard library. It runs the deterministic
language suite in every execution mode and checks CLI errors, LSP recovery,
formatter preservation, and test-runner failures. CI also tests default-feature
and release builds and checks individual optional modules.

`tests/regression/` is the self-contained regression suite. Other files under
`tests/` include examples, benchmarks, and integrations that can require WASM
artifacts, arguments, or filesystem setup; they are not all standalone tests.
See [tests/README.md](tests/README.md) for test conventions and
[ARCHITECTURE.md](ARCHITECTURE.md) for the interpreter's module boundaries.
Use the [performance baseline runner](benchmarks/README.md) to compare parsing,
formatting, execution, and cache costs across changes on the same machine.

-- Andreas Hauser, München
