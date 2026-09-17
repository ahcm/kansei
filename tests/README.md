# Tests

Run the deterministic suite with a built binary:

```sh
kansei test tests/regression
kansei test --bytecode off --timeout 10 tests/regression
```

The runner sorts and deduplicates `.ks` files and defaults to `off`, `simple`, and
`advanced` execution modes. Each run has a 30-second timeout and a 1 MiB limit
per output stream. Use `--bytecode` and `--timeout` to override these defaults.
The runner reports each result and exits nonzero if any test fails.

For `example.ks`, optional sidecars are:

| File | Meaning |
| --- | --- |
| `example.out` | Exact expected stdout, including newlines |
| `example.err` | Exact expected stderr, including newlines |
| `example.status` | Expected integer exit status; defaults to 0 |

A missing output sidecar means that stream is not compared. An empty sidecar
requires empty output. Use `assert`/`assert_eq` to test computed values; printing
a result without a snapshot only tests successful execution. Compare collection
elements when testing a generic array against a specialized numeric array, since
those are distinct runtime types.

The regression fixtures cover arithmetic, control flow, recursion, currying,
collections, structs, result handlers, parallel work, expected failures, output,
and rejection of trailing source. Add small deterministic fixtures here when
fixing language behavior. Negative syntax fixtures intentionally fail `kansei check`.

`python3 scripts/test-regressions.py target/debug/kansei` also exercises the LSP,
formatter, and runner itself. Rust unit tests cover parser errors, formatting
preservation, and module installation. Repeat with a release binary to catch
build-profile differences.

The same harness runs against `ks` in CI, including script arguments and
`program.exit`. `cargo test --test embedding` checks the library from an external
crate: persistent globals, host functions, structured errors, program metadata,
and output capture/failures. `cargo test --doc` checks the public API example.

The remaining `.ks` files are historical examples and integration programs.
The CLI harness also verifies reviewed snapshots for `test_logic.ks`,
`test_casts.ks`, and reference capture in `test_currying.ks`.
WAT unit tests execute both WASI targets with Wasmtime, checking stdout, stderr,
Unicode arguments, empty argument lists, and memory growth with large literals.
`shootout/` contains benchmarks; WASM, image, and SQLite examples may require
additional setup. Run those individually with their required features and inputs.
