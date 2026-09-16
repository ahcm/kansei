# Interpreter architecture

Source flows through `lexer` and `parser` into `ast::Expr`, then through slot
resolution and either AST evaluation or bytecode compilation/execution.
Lexical and syntax failures return `parser::ParseError` with one-based line and
column positions. CLI and LSP clients decide how to present those errors.

| Module | Responsibility |
| --- | --- |
| `src/eval.rs` | Interpreter state, environments, module loading, host calls, and shared runtime errors |
| `src/eval/numeric.rs` | Shared arithmetic and numeric conversions |
| `src/eval/compiler.rs` | Slot resolution, compilation, optimization eligibility, and bytecode diagnostics |
| `src/eval/wat.rs` | WAT generation and its emitted runtime |
| `src/eval/vm.rs` | Bytecode/register execution and cached runtime lookups |
| `src/eval/builtins.rs` | Shared builtin dispatch and native numeric module registration |
| `src/eval/ast_eval.rs` | Tree evaluation and host AST evaluation |
| `src/value.rs` | Runtime values, instructions, and cache representations |
| `src/std/` | Native standard library modules |

These are internal module boundaries, not independent public libraries. The
backends share the same runtime values and builtin implementation. Helpers are
visible within `eval`; keep changes to cache representations explicit in
`value.rs` and verify behavior across all execution modes.

## Formatting is separate from execution

`formatter::format_source` validates source, then creates temporary line-layout
metadata and tracks multiline literals/comments. It changes indentation without
regenerating program tokens. That metadata is allocated only for `fmt`; it is
never attached to `Expr`, retained in module/function caches, or created during
normal execution. `source::expr_to_source` remains the AST-to-source printer for
programmatically generated code, where original comments are unavailable.

## Regression strategy

Use `cargo test` for parser, formatter, and installer invariants. Use
`tests/regression/` for shared language behavior, running the same fixtures in
all execution modes. The CLI harness verifies release-sensitive behavior such
as syntax-error recovery in a persistent LSP process. Preserve this coverage
when adding compiler optimizations or splitting the interpreter further.
