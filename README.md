# About
Kansei (感性) is a language based on Rust in the spirit of Ruby 1.8.

Not supporting Rails is seen as a benefit.

A number of snippets can be found in files in the tests/ directory.

It supports currying:
```
fn add(x, y)
  x + y
end

add10 = add(10)
puts "Add 10 to 5: " + add10(5)
```

Logging defaults to stderr via `log(...)` and can be redirected with `-l/--log <path>`.
Use `std::log` to change log target/format at runtime.

CLI helpers:
- `kansei fmt <path>` formats `.ks` files in place
- `kansei check <path>` parses `.ks` files and exits non-zero on errors
- `kansei test <path>` runs `.ks` files and compares against `.out`/`.err` if present
- `kansei install [path]` installs dependencies from `kansei.toml` or local module paths
- `kansei wasm install <name>` builds and installs a wasm module from the wasm-modules repo
- `kansei lsp` starts a minimal Language Server over stdio (diagnostics only)

WASM install defaults:
- Repo: `../kansei-wasm-modules` if present, otherwise `https://github.com/ahcm/kansei-wasm-modules`
- Target: `wasm32-wasip1` (override with `--wasm-target`)

Custom repo/target:
```
kansei wasm install <name> --wasm-modules-repo <path-or-url> --wasm-target <target>
```
---
Have fun and may the force be with you!

-- Andreas Hauser, München, 8.1.2026
