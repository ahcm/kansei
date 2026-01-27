use std::wasm
wasm = std::wasm

spec = {
  "functions": [
    { "name": "sum", "params": ["i32", "i32"], "result": "i32" },
    { "name": "mean", "params": ["f64[]"], "result": "f64" }
  ]
}

assert(wasm.validate(spec, "sum", 1, 2))
assert(!wasm.validate(spec, "sum", "x", 2))
assert(wasm.validate(spec, "mean", [1.0, 2.0, 3.0]))
