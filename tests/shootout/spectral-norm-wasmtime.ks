program.wasm_backend = "wasmtime"
load wasm::spectral_norm

n = program.args[0]
use std::Int64
Int64 = std::Int64
n = Int64::parse(n)

res_wasm = wasm.spectral_norm.spectral_norm(n)

use std::Float64
Float64 = std::Float64
puts f"{res_wasm:.9}"
