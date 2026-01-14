# plotters_wasm

WASM wrapper for Plotters that renders line charts to SVG.

## Exports
- `alloc(size: usize) -> *mut u8`
- `dealloc(ptr: *mut u8, size: usize)`
- `line_chart_svg_str(data_ptr: *const f64, data_len: usize, width: u32, height: u32) -> i64`

`line_chart_svg_str` expects `data_len` to be an even count of `f64` values representing `(x, y)` pairs.
It returns an `i64` where the low 32 bits are the pointer and the high 32 bits are the length.
The function name ends in `_str` so the host treats the returned data as a UTF-8 string.

## Build
```
wasm32-unknown-unknown target required:
  rustup target add wasm32-unknown-unknown

build:
  cargo build --release --target wasm32-unknown-unknown
```

The output module will be at:
`target/wasm32-unknown-unknown/release/plotters_wasm.wasm`
