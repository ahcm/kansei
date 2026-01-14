# plotters_wasm

WASM wrapper for Plotters that renders line charts to SVG.

## Exports
- `alloc(size: usize) -> *mut u8`
- `dealloc(ptr: *mut u8, size: usize)`
- `line_chart_svg_str(data_ptr: *const f64, data_len: usize, width: u32, height: u32) -> i64`
- `line_chart_svg_str_multi(data_ptr, data_len, series_ptr, series_len, colors_ptr, colors_len, width, height, min_x, max_x, min_y, max_y, title_ptr, title_len, x_ptr, x_len, y_ptr, y_len) -> i64`
- `line_chart_png_str_multi(...) -> i64` (base64 PNG)
- `scatter_chart_svg_str_multi(...) -> i64`
- `scatter_chart_png_str_multi(...) -> i64` (base64 PNG)
- `bar_chart_svg_str_multi(...) -> i64`
- `bar_chart_png_str_multi(...) -> i64` (base64 PNG)

The simple `line_chart_svg_str` expects `data_len` to be an even count of `f64` values representing `(x, y)` pairs.
All `*_str` functions return an `i64` where the low 32 bits are the pointer and the high 32 bits are the length.
PNG variants return a base64-encoded PNG string.

### Multi-series format
`data_ptr` points to a flat `f64` array containing all series back-to-back: `[x0,y0, x1,y1, ...]`.
`series_ptr` points to `u32` counts for each series (number of points per series). The total must match `data_len / 2`.
`colors_ptr` points to `u32` colors in `0xRRGGBB` format. If `colors_len` is 0, defaults are used.
Axis range is auto when `min_x >= max_x` or `min_y >= max_y`.
Labels are UTF-8 strings passed as `(ptr, len)` pairs; use `len = 0` for none.

## Build
```
wasm32-unknown-unknown target required:
  rustup target add wasm32-unknown-unknown

build:
  cargo build --release --target wasm32-unknown-unknown
```

The output module will be at:
`target/wasm32-unknown-unknown/release/plotters_wasm.wasm`
