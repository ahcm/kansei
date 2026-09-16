load wasm::plotters
use std::IO
IO = std::IO

data = [
  0.0, 1.0,
  1.0, 2.0,
  2.0, 1.5,
  0.0, 0.5,
  1.0, 1.5,
  2.0, 0.5
]
series = [3, 3]
colors = [2001125, 14162784]

svg = wasm.plotters.line_chart_svg_str_multi(
  data,
  series,
  colors,
  640,
  360,
  0,
  0,
  0,
  0,
  "Plotters Demo",
  "x",
  "y"
)

IO.write("plot.svg", svg)
