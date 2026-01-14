load wasm::plotters
use std::IO
use std::lib::clap
use std::Int64

IO = std::IO
clap = std::lib::clap
Int64 = std::Int64

args = program.args
flags = ["--svg"]
options = ["--x", "--y"]
parsed = clap.parse(args, flags, options)

if len(parsed.args) == 0
  puts "usage: plotxy.ks [--svg] [--x N] [--y N] file.tsv"
  puts "defaults: --x 0 --y 1"
  return nil
end

x_col = 0
y_col = 1
if parsed.x != nil
  x_col = Int64.parse(parsed.x)
end
if parsed.y != nil
  y_col = Int64.parse(parsed.y)
end

path = parsed.args[0]
tsv = IO.read(path)
if tsv == nil
  puts "failed to read file"
  return nil
end

if parsed.svg
  svg = wasm.plotters.scatter_tsv_svg_str(tsv, x_col, y_col, 800, 600)
  IO.write("plot.svg", svg)
  puts "wrote plot.svg"
else
  png = wasm.plotters.scatter_tsv_png_str(tsv, x_col, y_col, 800, 600)
  IO.write("plot.png.b64", png)
  `base64 -d plot.png.b64 > plot.png`
  puts "wrote plot.png"
end
