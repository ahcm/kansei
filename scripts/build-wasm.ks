use std::IO
use std::File
use std::lib::Path

IO = std::IO
File = std::File
Path = std::lib::Path

root = IO.cwd()

cmd = "cargo build --release --target wasm32-unknown-unknown -p plotters_wasm -p spectral_norm_wasm"
puts cmd
`{cmd}`

base = Path.join(root, "target/wasm32-unknown-unknown/release")
plotters = Path.join(base, "plotters_wasm.wasm")
spectral = Path.join(base, "spectral_norm_wasm.wasm")

if not File.exists(plotters)
  puts "Missing wasm output: " + plotters
  program.exit(1)
end

if not File.exists(spectral)
  puts "Missing wasm output: " + spectral
  exit(1)
end

fn copy_wasm(src, dst)
  dir = Path.dirname(dst)
  if dir != "" && not File.exists(dir)
    File.mkdirs(dir)
  end
  if not File.copy(src, dst)
    puts "Failed to copy " + src + " -> " + dst
    program.exit(1)
  end
end

copy_wasm(plotters, Path.join(root, "wasm/plotters.wasm"))
copy_wasm(spectral, Path.join(root, "wasm/spectral_norm.wasm"))

copy_wasm(spectral, Path.join(root, "tests/shootout/wasm/spectral_norm.wasm"))

puts "Wasm modules copied to wasm/ and tests/*/wasm/"
