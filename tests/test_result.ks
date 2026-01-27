x = result { f64("asas") } else 0.0


x = result { f64("asas") } else |e| {
  puts f"{e.message}"
  puts f"Line {e.line}:{e.column}: {puts e.source}"
  0.0
}
