use std::parallel
parallel = std::parallel

ctx = {"a": 10, "b": 20}

# Verify that 'a' and 'b' from context map are accessible directly in the closure
results = parallel.loop(5, {|i, ctx|
  i + a + b
}, ctx)

puts results
