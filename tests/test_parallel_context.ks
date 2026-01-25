use std::parallel
parallel = std::parallel

ctx = {"a": 10, "b": 20}

# Verify single argument closure: |i|
# Context 'a' and 'b' are injected into environment automatically.
results1 = parallel.loop(5, ctx, {|i|
  i + a + b
})
puts results1

# Verify two argument closure: |ctx, i|
# Context object is passed explicitly as first argument.
results2 = parallel.loop(5, ctx, {|ctx, i|
  i + ctx.a
})
puts results2
