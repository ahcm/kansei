use std::parallel
parallel = std::parallel

# Test classic parallel.loop (n, function)
results = parallel.loop(4, {|i| i * 2})
# Order: [0, 2, 4, 6] (order not guaranteed but values should match)
puts results

# Test new order parallel.loop(n, context, function) -> closure(ctx, i)
ctx = {"val": 10}
results = parallel.loop(4, ctx, {|ctx, i| i + val})
puts results