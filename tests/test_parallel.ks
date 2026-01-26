use std::parallel
parallel = std::parallel

# Test classic parallel.loop (n, function)
results = parallel.loop(4, {|i| i * 2})
# Order: [0, 2, 4, 6] (order not guaranteed but values should match)
puts results

# Test new order parallel.loop(n, context, function) -> closure(ctx, i)
parallel_env = %{"val": 10}
results = parallel.loop(4, parallel_env, {|i| i + val})
puts results

# Test parallel.collect (n, function)
results = parallel.collect(4, {|i| i * 3})
puts results

# Test parallel.collect(n, context, function) with Env injection
collect_env = %{"val": 7}
results = parallel.collect(4, collect_env, {|i| i + val})
puts results

# Test std.collect (sequential)
results = std::collect(4, {|i| i * 4})
puts results

# Test std.collect(n, context, function) with Env injection
collect_env = %{"val": 3}
results = std::collect(4, collect_env, {|i| i + val})
puts results
