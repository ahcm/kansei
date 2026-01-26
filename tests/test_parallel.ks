use std::parallel
parallel = std::parallel

# Test classic parallel.collect (n, function)
results = parallel.collect(4, {|i| i * 2})
puts results

# Test new order parallel.collect(n, context, function) -> closure(ctx, i)
parallel_env = %{"val": 10}
results = parallel.collect(4, parallel_env, {|i| i + val})
puts results

# parallel.loop returns nil
result = parallel.loop(4, {|i| i * 2})
puts result

# Test parallel.collect (n, function)
results = parallel.collect(4, {|i| i * 3})
puts results

# Test parallel.collect(n, context, function) with Env injection
collect_env = %{"val": 7}
results = parallel.collect(4, collect_env, {|i| i + val})
puts results

# Test parallel.collect into F64Array
buf = [0.0; 4]
results = parallel.collect(4, {|i| i + 0.5}, buf)
puts typeof(results)
puts results

# Test std.collect (sequential)
results = std::collect(4, {|i| i * 4})
puts results

# Test std.collect(n, context, function) with Env injection
collect_env = %{"val": 3}
results = std::collect(4, collect_env, {|i| i + val})
puts results

# Test std.collect into F64Array
buf = [0.0; 4]
results = std::collect(4, {|i| i * 1.5}, buf)
puts typeof(results)
puts results

# Test collect keyword
results = collect 4 |i|
  i * 5
end
puts results

# Test collect keyword into F64Array
buf = [0.0; 4]
results = collect 4 into buf |i|
  i * 2.0
end
puts typeof(results)
puts results
