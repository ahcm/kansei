use std::parallel
parallel = std::parallel

env_ctx = %{"a": 10, "b": 20}
map_ctx = {"a": 10, "b": 20}

# Env context fields are injected into the environment.
results1 = parallel.loop(5, env_ctx, {|i|
  i + a + b
})
puts results1

# Map context is not injected; use ctx explicitly.
results2 = parallel.loop(5, map_ctx, {|ctx, i|
  i + ctx.a + ctx.b
})
puts results2
