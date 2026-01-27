# About
Kansei (感性) is a language based on Rust in the spirit of Ruby 1.8.

Not supporting Rails is seen as a benefit.

A number of snippets can be found in files in the tests/ directory.

It supports currying:
```
fn add(x, y)
  x + y
end

add10 = add(10)
puts "Add 10 to 5: " + add10(5)
```

Logging defaults to stderr via `log(...)` and can be redirected with `-l/--log <path>`.
---
Have fun and may the force be with you!

-- Andreas Hauser, München, 8.1.2026
