value = result { f64("invalid") } else 42
assert_eq(value, 42)
value = result { assert_eq(1, 2) } else 17
assert_eq(value, 17)
