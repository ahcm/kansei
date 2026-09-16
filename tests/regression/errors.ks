value = result { f64("invalid") } else 42
assert_eq(value, 42)
value = result { assert_eq(1, 2) } else 17
assert_eq(value, 17)
use std::kansei
value = result { std::kansei.ast.from_source("end") } else 31
assert_eq(value, 31)
assert_eq(typeof(std::kansei.ast.from_source("1 + 2")), "Ast")
