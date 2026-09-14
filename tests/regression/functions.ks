fn add(x, y)
  x + y
end
add10 = add(10)
assert_eq(add10(5), 15)
fn factorial(n)
  if n < 2
    1
  else
    n * factorial(n - 1)
  end
end
assert_eq(factorial(6), 720)
fn early(n)
  if n > 0
    return 17
  end
  23
end
assert_eq(early(1), 17)
assert_eq(early(0), 23)
fn apply(f, x)
  f(x)
end
assert_eq(apply(add(100), 5), 105)
