# Exercise AST expressions and repeatedly called functions (bytecode/cache paths).
fn equal(a, b)
  a == b
end
fn different(a, b)
  a != b
end
assert(true == true)
assert(false == false)
assert(true != false)
assert(nil == nil)
assert(nil != false)
loop 50
  assert(equal(true, true))
  assert(equal(false, false))
  assert(not equal(true, false))
  assert(different(true, false))
  assert(not different(false, false))
  assert(equal(nil, nil))
  assert(not equal(nil, false))
  assert(equal("same", "same"))
  assert(not equal("same", "other"))
  assert(not equal("1", 1))
  assert(equal(1, 1))
  assert(equal(1, 1.0))
  assert(equal(1.0, 1))
  assert(not equal(1, 2))
  assert(equal([true, nil, 1], [true, nil, 1]))
end
