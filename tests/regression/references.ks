fn bump(&value)
  value = value + 1
end
fn local_reference()
  value = 10
  bump(&value)
  assert_eq(value, 11)
  value = 20
  bump(&value)
  assert_eq(value, 21)
end
local_reference()
fn make_counter(start)
  count = start
  fn counter(&count, delta)
    count = count + delta
    count
  end
  counter(&count)
end
first = make_counter(0)
second = make_counter(100)
assert_eq(first(1), 1)
assert_eq(first(2), 3)
assert_eq(second(5), 105)
assert_eq(first(4), 7)
fn bind_two(&left, &right, delta)
  left = left + delta
  right = right + delta
  left + right
end
left = 1
right = 10
partial = bind_two(&left)
complete = partial(&right)
assert_eq(complete(2), 15)
assert_eq(left, 3)
assert_eq(right, 12)
assert_eq(complete(3), 21)
assert_eq(left, 6)
assert_eq(right, 15)
