use std::parallel
values = std::parallel.collect(4, {|i| i * 3})
assert_eq(len(values), 4)
for j in [0, 1, 2, 3]
  assert_eq(values[j], j * 3)
end
context = %{"offset": 10}
values = std::parallel.collect(4, context, {|i| i + offset})
assert_eq(len(values), 4)
for j in [0, 1, 2, 3]
  assert_eq(values[j], j + 10)
end
