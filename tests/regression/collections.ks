a = [1, 2, 3]
a[1] = 7
assert_eq(a[1], 7)
assert_eq(len(a), 3)
sum = 0
for x in a
  sum = sum + x
end
assert_eq(sum, 11)
i = 0
while i < 4
  i = i + 1
end
assert_eq(i, 4)
m = {"answer": 41}
m["answer"] = m["answer"] + 1
assert_eq(m["answer"], 42)
values = collect 4 |j|
  j * 2
end
assert_eq(len(values), 4)
for j in [0, 1, 2, 3]
  assert_eq(values[j], j * 2)
end
