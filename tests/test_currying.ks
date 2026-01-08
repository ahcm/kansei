fn add(x, y)
  x + y
end

add10 = add(10)
puts "Add 10 to 5: " + add10(5)

add10_20 = add(10, 20)
puts "Direct call: " + add10_20

fn apply(f, v)
  f(v)
end

res = apply(add(100), 200)
puts "Apply result: " + res

fn fact(n)
  if n < 2
    1
  else
    n * fact(n - 1)
  end
end
puts "Fact 5: " + fact(5)

fn make_counter()
  count = 0
  fn counter()
    # Note: Mutable closures are tricky. Kansei 'assign' attempts to update upvalues.
    count = count + 1
    count
  end
end

c1 = make_counter()
puts c1()
puts c1()
