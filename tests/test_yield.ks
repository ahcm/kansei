fn repeater(n)
  i = 0
  while i < n
    yield(i)
    i = i + 1
  end
end

puts "Repeating 3 times:"
repeater(3) { |idx|
  puts "Index: " + idx
}

x = 100
fn scope_test()
  x = 200
  yield()
end

puts "Lexical scope check:"
scope_test() {
  puts "x should be 100: " + x
}

fn simple()
  puts "Before yield"
  yield()
  puts "After yield"
end

simple() {
  puts "Inside block"
}
