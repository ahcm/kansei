fn greet(name)
  puts "Hello, " + name
end

greet("Alice")

x = 10
fn modify_x(&x)
  x = 20
  puts "Inside: " + x
end

modify_x(&x)
puts "Outside: " + x

fn add(a, b)
  a + b
end

result = add(5, 7)
puts "Result: " + result

fn factorial(n)
  if n < 2
    1
  else
    n * factorial(n - 1)
  end
end

puts "Factorial of 5: " + factorial(5)
