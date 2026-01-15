struct Point
{
  x : Float64,
  y : Float64,
  sum: Float64
}

fn Point.sum(self) # Error at line 8: Method 'sum' conflicts with field on Point
  5
end

p = Point {x: 1, y: 2, sum: 3}

puts p
puts p.x
puts p.sum
puts p.sum() # should not work, but throw an error that 3 cannot be executed
