struct Point {
  x: Float64,
  y: Float64
}

fn sum(point { x: Float64, y: Float64 })
  point.x + point.y
end

p = Point { x: 3.0, y: 4.0 }
puts p.x
puts sum(p)

puts sum(Point { x: 3.0, y: 4.0 })

p.x = 6
puts p.x
puts sum(p)
