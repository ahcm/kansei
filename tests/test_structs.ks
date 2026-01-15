struct Point
{
  x: Float64,
  y: Float64
}

struct Triangle
{
  x: Float64,
  y: Float64,
  z: Float64
}

fn sum(p { x: Float64, y: Float64 })    # matching structs that have x and y fields
  p.x + p.y
end

p = Point { x: 3.0, y: 4.0 }
puts p.x
puts sum(p)

puts sum(Point { x: 3.0, y: 4.0 })      # anonymous struct

p.x = 6
puts p.x
puts sum(p)

fn Point.sum(self)      # only works on Point, needs self, cannot shadow field
  self.x + self.y
end
  
puts p.sum()

t = Triangle {x:1, y:2, z:3}
sum(t)    # works
#t.sum()   # function not defined for Triangle

fn tri_sum(t {x: Float64, y: Float64, z: Float64})
  t.x + t.y + t.z
end

puts tri_sum(t)

#puts tri_sum(p)   # Error at line 44: t missing field 'z'

