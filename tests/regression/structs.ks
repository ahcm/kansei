struct Point { x: Float64, y: Float64 }
fn Point.sum(self)
  self.x + self.y
end
p = Point { x: 3.0, y: 4.0 }
assert_eq(p.sum(), 7.0)
p.x = 6
assert_eq(p.sum(), 10.0)
fn total(p { x: Float64, y: Float64 })
  p.x + p.y
end
assert_eq(total(p), 10.0)
