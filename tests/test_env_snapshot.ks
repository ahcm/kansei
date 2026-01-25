use std::kansei

ast = std::kansei::ast

env1 = %{"x": 3}
puts ast.eval_in("x + 2", env1, nil)

struct Point { x: Int64, y: Int64 }
p = Point { x: 4, y: 7 }
env2 = %p
puts ast.eval_in("x + y", env2, nil)

fn add1(x)
  x + 1
end
env3 = %{"f": add1, "x": 10}
puts ast.eval_in("f(x)", env3, nil)

x = 9
env4 = %{"x": &x}
puts ast.eval_in("x + 1", env4, nil)

env5 = %{"std": std, "x": 3}
puts ast.eval_in("std::Int64.parse(\"2\") + x", env5, nil)
