# Kansei Language Reference

Kansei is an intuitive scripting language designed for simplicity and flexibility.

## Comments
```ruby
# This is a comment
x = 10 # Inline comment
```

## Data Types

### Primitives
- **Integer**: `1`, `42`, `-10`
- **String**: `"Hello"`, `"World"`
- **Boolean**: `true`, `false`
- **Nil**: `nil`

### Data Structures
- **Array**: Ordered list of values.
  ```ruby
  arr = [1, 2, 3]
  first = arr[0]
  ```
- **Map**: Key-value pairs (keys are strings).
  ```ruby
  user = {"name": "Alice", "age": 30}
  name = user["name"]
  ```

## Variables
Variables are dynamically typed and defined on assignment.
```ruby
x = 10
x = "Now a string"
```

## Operators
- Arithmetic: `+`, `-`, `*`, `/`
- Comparison: `==`, `!=`, `<`, `>`
- String Concatenation: `"Hello " + "World"`

## Control Flow

### If / Else
```ruby
if x < 10
  puts "Less than 10"
elif x == 10
  puts "Equal to 10"
else
  puts "Greater than 10"
end
```

### While Loop
```ruby
while x > 0
  x = x - 1
end
```

### For Loop
Iterates over Arrays (values) or Maps (keys).
```ruby
for item in [1, 2, 3]
  puts item
end
```

## Functions
Functions are first-class citizens. They capture their definition environment (closures).

### Definition
```ruby
fn add(a, b)
  a + b # Implicit return of last expression
end
```

### Calling
```ruby
res = add(1, 2)
```

### Currying
Functions support partial application (currying) by default.
```ruby
add10 = add(10) # Returns a new function that takes 1 argument
res = add10(5)  # Returns 15
```

## Blocks and Yield
Functions can accept a block of code using `{ |params| ... }`. The function can execute this block using `yield`.

```ruby
fn repeater(n)
  i = 0
  while i < n
    yield(i)
    i = i + 1
  end
end

repeater(3) { |idx|
  puts "Index: " + idx
}
```

## Built-in Functions
- `puts(val)`: Print value with newline.
- `print(val)`: Print value without newline.
- `len(obj)`: Return length of String, Array, or Map.
- `read_file(path)`: Read file content as string.
- `write_file(path, content)`: Write string to file.

## Shell Commands
Backticks execute shell commands and capture stdout (trimmed).
```ruby
files = `ls -la`
puts files
```
