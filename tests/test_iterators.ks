arr = [1, 2, 3]
puts "Iterating array:"
for x in arr
  puts x * 10
end

map = {"a": 100, "b": 200}
puts "Iterating map keys:"
for k in map
  puts "Key: " + k + ", Value: " + map[k]
end

i = 99
puts "Before loop i: " + i
for i in [1, 2]
  puts "Inside loop i: " + i
end
puts "After loop i: " + i
