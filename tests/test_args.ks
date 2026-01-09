puts "Program Name:"
puts program.name
puts "Program Args:"
puts program.args
puts "Len: " + len(program.args)

for arg in program.args
  puts "Arg: " + arg
end
