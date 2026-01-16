puts f"not false -> {not false}"
puts f"not nil -> {not nil}"
puts f"not 0 -> {not 0}"

puts f"false and 1 -> {false and 1}"
puts f"1 and 0 -> {1 and 0}"
puts f"nil and 1 -> {nil and 1}"

puts f"false or 1 -> {false or 1}"
puts f"nil or 1 -> {nil or 1}"
puts f"1 or 0 -> {1 or 0}"

puts f"true && false -> {true && false}"
puts f"true && true -> {true && true}"
puts f"false && true -> {false && true}"

puts f"true || false -> {true || false}"
puts f"false || false -> {false || false}"
puts f"false || true -> {false || true}"

# Uncomment to see strict boolean errors for &&/||
# puts f"{1 && true}"
# puts f"{0 || false}"
