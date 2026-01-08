arr = [10, 20, 30]
puts arr
puts "Length: " + len(arr)
puts "First: " + arr[0]

user = {
  "name": "Bob",
  "age": 42
}
puts user
puts "Name: " + user["name"]
puts "Age: " + user["age"]

nested = [
  {"a": 1},
  {"b": 2}
]
puts nested
puts nested[1]["b"]

key = "dynamic"
map = {key: "value"}
puts map["dynamic"]
