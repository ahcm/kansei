use std::lib::Bytes

Bytes = std::lib::Bytes

buf = Bytes.buf(3, 7)
Bytes.set(buf, 1, 255)
Bytes.push(buf, 1)
puts Bytes.len(buf)
puts Bytes.get(buf, 0)
puts Bytes.get(buf, 1)
puts Bytes.get(buf, 3)

bytes = Bytes.freeze(buf)
puts Bytes.len(bytes)
puts Bytes.to_array(bytes)
puts Bytes.to_string(Bytes.from_string("hello"))
