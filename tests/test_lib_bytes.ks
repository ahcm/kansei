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

Bytes.fill(buf, 3)
buf2 = Bytes.buf(5, 0)
Bytes.copy(buf2, 1, bytes, 0, 3)
puts Bytes.to_array(buf2)
puts Bytes.find(Bytes.from_string("hello"), Bytes.from_string("ell"))
