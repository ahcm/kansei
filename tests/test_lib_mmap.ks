use std::IO
use std::lib::Bytes
use std::lib::Mmap

IO = std::IO
Bytes = std::lib::Bytes
Mmap = std::lib::Mmap

path = "tmp_mmap_test.bin"
IO.write(path, "hello")

map = Mmap.open(path, "r")
bytes = Mmap.read(map, 0, 5)
puts Bytes.to_string(bytes)
view = Bytes.slice_view(map, 1, 3)
puts Bytes.to_string(view)

rw = Mmap.open(path, "rw")
Mmap.write(rw, 0, Bytes.from_string("HELLO"))
Mmap.flush(rw)

map2 = Mmap.open(path, "r")
bytes2 = Mmap.read(map2, 0, 5)
puts Bytes.to_string(bytes2)

IO.remove(path)
