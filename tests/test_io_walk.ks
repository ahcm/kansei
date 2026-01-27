use std::IO
IO = std::IO

tmp = "tests/tmp_walk"
IO.mkdirs(tmp)
IO.write(tmp + "/a.ks", "puts 1")
IO.mkdirs(tmp + "/sub")
IO.write(tmp + "/sub/b.ks", "puts 2")

paths = IO.walk(tmp)
assert(len(paths) >= 2)

IO.remove(tmp + "/a.ks")
IO.remove(tmp + "/sub/b.ks")
