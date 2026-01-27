use std::OS
OS = std::OS

res = OS.run("echo", ["hi"])
assert(res.success)
assert_eq(res.stdout, "hi\n")
