use std::IO
IO = std::IO

path = "tests/tmp_lines.txt"
IO.write_lines(path, ["a", "b", "c"])
lines = IO.read_lines(path)
assert_eq(len(lines), 3, "expected 3 lines")
assert_eq(lines[0], "a")
assert_eq(lines[2], "c")

matches = IO.glob("tests/test_logging.ks")
assert_eq(len(matches), 1, "expected one glob match")
assert_eq(matches[0], "tests/test_logging.ks")

IO.remove(path)
