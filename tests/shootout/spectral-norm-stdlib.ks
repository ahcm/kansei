use std::lib::tests
tests = std::lib::tests

n = program.args[0]
use std::Int64
Int64 = std::Int64
n = Int64::parse(n)

res = tests.spectralnorm(n)

use std::Float64
Float64 = std::Float64
puts f"{res:.9}"
