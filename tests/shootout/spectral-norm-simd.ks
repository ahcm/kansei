# Spectral Norm benchmark using std::simd
# Uses SIMD operations for dot products in matrix-vector multiplies

@file
use std::simd
@file
simd = std::simd

@file
fn eval_A(i, j)
  1.0 / ((i + j) * (i + j + 1) / 2 + i + 1)
end

# Precompute row i of matrix A as an F64Array
@file
fn compute_A_row(i, n)
  collect n into [0.0;n] |j|
    1.0 / ((i + j) * (i + j + 1) / 2 + i + 1)
  end
end

# Precompute row i of matrix A^T (which is column i of A)
@file
fn compute_At_row(i, n)
  collect n into [0.0;n] |j|
    1.0 / ((j + i) * (j + i + 1) / 2 + j + 1)
  end
end

@file
fn eval_A_times_u_simd(n, u, au, A_rows)
  loop n |i|
    au[i] = simd.dot(A_rows[i], u)
  end
end

@file
fn eval_At_times_u_simd(n, u, au, At_rows)
  collect n into au |i|
    simd.dot(At_rows[i], u)
  end
end

@file
fn eval_AtA_times_u_simd(n, u, atAu, A_rows, At_rows)
  v = [0.0; n]
  eval_A_times_u_simd(n, u, v, A_rows)
  eval_At_times_u_simd(n, v, atAu, At_rows)
end

fn main(n)
  # Precompute matrix rows for SIMD dot products
  A_rows = [[0.0; n];n]
  At_rows = [[0.0; n];n]
  
  collect n into A_rows |i|
    compute_A_row(i, n)
  end
  collect n into At_rows |i|
    compute_At_row(i, n)
  end

  u = [1.0; n]
  v = [0.0; n]

  loop 10 |i|
    eval_AtA_times_u_simd(n, u, v, A_rows, At_rows)
    eval_AtA_times_u_simd(n, v, u, A_rows, At_rows)
  end

  # Use SIMD for final dot products
  vBv = simd.dot(u, v)
  vv = simd.dot(v, v)

  use std::Float64
  Float64 = std::Float64
  res = Float64.sqrt(vBv / vv)
  puts f"{res:.9}"
end

n = program.args[0]
use std::Int64
Int64 = std::Int64
n = Int64::parse(n)
main(n)
