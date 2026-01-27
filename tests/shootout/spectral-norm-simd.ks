# Spectral Norm benchmark using std::simd
# Uses SIMD operations for dot products in matrix-vector multiplies

@file
use std::simd
@file
simd = std::simd

# Precompute row i of matrix A as an F64Array
# row[j] = eval_A(i, j)
@file
fn compute_A_row(i, n)
  j = collect n into [0.0; n] |j| f64(j) end
  #j = [{|j| f64(j)};n]
  i_arr = [f64(i); n]
  ones = [1.0; n]
  two = [2.0; n]
  ij = simd.add(i_arr, j)
  ij1 = simd.add(ij, ones)
  prod = simd.mul(ij, ij1)
  half = simd.div(prod, two)
  denom = simd.add(half, simd.add(i_arr, ones))
  simd.div(ones, denom)
end

# Precompute row i of matrix A^T (which is column i of A)
# row[j] = eval_A(j, i)
@file
fn compute_At_row(i, n)
  j = collect n into [0.0; n] |j| f64(j) end
  i_arr = [f64(i); n]
  ones = [1.0; n]
  two = [2.0; n]
  ij = simd.add(j, i_arr)
  ij1 = simd.add(ij, ones)
  prod = simd.mul(ij, ij1)
  half = simd.div(prod, two)
  denom = simd.add(half, simd.add(j, ones))
  simd.div(ones, denom)
end

@file
fn eval_AtA_times_u_simd(n, u, atAu, A_rows, At_rows)
  v = collect n into [0.0;n] |i| simd.dot(A_rows[i], u) end
  collect n into atAu |i| simd.dot(At_rows[i], v) end
end

fn main(n)
  # Precompute matrix rows for SIMD dot products
  A_rows  = collect n |i| compute_A_row(i,n) end
  At_rows = collect n |i| compute_At_row(i, n) end  

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
  res = std::Float64.sqrt(vBv / vv)
  puts f"{res:.9}"
end

n = program.args[0]
n = i64(n)
main(n)
