# Spectral Norm benchmark using std::simd
# Uses SIMD operations for dot products in matrix-vector multiplies
# Uses std::parallel for row generation

@file
use std::simd
@file
simd = std::simd
@file
use std::parallel
@file
parallel = std::parallel

@file
fn eval_A(i, j)
  1.0 / ((i + j) * (i + j + 1) / 2 + i + 1)
end

# Precompute row i of matrix A as an F64Array
# row[j] = eval_A(i, j)
@file
fn compute_A_row(i, n)
  # Use parallel.loop(n, func, ctx)
  # func receives (j, i) and calls eval_a(i, j)
  # We use parallel.eval_a_i_j for this (args[1]=i, args[0]=j)
  parallel.loop(n, parallel.eval_a_i_j, i)
end

# Precompute row i of matrix A^T (which is column i of A)
# row[j] = eval_A(j, i)
@file
fn compute_At_row(i, n)
  # Use parallel.loop(n, func, ctx)
  # func receives (j, i) and calls eval_a(j, i)
  # We use parallel.eval_a_j_i for this (args[1]=i, args[0]=j)
  parallel.loop(n, parallel.eval_a_j_i, i)
end

@file
fn eval_A_times_u_simd(n, u, au, A_rows)
  loop n |i|
    au[i] = simd.dot(A_rows[i], u)
  end
end

@file
fn eval_At_times_u_simd(n, u, au, At_rows)
  loop n |i|
    au[i] = simd.dot(At_rows[i], u)
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
  A_rows = []
  At_rows = []
  
  # Note: A_rows generation can also be parallelized if we wanted, 
  # but here we parallelize the inner loop (row generation).
  
  loop n |i|
    A_rows = A_rows + [compute_A_row(i, n)]
    At_rows = At_rows + [compute_At_row(i, n)]
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