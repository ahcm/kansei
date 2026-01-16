#[unsafe(no_mangle)]
pub extern "C" fn alloc(size: usize) -> *mut u8
{
    let mut buf = Vec::<u8>::with_capacity(size);
    let ptr = buf.as_mut_ptr();
    std::mem::forget(buf);
    ptr
}

#[unsafe(no_mangle)]
pub extern "C" fn dealloc(ptr: *mut u8, size: usize)
{
    if ptr.is_null() || size == 0
    {
        return;
    }
    unsafe {
        let _ = Vec::from_raw_parts(ptr, 0, size);
    }
}

fn eval_a(i: usize, j: usize) -> f64
{
    let ij = i + j;
    let denom = (ij * (ij + 1) / 2 + i + 1) as f64;
    1.0 / denom
}

fn eval_a_times_u(n: usize, u: &[f64], au: &mut [f64])
{
    for i in 0..n
    {
        let mut sum = 0.0;
        for j in 0..n
        {
            sum += eval_a(i, j) * u[j];
        }
        au[i] = sum;
    }
}

fn eval_at_times_u(n: usize, u: &[f64], au: &mut [f64])
{
    for i in 0..n
    {
        let mut sum = 0.0;
        for j in 0..n
        {
            sum += eval_a(j, i) * u[j];
        }
        au[i] = sum;
    }
}

fn eval_ata_times_u(n: usize, u: &[f64], at_au: &mut [f64])
{
    let mut v = vec![0.0; n];
    eval_a_times_u(n, u, &mut v);
    eval_at_times_u(n, &v, at_au);
}

#[unsafe(no_mangle)]
pub extern "C" fn spectral_norm(n: i32) -> f64
{
    if n <= 0
    {
        return 0.0;
    }
    let n = n as usize;
    let mut u = vec![1.0; n];
    let mut v = vec![0.0; n];
    for _ in 0..10
    {
        eval_ata_times_u(n, &u, &mut v);
        eval_ata_times_u(n, &v, &mut u);
    }
    let mut v_bv = 0.0;
    let mut vv = 0.0;
    for i in 0..n
    {
        v_bv += u[i] * v[i];
        vv += v[i] * v[i];
    }
    (v_bv / vv).sqrt()
}
