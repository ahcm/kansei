use crate::ast::FloatKind;
use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

fn n_from_args(args: &[Value]) -> Result<usize, String>
{
    match args.get(0)
    {
        Some(Value::Integer { value, .. }) if *value >= 0 =>
        {
            usize::try_from(*value).map_err(|_| "tests.spectralnorm expects n >= 0".to_string())
        }
        Some(Value::Unsigned { value, .. }) =>
        {
            usize::try_from(*value).map_err(|_| "tests.spectralnorm expects n >= 0".to_string())
        }
        Some(Value::Float { value, .. }) if *value >= 0.0 => Ok(*value as usize),
        _ => Err("tests.spectralnorm expects a non-negative integer".to_string()),
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

fn native_spectralnorm(args: &[Value]) -> Result<Value, String>
{
    let n = n_from_args(args)?;
    if n == 0
    {
        return Ok(Value::Float {
            value: 0.0,
            kind: FloatKind::F64,
        });
    }
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
    Ok(Value::Float {
        value: (v_bv / vv).sqrt(),
        kind: FloatKind::F64,
    })
}

pub fn build_tests_module() -> Value
{
    let mut map = FxHashMap::default();
    map.insert(
        intern::intern("spectralnorm"),
        Value::NativeFunction(native_spectralnorm),
    );
    Value::Map(Rc::new(RefCell::new(MapValue::new(map))))
}
