use crate::ast::{FloatKind, IntKind};
use crate::intern;
use crate::value::{MapValue, Value};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::simd::Simd;
use std::simd::num::{SimdFloat, SimdInt};

const SIMD_LANES: usize = 64;

fn make_float(value: f64, kind: FloatKind) -> Value
{
    Value::Float { value, kind }
}

fn make_signed_int(value: i128, kind: IntKind) -> Value
{
    Value::Integer { value, kind }
}

fn native_simd_add(args: &[Value]) -> Result<Value, String>
{
    simd_binary_op(args, |a, b| a + b, |a, b| a + b)
}

fn native_simd_sub(args: &[Value]) -> Result<Value, String>
{
    simd_binary_op(args, |a, b| a - b, |a, b| a - b)
}

fn native_simd_mul(args: &[Value]) -> Result<Value, String>
{
    simd_binary_op(args, |a, b| a * b, |a, b| a * b)
}

fn native_simd_div(args: &[Value]) -> Result<Value, String>
{
    simd_binary_op(args, |a, b| a / b, |a, b| a / b)
}

fn native_simd_sum(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 1
    {
        return Err("simd.sum requires exactly 1 array argument".to_string());
    }
    match &args[0]
    {
        Value::F64Array(arr) =>
        {
            let vec = arr.borrow();
            let (prefix, simd, suffix) = vec.as_simd::<SIMD_LANES>();

            let mut sum = Simd::<f64, SIMD_LANES>::splat(0.0);
            for v in simd
            {
                sum += *v;
            }
            let mut result: f64 = sum.reduce_sum();
            for x in prefix.iter().chain(suffix.iter())
            {
                result += x;
            }
            Ok(make_float(result, FloatKind::F64))
        }
        Value::I64Array(arr) =>
        {
            let vec = arr.borrow();
            let (prefix, simd, suffix) = vec.as_simd::<SIMD_LANES>();

            let mut sum = Simd::<i64, SIMD_LANES>::splat(0);
            for v in simd
            {
                sum += *v;
            }
            let mut result: i64 = sum.reduce_sum();
            for x in prefix.iter().chain(suffix.iter())
            {
                result += x;
            }
            Ok(make_signed_int(result as i128, IntKind::I64))
        }
        _ => Err("simd.sum requires F64Array or I64Array argument".to_string()),
    }
}

fn native_simd_dot(args: &[Value]) -> Result<Value, String>
{
    if args.len() != 2
    {
        return Err("simd.dot requires exactly 2 array arguments".to_string());
    }
    match (&args[0], &args[1])
    {
        (Value::F64Array(a), Value::F64Array(b)) =>
        {
            let a_vec = a.borrow();
            let b_vec = b.borrow();
            if a_vec.len() != b_vec.len()
            {
                return Err("simd.dot requires arrays of equal length".to_string());
            }
            let len = a_vec.len();
            let simd_len = len / SIMD_LANES * SIMD_LANES;
            let mut sum = Simd::<f64, SIMD_LANES>::splat(0.0);
            let mut i = 0;
            while i < simd_len
            {
                let va = Simd::<f64, SIMD_LANES>::from_slice(&a_vec[i..i + SIMD_LANES]);
                let vb = Simd::<f64, SIMD_LANES>::from_slice(&b_vec[i..i + SIMD_LANES]);
                sum += va * vb;
                i += SIMD_LANES;
            }
            let mut result: f64 = sum.reduce_sum();
            while i < len
            {
                result += a_vec[i] * b_vec[i];
                i += 1;
            }
            Ok(make_float(result, FloatKind::F64))
        }
        (Value::I64Array(a), Value::I64Array(b)) =>
        {
            let a_vec = a.borrow();
            let b_vec = b.borrow();
            if a_vec.len() != b_vec.len()
            {
                return Err("simd.dot requires arrays of equal length".to_string());
            }
            let len = a_vec.len();
            let simd_len = len / SIMD_LANES * SIMD_LANES;
            let mut sum = Simd::<i64, SIMD_LANES>::splat(0);
            let mut i = 0;
            while i < simd_len
            {
                let va = Simd::<i64, SIMD_LANES>::from_slice(&a_vec[i..i + SIMD_LANES]);
                let vb = Simd::<i64, SIMD_LANES>::from_slice(&b_vec[i..i + SIMD_LANES]);
                sum += va * vb;
                i += SIMD_LANES;
            }
            let mut result: i64 = sum.reduce_sum();
            while i < len
            {
                result += a_vec[i] * b_vec[i];
                i += 1;
            }
            Ok(make_signed_int(result as i128, IntKind::I64))
        }
        _ => Err("simd.dot requires F64Array or I64Array arguments".to_string()),
    }
}

fn simd_binary_op<F64Op, I64Op>(args: &[Value], f64_op: F64Op, i64_op: I64Op) -> Result<Value, String>
where
    F64Op: Fn(Simd<f64, SIMD_LANES>, Simd<f64, SIMD_LANES>) -> Simd<f64, SIMD_LANES>,
    I64Op: Fn(Simd<i64, SIMD_LANES>, Simd<i64, SIMD_LANES>) -> Simd<i64, SIMD_LANES>,
{
    if args.len() != 2
    {
        return Err("SIMD binary operations require exactly 2 array arguments".to_string());
    }
    match (&args[0], &args[1])
    {
        (Value::F64Array(a), Value::F64Array(b)) =>
        {
            let a_vec = a.borrow();
            let b_vec = b.borrow();
            if a_vec.len() != b_vec.len()
            {
                return Err("SIMD operations require arrays of equal length".to_string());
            }
            let mut result = Vec::with_capacity(a_vec.len());
            let (a_prefix, a_simd, a_suffix) = a_vec.as_simd::<SIMD_LANES>();
            let (b_prefix, b_simd, b_suffix) = b_vec.as_simd::<SIMD_LANES>();

            // Handle prefix (unaligned start)
            for (x, y) in a_prefix.iter().zip(b_prefix.iter())
            {
                result.push(f64_op(Simd::splat(*x), Simd::splat(*y))[0]);
            }
            // SIMD main loop
            for (va, vb) in a_simd.iter().zip(b_simd.iter())
            {
                let r = f64_op(*va, *vb);
                result.extend_from_slice(r.as_array());
            }
            // Handle suffix (unaligned end)
            for (x, y) in a_suffix.iter().zip(b_suffix.iter())
            {
                result.push(f64_op(Simd::splat(*x), Simd::splat(*y))[0]);
            }
            Ok(Value::F64Array(Rc::new(RefCell::new(result))))
        }
        (Value::I64Array(a), Value::I64Array(b)) =>
        {
            let a_vec = a.borrow();
            let b_vec = b.borrow();
            if a_vec.len() != b_vec.len()
            {
                return Err("SIMD operations require arrays of equal length".to_string());
            }
            let mut result = Vec::with_capacity(a_vec.len());
            let (a_prefix, a_simd, a_suffix) = a_vec.as_simd::<SIMD_LANES>();
            let (b_prefix, b_simd, b_suffix) = b_vec.as_simd::<SIMD_LANES>();

            // Handle prefix
            for (x, y) in a_prefix.iter().zip(b_prefix.iter())
            {
                result.push(i64_op(Simd::splat(*x), Simd::splat(*y))[0]);
            }
            // SIMD main loop
            for (va, vb) in a_simd.iter().zip(b_simd.iter())
            {
                let r = i64_op(*va, *vb);
                result.extend_from_slice(r.as_array());
            }
            // Handle suffix
            for (x, y) in a_suffix.iter().zip(b_suffix.iter())
            {
                result.push(i64_op(Simd::splat(*x), Simd::splat(*y))[0]);
            }
            Ok(Value::I64Array(Rc::new(RefCell::new(result))))
        }
        _ => Err("SIMD operations require F64Array or I64Array arguments".to_string()),
    }
}

pub fn build_simd_module() -> Value
{
    let mut simd_map = FxHashMap::default();
    simd_map.insert(intern::intern("add"), Value::NativeFunction(native_simd_add));
    simd_map.insert(intern::intern("sub"), Value::NativeFunction(native_simd_sub));
    simd_map.insert(intern::intern("mul"), Value::NativeFunction(native_simd_mul));
    simd_map.insert(intern::intern("div"), Value::NativeFunction(native_simd_div));
    simd_map.insert(intern::intern("sum"), Value::NativeFunction(native_simd_sum));
    simd_map.insert(intern::intern("dot"), Value::NativeFunction(native_simd_dot));
    Value::Map(Rc::new(RefCell::new(MapValue::new(simd_map))))
}
