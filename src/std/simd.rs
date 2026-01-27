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
    simd_binary_op(args, |a, b| a + b, |a, b| a + b, |a, b| a + b, |a, b| a + b)
}

fn native_simd_sub(args: &[Value]) -> Result<Value, String>
{
    simd_binary_op(args, |a, b| a - b, |a, b| a - b, |a, b| a - b, |a, b| a - b)
}

fn native_simd_mul(args: &[Value]) -> Result<Value, String>
{
    simd_binary_op(args, |a, b| a * b, |a, b| a * b, |a, b| a * b, |a, b| a * b)
}

fn native_simd_div(args: &[Value]) -> Result<Value, String>
{
    simd_binary_op(args, |a, b| a / b, |a, b| a / b, |a, b| a / b, |a, b| a / b)
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
        Value::F32Array(arr) =>
        {
            let vec = arr.borrow();
            let (prefix, simd, suffix) = vec.as_simd::<SIMD_LANES>();

            let mut sum = Simd::<f32, SIMD_LANES>::splat(0.0);
            for v in simd
            {
                sum += *v;
            }
            let mut result: f32 = sum.reduce_sum();
            for x in prefix.iter().chain(suffix.iter())
            {
                result += x;
            }
            Ok(make_float(result as f64, FloatKind::F32))
        }
        Value::I32Array(arr) =>
        {
            let vec = arr.borrow();
            let (prefix, simd, suffix) = vec.as_simd::<SIMD_LANES>();

            let mut sum = Simd::<i32, SIMD_LANES>::splat(0);
            for v in simd
            {
                sum += *v;
            }
            let mut result: i32 = sum.reduce_sum();
            for x in prefix.iter().chain(suffix.iter())
            {
                result += x;
            }
            Ok(make_signed_int(result as i128, IntKind::I32))
        }
        _ =>
        {
            Err("simd.sum requires F64Array, F32Array, I64Array, or I32Array argument".to_string())
        }
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
                return Err(format!(
                    "simd.dot requires arrays of equal length {} vs {}",
                    a_vec.len(),
                    b_vec.len()
                ));
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
                return Err(format!(
                    "simd.dot requires arrays of equal length {} vs {}",
                    a_vec.len(),
                    b_vec.len()
                ));
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
        (Value::F32Array(a), Value::F32Array(b)) =>
        {
            let a_vec = a.borrow();
            let b_vec = b.borrow();
            if a_vec.len() != b_vec.len()
            {
                return Err(format!(
                    "simd.dot requires arrays of equal length {} vs {}",
                    a_vec.len(),
                    b_vec.len()
                ));
            }
            let len = a_vec.len();
            let simd_len = len / SIMD_LANES * SIMD_LANES;
            let mut sum = Simd::<f32, SIMD_LANES>::splat(0.0);
            let mut i = 0;
            while i < simd_len
            {
                let va = Simd::<f32, SIMD_LANES>::from_slice(&a_vec[i..i + SIMD_LANES]);
                let vb = Simd::<f32, SIMD_LANES>::from_slice(&b_vec[i..i + SIMD_LANES]);
                sum += va * vb;
                i += SIMD_LANES;
            }
            let mut result: f32 = sum.reduce_sum();
            while i < len
            {
                result += a_vec[i] * b_vec[i];
                i += 1;
            }
            Ok(make_float(result as f64, FloatKind::F32))
        }
        (Value::I32Array(a), Value::I32Array(b)) =>
        {
            let a_vec = a.borrow();
            let b_vec = b.borrow();
            if a_vec.len() != b_vec.len()
            {
                return Err(format!(
                    "simd.dot requires arrays of equal length {} vs {}",
                    a_vec.len(),
                    b_vec.len()
                ));
            }
            let len = a_vec.len();
            let simd_len = len / SIMD_LANES * SIMD_LANES;
            let mut sum = Simd::<i32, SIMD_LANES>::splat(0);
            let mut i = 0;
            while i < simd_len
            {
                let va = Simd::<i32, SIMD_LANES>::from_slice(&a_vec[i..i + SIMD_LANES]);
                let vb = Simd::<i32, SIMD_LANES>::from_slice(&b_vec[i..i + SIMD_LANES]);
                sum += va * vb;
                i += SIMD_LANES;
            }
            let mut result: i32 = sum.reduce_sum();
            while i < len
            {
                result += a_vec[i] * b_vec[i];
                i += 1;
            }
            Ok(make_signed_int(result as i128, IntKind::I32))
        }
        _ => Err("simd.dot requires matching F64Array, F32Array, I64Array, or I32Array arguments"
            .to_string()),
    }
}

fn simd_binary_op<F64Op, I64Op, F32Op, I32Op>(
    args: &[Value],
    f64_op: F64Op,
    i64_op: I64Op,
    f32_op: F32Op,
    i32_op: I32Op,
) -> Result<Value, String>
where
    F64Op: Fn(Simd<f64, SIMD_LANES>, Simd<f64, SIMD_LANES>) -> Simd<f64, SIMD_LANES>,
    I64Op: Fn(Simd<i64, SIMD_LANES>, Simd<i64, SIMD_LANES>) -> Simd<i64, SIMD_LANES>,
    F32Op: Fn(Simd<f32, SIMD_LANES>, Simd<f32, SIMD_LANES>) -> Simd<f32, SIMD_LANES>,
    I32Op: Fn(Simd<i32, SIMD_LANES>, Simd<i32, SIMD_LANES>) -> Simd<i32, SIMD_LANES>,
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
                return Err(format!(
                    "SIMD operations require arrays of equal length {} vs {}",
                    a_vec.len(),
                    b_vec.len()
                ));
            }
            let mut result = Vec::with_capacity(a_vec.len());
            let (a_prefix, a_simd, a_suffix) = a_vec.as_simd::<SIMD_LANES>();

            // Handle prefix (unaligned start)
            let prefix_len = a_prefix.len();
            for i in 0..prefix_len
            {
                result.push(f64_op(Simd::splat(a_vec[i]), Simd::splat(b_vec[i]))[0]);
            }
            // SIMD main loop
            for (chunk_idx, va) in a_simd.iter().enumerate()
            {
                let start = prefix_len + chunk_idx * SIMD_LANES;
                let vb = Simd::<f64, SIMD_LANES>::from_slice(&b_vec[start..start + SIMD_LANES]);
                let r = f64_op(*va, vb);
                result.extend_from_slice(r.as_array());
            }
            // Handle suffix (unaligned end)
            let suffix_len = a_suffix.len();
            let suffix_start = a_vec.len() - suffix_len;
            for i in suffix_start..a_vec.len()
            {
                result.push(f64_op(Simd::splat(a_vec[i]), Simd::splat(b_vec[i]))[0]);
            }
            Ok(Value::F64Array(Rc::new(RefCell::new(result))))
        }
        (Value::I64Array(a), Value::I64Array(b)) =>
        {
            let a_vec = a.borrow();
            let b_vec = b.borrow();
            if a_vec.len() != b_vec.len()
            {
                return Err(format!(
                    "SIMD operations require arrays of equal length {} vs {}",
                    a_vec.len(),
                    b_vec.len()
                ));
            }
            let mut result = Vec::with_capacity(a_vec.len());
            let (a_prefix, a_simd, a_suffix) = a_vec.as_simd::<SIMD_LANES>();

            // Handle prefix
            let prefix_len = a_prefix.len();
            for i in 0..prefix_len
            {
                result.push(i64_op(Simd::splat(a_vec[i]), Simd::splat(b_vec[i]))[0]);
            }
            // SIMD main loop
            for (chunk_idx, va) in a_simd.iter().enumerate()
            {
                let start = prefix_len + chunk_idx * SIMD_LANES;
                let vb = Simd::<i64, SIMD_LANES>::from_slice(&b_vec[start..start + SIMD_LANES]);
                let r = i64_op(*va, vb);
                result.extend_from_slice(r.as_array());
            }
            // Handle suffix
            let suffix_len = a_suffix.len();
            let suffix_start = a_vec.len() - suffix_len;
            for i in suffix_start..a_vec.len()
            {
                result.push(i64_op(Simd::splat(a_vec[i]), Simd::splat(b_vec[i]))[0]);
            }
            Ok(Value::I64Array(Rc::new(RefCell::new(result))))
        }
        (Value::F32Array(a), Value::F32Array(b)) =>
        {
            let a_vec = a.borrow();
            let b_vec = b.borrow();
            if a_vec.len() != b_vec.len()
            {
                return Err(format!(
                    "SIMD operations require arrays of equal length {} vs {}",
                    a_vec.len(),
                    b_vec.len()
                ));
            }
            let mut result = Vec::with_capacity(a_vec.len());
            let (a_prefix, a_simd, a_suffix) = a_vec.as_simd::<SIMD_LANES>();

            let prefix_len = a_prefix.len();
            for i in 0..prefix_len
            {
                result.push(f32_op(Simd::splat(a_vec[i]), Simd::splat(b_vec[i]))[0]);
            }
            for (chunk_idx, va) in a_simd.iter().enumerate()
            {
                let start = prefix_len + chunk_idx * SIMD_LANES;
                let vb = Simd::<f32, SIMD_LANES>::from_slice(&b_vec[start..start + SIMD_LANES]);
                let r = f32_op(*va, vb);
                result.extend_from_slice(r.as_array());
            }
            let suffix_len = a_suffix.len();
            let suffix_start = a_vec.len() - suffix_len;
            for i in suffix_start..a_vec.len()
            {
                result.push(f32_op(Simd::splat(a_vec[i]), Simd::splat(b_vec[i]))[0]);
            }
            Ok(Value::F32Array(Rc::new(RefCell::new(result))))
        }
        (Value::I32Array(a), Value::I32Array(b)) =>
        {
            let a_vec = a.borrow();
            let b_vec = b.borrow();
            if a_vec.len() != b_vec.len()
            {
                return Err(format!(
                    "SIMD operations require arrays of equal length {} vs {}",
                    a_vec.len(),
                    b_vec.len()
                ));
            }
            let mut result = Vec::with_capacity(a_vec.len());
            let (a_prefix, a_simd, a_suffix) = a_vec.as_simd::<SIMD_LANES>();

            let prefix_len = a_prefix.len();
            for i in 0..prefix_len
            {
                result.push(i32_op(Simd::splat(a_vec[i]), Simd::splat(b_vec[i]))[0]);
            }
            for (chunk_idx, va) in a_simd.iter().enumerate()
            {
                let start = prefix_len + chunk_idx * SIMD_LANES;
                let vb = Simd::<i32, SIMD_LANES>::from_slice(&b_vec[start..start + SIMD_LANES]);
                let r = i32_op(*va, vb);
                result.extend_from_slice(r.as_array());
            }
            let suffix_len = a_suffix.len();
            let suffix_start = a_vec.len() - suffix_len;
            for i in suffix_start..a_vec.len()
            {
                result.push(i32_op(Simd::splat(a_vec[i]), Simd::splat(b_vec[i]))[0]);
            }
            Ok(Value::I32Array(Rc::new(RefCell::new(result))))
        }
        _ => Err(
            "SIMD operations require matching F64Array, F32Array, I64Array, or I32Array arguments"
                .to_string(),
        ),
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
