//! Numeric conversions and shared arithmetic semantics.
use super::*;

pub(super) fn normalize_float_value(value: f64, kind: FloatKind) -> f64
{
    match kind
    {
        FloatKind::F32 => (value as f32) as f64,
        FloatKind::F64 | FloatKind::F128 => value,
    }
}

pub(super) fn clone_value(value: &Value) -> Value
{
    deep_clone_value(value)
}

pub(super) fn promote_float_kind(left: FloatKind, right: FloatKind) -> FloatKind
{
    let rank = |kind| match kind
    {
        FloatKind::F32 => 0,
        FloatKind::F64 => 1,
        FloatKind::F128 => 2,
    };
    if rank(left) >= rank(right)
    {
        left
    }
    else
    {
        right
    }
}

pub(super) fn pow_signed_int(base: i128, exp: i128, kind: IntKind) -> Value
{
    if exp < 0
    {
        return make_float((base as f64).powf(exp as f64), FloatKind::F64);
    }
    if let Ok(exp_u32) = u32::try_from(exp)
    {
        make_signed_int(base.pow(exp_u32), kind)
    }
    else
    {
        make_float((base as f64).powf(exp as f64), FloatKind::F64)
    }
}

pub(super) fn pow_unsigned_int(base: u128, exp: u128, kind: IntKind) -> Value
{
    if let Ok(exp_u32) = u32::try_from(exp)
    {
        make_unsigned_int(base.pow(exp_u32), kind)
    }
    else
    {
        make_float((base as f64).powf(exp as f64), FloatKind::F64)
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) enum BinOpKind
{
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Eq,
    Gt,
    Lt,
}

pub(super) fn eval_binop(op: BinOpKind, l: Value, r: Value) -> EvalResult
{
    let res = match (l, r)
    {
        (
            Value::Integer {
                value: i1,
                kind: k1,
            },
            Value::Integer {
                value: i2,
                kind: k2,
            },
        ) =>
        {
            let kind = signed_kind_for_bits(int_kind_bits(k1).max(int_kind_bits(k2)));
            match op
            {
                BinOpKind::Add => make_signed_int(i1 + i2, kind),
                BinOpKind::Sub => make_signed_int(i1 - i2, kind),
                BinOpKind::Mul => make_signed_int(i1 * i2, kind),
                BinOpKind::Div => make_signed_int(i1 / i2, kind),
                BinOpKind::Pow => pow_signed_int(i1, i2, kind),
                BinOpKind::Eq => Value::Boolean(i1 == i2),
                BinOpKind::Gt => Value::Boolean(i1 > i2),
                BinOpKind::Lt => Value::Boolean(i1 < i2),
            }
        }
        (
            Value::Unsigned {
                value: u1,
                kind: k1,
            },
            Value::Unsigned {
                value: u2,
                kind: k2,
            },
        ) =>
        {
            let kind = unsigned_kind_for_bits(int_kind_bits(k1).max(int_kind_bits(k2)));
            match op
            {
                BinOpKind::Add => make_unsigned_int(u1 + u2, kind),
                BinOpKind::Sub => make_unsigned_int(u1 - u2, kind),
                BinOpKind::Mul => make_unsigned_int(u1 * u2, kind),
                BinOpKind::Div => make_unsigned_int(u1 / u2, kind),
                BinOpKind::Pow => pow_unsigned_int(u1, u2, kind),
                BinOpKind::Eq => Value::Boolean(u1 == u2),
                BinOpKind::Gt => Value::Boolean(u1 > u2),
                BinOpKind::Lt => Value::Boolean(u1 < u2),
            }
        }
        (Value::Integer { value: i1, .. }, Value::Unsigned { value: u2, .. }) =>
        {
            let u2_i = i128::try_from(u2).map_err(|_| RuntimeError::simple("Unsigned value too large for signed operation".to_string(), 0))?;
            match op
            {
                BinOpKind::Add => make_signed_int(i1 + u2_i, IntKind::I128),
                BinOpKind::Sub => make_signed_int(i1 - u2_i, IntKind::I128),
                BinOpKind::Mul => make_signed_int(i1 * u2_i, IntKind::I128),
                BinOpKind::Div => make_signed_int(i1 / u2_i, IntKind::I128),
                BinOpKind::Pow => pow_signed_int(i1, u2_i, IntKind::I128),
                BinOpKind::Eq => Value::Boolean(i1 == u2_i),
                BinOpKind::Gt => Value::Boolean(i1 > u2_i),
                BinOpKind::Lt => Value::Boolean(i1 < u2_i),
            }
        }
        (Value::Unsigned { value: u1, .. }, Value::Integer { value: i2, .. }) =>
        {
            let u1_i = i128::try_from(u1).map_err(|_| RuntimeError::simple("Unsigned value too large for signed operation".to_string(), 0))?;
            match op
            {
                BinOpKind::Add => make_signed_int(u1_i + i2, IntKind::I128),
                BinOpKind::Sub => make_signed_int(u1_i - i2, IntKind::I128),
                BinOpKind::Mul => make_signed_int(u1_i * i2, IntKind::I128),
                BinOpKind::Div => make_signed_int(u1_i / i2, IntKind::I128),
                BinOpKind::Pow => pow_signed_int(u1_i, i2, IntKind::I128),
                BinOpKind::Eq => Value::Boolean(u1_i == i2),
                BinOpKind::Gt => Value::Boolean(u1_i > i2),
                BinOpKind::Lt => Value::Boolean(u1_i < i2),
            }
        }
        (
            Value::Float {
                value: f1,
                kind: k1,
            },
            Value::Float {
                value: f2,
                kind: k2,
            },
        ) =>
        {
            let kind = promote_float_kind(k1, k2);
            match op
            {
                BinOpKind::Add => make_float(f1 + f2, kind),
                BinOpKind::Sub => make_float(f1 - f2, kind),
                BinOpKind::Mul => make_float(f1 * f2, kind),
                BinOpKind::Div => make_float(f1 / f2, kind),
                BinOpKind::Pow => make_float(f1.powf(f2), kind),
                BinOpKind::Eq => Value::Boolean(f1 == f2),
                BinOpKind::Gt => Value::Boolean(f1 > f2),
                BinOpKind::Lt => Value::Boolean(f1 < f2),
            }
        }
        (v @ Value::Integer { .. }, Value::Float { value: f, kind })
        | (v @ Value::Unsigned { .. }, Value::Float { value: f, kind }) =>
        {
            let f1 = int_value_as_f64(&v).unwrap_or(0.0);
            match op
            {
                BinOpKind::Add => make_float(f1 + f, kind),
                BinOpKind::Sub => make_float(f1 - f, kind),
                BinOpKind::Mul => make_float(f1 * f, kind),
                BinOpKind::Div => make_float(f1 / f, kind),
                BinOpKind::Pow => make_float(f1.powf(f), kind),
                BinOpKind::Eq => Value::Boolean(f1 == f),
                BinOpKind::Gt => Value::Boolean(f1 > f),
                BinOpKind::Lt => Value::Boolean(f1 < f),
            }
        }
        (Value::Float { value: f, kind }, v @ Value::Integer { .. })
        | (Value::Float { value: f, kind }, v @ Value::Unsigned { .. }) =>
        {
            let f2 = int_value_as_f64(&v).unwrap_or(0.0);
            match op
            {
                BinOpKind::Add => make_float(f + f2, kind),
                BinOpKind::Sub => make_float(f - f2, kind),
                BinOpKind::Mul => make_float(f * f2, kind),
                BinOpKind::Div => make_float(f / f2, kind),
                BinOpKind::Pow => make_float(f.powf(f2), kind),
                BinOpKind::Eq => Value::Boolean(f == f2),
                BinOpKind::Gt => Value::Boolean(f > f2),
                BinOpKind::Lt => Value::Boolean(f < f2),
            }
        }
        (Value::String(s1), Value::String(s2)) => match op
        {
            BinOpKind::Eq => Value::Boolean(s1 == s2),
            BinOpKind::Add =>
            {
                let mut out = s1.clone();
                Rc::make_mut(&mut out).push_str(&s2);
                Value::String(out)
            }
            _ =>
            {
                return Err(RuntimeError::simple("Invalid types for operation".to_string(), 0));
            }
        },
        (Value::String(s), v2) => match op
        {
            BinOpKind::Eq => Value::Boolean(false),
            BinOpKind::Add =>
            {
                let mut out = s.clone();
                Rc::make_mut(&mut out).push_str(&v2.inspect());
                Value::String(out)
            }
            _ =>
            {
                return Err(RuntimeError::simple("Invalid types for operation".to_string(), 0));
            }
        },
        (left, right) if matches!(op, BinOpKind::Eq) => Value::Boolean(left == right),
        _ =>
        {
            return Err(RuntimeError::simple("Invalid types for operation".to_string(), 0));
        }
    };
    Ok(res)
}

pub(super) fn eval_cached_binop(
    op: BinOpKind,
    cache: &Rc<RefCell<BinaryOpCache>>,
    l: Value,
    r: Value,
) -> EvalResult
{
    let cached = cache.borrow().kind.clone();
    if let Some(kind) = cached.clone()
    {
        match kind
        {
            BinaryOpCacheKind::Float =>
            {
                if let (
                    Value::Float {
                        value: f1,
                        kind: k1,
                    },
                    Value::Float {
                        value: f2,
                        kind: k2,
                    },
                ) = (&l, &r)
                {
                    let kind = promote_float_kind(*k1, *k2);
                    let res = match op
                    {
                        BinOpKind::Add => make_float(f1 + f2, kind),
                        BinOpKind::Sub => make_float(f1 - f2, kind),
                        BinOpKind::Mul => make_float(f1 * f2, kind),
                        BinOpKind::Div => make_float(f1 / f2, kind),
                        BinOpKind::Pow => make_float(f1.powf(*f2), kind),
                        BinOpKind::Eq => Value::Boolean(f1 == f2),
                        BinOpKind::Gt => Value::Boolean(f1 > f2),
                        BinOpKind::Lt => Value::Boolean(f1 < f2),
                    };
                    cache.borrow_mut().hits += 1;
                    return Ok(res);
                }
            }
            BinaryOpCacheKind::Int =>
            {
                if let (
                    Value::Integer {
                        value: i1,
                        kind: k1,
                    },
                    Value::Integer {
                        value: i2,
                        kind: k2,
                    },
                ) = (&l, &r)
                {
                    let kind = signed_kind_for_bits(int_kind_bits(*k1).max(int_kind_bits(*k2)));
                    let res = match op
                    {
                        BinOpKind::Add => make_signed_int(*i1 + *i2, kind),
                        BinOpKind::Sub => make_signed_int(*i1 - *i2, kind),
                        BinOpKind::Mul => make_signed_int(*i1 * *i2, kind),
                        BinOpKind::Div => make_signed_int(*i1 / *i2, kind),
                        BinOpKind::Pow => pow_signed_int(*i1, *i2, kind),
                        BinOpKind::Eq => Value::Boolean(i1 == i2),
                        BinOpKind::Gt => Value::Boolean(i1 > i2),
                        BinOpKind::Lt => Value::Boolean(i1 < i2),
                    };
                    cache.borrow_mut().hits += 1;
                    return Ok(res);
                }
            }
            BinaryOpCacheKind::Uint =>
            {
                if let (
                    Value::Unsigned {
                        value: u1,
                        kind: k1,
                    },
                    Value::Unsigned {
                        value: u2,
                        kind: k2,
                    },
                ) = (&l, &r)
                {
                    let kind = unsigned_kind_for_bits(int_kind_bits(*k1).max(int_kind_bits(*k2)));
                    let res = match op
                    {
                        BinOpKind::Add => make_unsigned_int(*u1 + *u2, kind),
                        BinOpKind::Sub => make_unsigned_int(*u1 - *u2, kind),
                        BinOpKind::Mul => make_unsigned_int(*u1 * *u2, kind),
                        BinOpKind::Div => make_unsigned_int(*u1 / *u2, kind),
                        BinOpKind::Pow => pow_unsigned_int(*u1, *u2, kind),
                        BinOpKind::Eq => Value::Boolean(u1 == u2),
                        BinOpKind::Gt => Value::Boolean(u1 > u2),
                        BinOpKind::Lt => Value::Boolean(u1 < u2),
                    };
                    cache.borrow_mut().hits += 1;
                    return Ok(res);
                }
            }
        }
    }

    cache.borrow_mut().misses += 1;
    let res = eval_binop(op, l, r)?;
    let new_kind = match (&res, &cached)
    {
        (Value::Float { .. }, _) => Some(BinaryOpCacheKind::Float),
        (Value::Integer { .. }, _) => Some(BinaryOpCacheKind::Int),
        (Value::Unsigned { .. }, _) => Some(BinaryOpCacheKind::Uint),
        _ => None,
    };
    if let Some(kind) = new_kind
    {
        cache.borrow_mut().kind = Some(kind);
    }
    Ok(res)
}
pub(super) fn make_float(value: f64, kind: FloatKind) -> Value
{
    Value::Float {
        value: normalize_float_value(value, kind),
        kind,
    }
}

pub(super) fn reg_binop_from_op(op: &Op) -> Option<RegBinOp>
{
    match op
    {
        Op::Add => Some(RegBinOp::Add),
        Op::Subtract => Some(RegBinOp::Sub),
        Op::Multiply => Some(RegBinOp::Mul),
        Op::Divide => Some(RegBinOp::Div),
        Op::Power => Some(RegBinOp::Pow),
        Op::Equal => Some(RegBinOp::Eq),
        Op::GreaterThan => Some(RegBinOp::Gt),
        Op::LessThan => Some(RegBinOp::Lt),
        _ => None,
    }
}

pub(super) fn int_kind_bits(kind: IntKind) -> u32
{
    match kind
    {
        IntKind::I8 | IntKind::U8 => 8,
        IntKind::I16 | IntKind::U16 => 16,
        IntKind::I32 | IntKind::U32 => 32,
        IntKind::I64 | IntKind::U64 => 64,
        IntKind::I128 | IntKind::U128 => 128,
    }
}

pub(super) fn signed_kind_for_bits(bits: u32) -> IntKind
{
    match bits
    {
        8 => IntKind::I8,
        16 => IntKind::I16,
        32 => IntKind::I32,
        64 => IntKind::I64,
        _ => IntKind::I128,
    }
}

pub(super) fn unsigned_kind_for_bits(bits: u32) -> IntKind
{
    match bits
    {
        8 => IntKind::U8,
        16 => IntKind::U16,
        32 => IntKind::U32,
        64 => IntKind::U64,
        _ => IntKind::U128,
    }
}

pub(super) fn make_signed_int(value: i128, kind: IntKind) -> Value
{
    Value::Integer { value, kind }
}

pub(super) fn make_unsigned_int(value: u128, kind: IntKind) -> Value
{
    Value::Unsigned { value, kind }
}

pub(super) fn cast_f64_value(value: &Value) -> Result<Value, String>
{
    match value
    {
        Value::Float { value, .. } =>
        {
            Ok(Value::Float {
                value: *value,
                kind: FloatKind::F64,
            })
        }
        Value::Integer { value, .. } =>
        {
            Ok(Value::Float {
                value: *value as f64,
                kind: FloatKind::F64,
            })
        }
        Value::Unsigned { value, .. } =>
        {
            Ok(Value::Float {
                value: *value as f64,
                kind: FloatKind::F64,
            })
        }
        Value::String(s) => s
            .parse::<f64>()
            .map(|value| Value::Float {
                value,
                kind: FloatKind::F64,
            })
            .map_err(|_| "f64 expects a numeric string".to_string()),
        _ => Err("f64 expects a number or numeric string".to_string()),
    }
}

pub(super) fn cast_f32_value(value: &Value) -> Result<Value, String>
{
    match value
    {
        Value::Float { value, .. } =>
        {
            Ok(Value::Float {
                value: *value as f32 as f64,
                kind: FloatKind::F32,
            })
        }
        Value::Integer { value, .. } =>
        {
            Ok(Value::Float {
                value: *value as f32 as f64,
                kind: FloatKind::F32,
            })
        }
        Value::Unsigned { value, .. } =>
        {
            Ok(Value::Float {
                value: *value as f32 as f64,
                kind: FloatKind::F32,
            })
        }
        Value::String(s) => s
            .parse::<f32>()
            .map(|value| Value::Float {
                value: value as f64,
                kind: FloatKind::F32,
            })
            .map_err(|_| "f32 expects a numeric string".to_string()),
        _ => Err("f32 expects a number or numeric string".to_string()),
    }
}

pub(super) fn cast_i64_value(value: &Value) -> Result<Value, String>
{
    match value
    {
        Value::Integer { value, .. } => i64::try_from(*value)
            .map(|value| make_signed_int(value as i128, IntKind::I64))
            .map_err(|_| "i64 out of range".to_string()),
        Value::Unsigned { value, .. } => i64::try_from(*value)
            .map(|value| make_signed_int(value as i128, IntKind::I64))
            .map_err(|_| "i64 out of range".to_string()),
        Value::String(s) => s
            .parse::<i64>()
            .map(|value| make_signed_int(value as i128, IntKind::I64))
            .map_err(|_| "i64 expects an integer string".to_string()),
        Value::Float { .. } =>
        {
            Err("i64 does not accept float values (rounding not specified)".to_string())
        }
        _ => Err("i64 expects an integer or integer string".to_string()),
    }
}

pub(super) fn cast_i32_value(value: &Value) -> Result<Value, String>
{
    match value
    {
        Value::Integer { value, .. } => i32::try_from(*value)
            .map(|value| make_signed_int(value as i128, IntKind::I32))
            .map_err(|_| "i32 out of range".to_string()),
        Value::Unsigned { value, .. } => i32::try_from(*value)
            .map(|value| make_signed_int(value as i128, IntKind::I32))
            .map_err(|_| "i32 out of range".to_string()),
        Value::String(s) => s
            .parse::<i32>()
            .map(|value| make_signed_int(value as i128, IntKind::I32))
            .map_err(|_| "i32 expects an integer string".to_string()),
        Value::Float { .. } =>
        {
            Err("i32 does not accept float values (rounding not specified)".to_string())
        }
        _ => Err("i32 expects an integer or integer string".to_string()),
    }
}

pub(super) fn cast_u64_value(value: &Value) -> Result<Value, String>
{
    match value
    {
        Value::Unsigned { value, .. } =>
        {
            Ok(make_unsigned_int(*value, IntKind::U64))
        }
        Value::Integer { value, .. } =>
        {
            if *value < 0
            {
                return Err("u64 out of range".to_string());
            }
            u64::try_from(*value)
                .map(|value| make_unsigned_int(value as u128, IntKind::U64))
                .map_err(|_| "u64 out of range".to_string())
        }
        Value::String(s) => s
            .parse::<u64>()
            .map(|value| make_unsigned_int(value as u128, IntKind::U64))
            .map_err(|_| "u64 expects an unsigned integer string".to_string()),
        Value::Float { .. } =>
        {
            Err("u64 does not accept float values (rounding not specified)".to_string())
        }
        _ => Err("u64 expects an integer or integer string".to_string()),
    }
}

pub(super) fn cast_u32_value(value: &Value) -> Result<Value, String>
{
    match value
    {
        Value::Unsigned { value, .. } =>
        {
            if *value > u32::MAX as u128
            {
                return Err("u32 out of range".to_string());
            }
            Ok(make_unsigned_int(*value, IntKind::U32))
        }
        Value::Integer { value, .. } =>
        {
            if *value < 0
            {
                return Err("u32 out of range".to_string());
            }
            u32::try_from(*value)
                .map(|value| make_unsigned_int(value as u128, IntKind::U32))
                .map_err(|_| "u32 out of range".to_string())
        }
        Value::String(s) => s
            .parse::<u32>()
            .map(|value| make_unsigned_int(value as u128, IntKind::U32))
            .map_err(|_| "u32 expects an unsigned integer string".to_string()),
        Value::Float { .. } =>
        {
            Err("u32 does not accept float values (rounding not specified)".to_string())
        }
        _ => Err("u32 expects an integer or integer string".to_string()),
    }
}

pub(super) fn int_value_as_f64(value: &Value) -> Option<f64>
{
    match value
    {
        Value::Integer { value, .. } => Some(*value as f64),
        Value::Unsigned { value, .. } => Some(*value as f64),
        Value::Float { value, .. } => Some(*value),
        _ => None,
    }
}

pub(super) fn int_value_as_i64(value: &Value) -> Option<i64>
{
    match value
    {
        Value::Integer { value, .. } => i64::try_from(*value).ok(),
        Value::Unsigned { value, .. } => i64::try_from(*value).ok(),
        _ => None,
    }
}

pub(super) fn default_int(value: i128) -> Value
{
    make_signed_int(value, IntKind::I64)
}

pub(super) fn int_value_as_usize(value: &Value) -> Option<usize>
{
    match value
    {
        Value::Integer { value, .. } if *value >= 0 => usize::try_from(*value).ok(),
        Value::Unsigned { value, .. } => usize::try_from(*value).ok(),
        _ => None,
    }
}

pub(super) fn number_to_usize(value: &Value) -> Option<usize>
{
    match value
    {
        Value::Float { value, .. } if *value >= 0.0 => Some(*value as usize),
        _ => int_value_as_usize(value),
    }
}
