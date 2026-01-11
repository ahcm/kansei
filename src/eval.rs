use crate::ast::{Closure, Expr, ExprKind, FloatKind, IntKind, Op};
use crate::intern;
use crate::intern::{SymbolId, symbol_name};
use crate::value::{Builtin, Environment, Instruction, RangeEnd, Value};
use crate::wasm::{WasmFunction, WasmModule};
use rustc_hash::FxHashMap;
use std::collections::HashSet;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::Command;
use std::rc::Rc;
use std::cell::RefCell;
use std::simd::Simd;
use std::simd::num::SimdFloat;
use wasmi::{Value as WasmValue};
use wasmi::core::ValueType;

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub line: usize,
}

pub type EvalResult = Result<Value, RuntimeError>;

fn native_int64_parse(args: &[Value]) -> Result<Value, String> {
    parse_signed_int(args, IntKind::I64, "Int64")
}

fn native_float64_parse(args: &[Value]) -> Result<Value, String> {
    let arg = args.get(0).ok_or_else(|| "Float64.parse expects 1 argument".to_string())?;
    match arg {
        Value::String(s) => s.parse::<f64>()
            .map(|value| make_float(value, FloatKind::F64))
            .map_err(|_| "Float64.parse failed to parse string".to_string()),
        _ => Err("Float64.parse expects a string argument".to_string()),
    }
}

fn native_float64_sqrt(args: &[Value]) -> Result<Value, String> {
    let arg = args.get(0).ok_or_else(|| "Float64.sqrt expects 1 argument".to_string())?;
    let value = match arg {
        Value::Float { value, .. } => *value,
        v => int_value_as_f64(v).ok_or_else(|| "Float64.sqrt expects a number".to_string())?,
    };
    Ok(make_float(value.sqrt(), FloatKind::F64))
}

fn native_float32_parse(args: &[Value]) -> Result<Value, String> {
    let arg = args.get(0).ok_or_else(|| "Float32.parse expects 1 argument".to_string())?;
    match arg {
        Value::String(s) => s.parse::<f32>()
            .map(|value| make_float(value as f64, FloatKind::F32))
            .map_err(|_| "Float32.parse failed to parse string".to_string()),
        _ => Err("Float32.parse expects a string argument".to_string()),
    }
}

fn native_float32_sqrt(args: &[Value]) -> Result<Value, String> {
    let arg = args.get(0).ok_or_else(|| "Float32.sqrt expects 1 argument".to_string())?;
    let value = match arg {
        Value::Float { value, .. } => *value,
        v => int_value_as_f64(v).ok_or_else(|| "Float32.sqrt expects a number".to_string())?,
    };
    Ok(make_float((value as f32).sqrt() as f64, FloatKind::F32))
}

fn native_float128_parse(args: &[Value]) -> Result<Value, String> {
    let arg = args.get(0).ok_or_else(|| "Float128.parse expects 1 argument".to_string())?;
    match arg {
        Value::String(s) => s.parse::<f64>()
            .map(|value| make_float(value, FloatKind::F128))
            .map_err(|_| "Float128.parse failed to parse string".to_string()),
        _ => Err("Float128.parse expects a string argument".to_string()),
    }
}

fn native_float128_sqrt(args: &[Value]) -> Result<Value, String> {
    let arg = args.get(0).ok_or_else(|| "Float128.sqrt expects 1 argument".to_string())?;
    let value = match arg {
        Value::Float { value, .. } => *value,
        v => int_value_as_f64(v).ok_or_else(|| "Float128.sqrt expects a number".to_string())?,
    };
    Ok(make_float(value.sqrt(), FloatKind::F128))
}

fn signed_int_min(kind: IntKind) -> i128 {
    match kind {
        IntKind::I8 => i8::MIN as i128,
        IntKind::I16 => i16::MIN as i128,
        IntKind::I32 => i32::MIN as i128,
        IntKind::I64 => i64::MIN as i128,
        IntKind::I128 => i128::MIN,
        _ => panic!("Expected signed int kind, got {:?}", kind),
    }
}

fn signed_int_max(kind: IntKind) -> i128 {
    match kind {
        IntKind::I8 => i8::MAX as i128,
        IntKind::I16 => i16::MAX as i128,
        IntKind::I32 => i32::MAX as i128,
        IntKind::I64 => i64::MAX as i128,
        IntKind::I128 => i128::MAX,
        _ => panic!("Expected signed int kind, got {:?}", kind),
    }
}

fn unsigned_int_max(kind: IntKind) -> u128 {
    match kind {
        IntKind::U8 => u8::MAX as u128,
        IntKind::U16 => u16::MAX as u128,
        IntKind::U32 => u32::MAX as u128,
        IntKind::U64 => u64::MAX as u128,
        IntKind::U128 => u128::MAX,
        _ => panic!("Expected unsigned int kind, got {:?}", kind),
    }
}

fn parse_signed_int(args: &[Value], kind: IntKind, label: &str) -> Result<Value, String> {
    let arg = args.get(0).ok_or_else(|| format!("{}.parse expects 1 argument", label))?;
    let s = match arg {
        Value::String(s) => s.as_str(),
        _ => return Err(format!("{}.parse expects a string argument", label)),
    };
    let value = s.parse::<i128>().map_err(|_| format!("{}.parse failed to parse string", label))?;
    let min = signed_int_min(kind);
    let max = signed_int_max(kind);
    if value < min || value > max {
        return Err(format!("{}.parse out of range for {:?}", label, kind));
    }
    Ok(make_signed_int(value, kind))
}

fn parse_unsigned_int(args: &[Value], kind: IntKind, label: &str) -> Result<Value, String> {
    let arg = args.get(0).ok_or_else(|| format!("{}.parse expects 1 argument", label))?;
    let s = match arg {
        Value::String(s) => s.as_str(),
        _ => return Err(format!("{}.parse expects a string argument", label)),
    };
    let value = s.parse::<u128>().map_err(|_| format!("{}.parse failed to parse string", label))?;
    let max = unsigned_int_max(kind);
    if value > max {
        return Err(format!("{}.parse out of range for {:?}", label, kind));
    }
    Ok(make_unsigned_int(value, kind))
}

fn native_int8_parse(args: &[Value]) -> Result<Value, String> { parse_signed_int(args, IntKind::I8, "Int8") }
fn native_int16_parse(args: &[Value]) -> Result<Value, String> { parse_signed_int(args, IntKind::I16, "Int16") }
fn native_int32_parse(args: &[Value]) -> Result<Value, String> { parse_signed_int(args, IntKind::I32, "Int32") }
fn native_int128_parse(args: &[Value]) -> Result<Value, String> { parse_signed_int(args, IntKind::I128, "Int128") }

fn native_uint8_parse(args: &[Value]) -> Result<Value, String> { parse_unsigned_int(args, IntKind::U8, "Uint8") }
fn native_uint16_parse(args: &[Value]) -> Result<Value, String> { parse_unsigned_int(args, IntKind::U16, "Uint16") }
fn native_uint32_parse(args: &[Value]) -> Result<Value, String> { parse_unsigned_int(args, IntKind::U32, "Uint32") }
fn native_uint64_parse(args: &[Value]) -> Result<Value, String> { parse_unsigned_int(args, IntKind::U64, "Uint64") }
fn native_uint128_parse(args: &[Value]) -> Result<Value, String> { parse_unsigned_int(args, IntKind::U128, "Uint128") }

fn build_int64_module() -> Value {
    let mut int64_map = FxHashMap::default();
    int64_map.insert(intern::intern("parse"), Value::NativeFunction(native_int64_parse));
    Value::Map(Rc::new(RefCell::new(int64_map)))
}

fn build_int8_module() -> Value {
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_int8_parse));
    Value::Map(Rc::new(RefCell::new(map)))
}

fn build_int16_module() -> Value {
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_int16_parse));
    Value::Map(Rc::new(RefCell::new(map)))
}

fn build_int32_module() -> Value {
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_int32_parse));
    Value::Map(Rc::new(RefCell::new(map)))
}

fn build_int128_module() -> Value {
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_int128_parse));
    Value::Map(Rc::new(RefCell::new(map)))
}

fn build_uint8_module() -> Value {
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint8_parse));
    Value::Map(Rc::new(RefCell::new(map)))
}

fn build_uint16_module() -> Value {
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint16_parse));
    Value::Map(Rc::new(RefCell::new(map)))
}

fn build_uint32_module() -> Value {
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint32_parse));
    Value::Map(Rc::new(RefCell::new(map)))
}

fn build_uint64_module() -> Value {
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint64_parse));
    Value::Map(Rc::new(RefCell::new(map)))
}

fn build_uint128_module() -> Value {
    let mut map = FxHashMap::default();
    map.insert(intern::intern("parse"), Value::NativeFunction(native_uint128_parse));
    Value::Map(Rc::new(RefCell::new(map)))
}

fn build_float32_module() -> Value {
    let mut float32_map = FxHashMap::default();
    float32_map.insert(intern::intern("parse"), Value::NativeFunction(native_float32_parse));
    float32_map.insert(intern::intern("sqrt"), Value::NativeFunction(native_float32_sqrt));
    Value::Map(Rc::new(RefCell::new(float32_map)))
}

fn build_float64_module() -> Value {
    let mut float64_map = FxHashMap::default();
    float64_map.insert(intern::intern("parse"), Value::NativeFunction(native_float64_parse));
    float64_map.insert(intern::intern("sqrt"), Value::NativeFunction(native_float64_sqrt));
    Value::Map(Rc::new(RefCell::new(float64_map)))
}

fn build_float128_module() -> Value {
    let mut float128_map = FxHashMap::default();
    float128_map.insert(intern::intern("parse"), Value::NativeFunction(native_float128_parse));
    float128_map.insert(intern::intern("sqrt"), Value::NativeFunction(native_float128_sqrt));
    Value::Map(Rc::new(RefCell::new(float128_map)))
}

fn build_std_module() -> Value {
    let mut std_map = FxHashMap::default();
    std_map.insert(intern::intern("Int8"), build_int8_module());
    std_map.insert(intern::intern("Int16"), build_int16_module());
    std_map.insert(intern::intern("Int32"), build_int32_module());
    std_map.insert(intern::intern("Int64"), build_int64_module());
    std_map.insert(intern::intern("Int128"), build_int128_module());
    std_map.insert(intern::intern("Uint8"), build_uint8_module());
    std_map.insert(intern::intern("Uint16"), build_uint16_module());
    std_map.insert(intern::intern("Uint32"), build_uint32_module());
    std_map.insert(intern::intern("Uint64"), build_uint64_module());
    std_map.insert(intern::intern("Uint128"), build_uint128_module());
    std_map.insert(intern::intern("Float32"), build_float32_module());
    std_map.insert(intern::intern("Float64"), build_float64_module());
    std_map.insert(intern::intern("Float128"), build_float128_module());
    Value::Map(Rc::new(RefCell::new(std_map)))
}

fn normalize_float_value(value: f64, kind: FloatKind) -> f64 {
    match kind {
        FloatKind::F32 => (value as f32) as f64,
        FloatKind::F64 | FloatKind::F128 => value,
    }
}

fn promote_float_kind(left: FloatKind, right: FloatKind) -> FloatKind {
    let rank = |kind| match kind {
        FloatKind::F32 => 0,
        FloatKind::F64 => 1,
        FloatKind::F128 => 2,
    };
    if rank(left) >= rank(right) { left } else { right }
}

fn make_float(value: f64, kind: FloatKind) -> Value {
    Value::Float { value: normalize_float_value(value, kind), kind }
}

fn int_kind_bits(kind: IntKind) -> u32 {
    match kind {
        IntKind::I8 | IntKind::U8 => 8,
        IntKind::I16 | IntKind::U16 => 16,
        IntKind::I32 | IntKind::U32 => 32,
        IntKind::I64 | IntKind::U64 => 64,
        IntKind::I128 | IntKind::U128 => 128,
    }
}

fn signed_kind_for_bits(bits: u32) -> IntKind {
    match bits {
        8 => IntKind::I8,
        16 => IntKind::I16,
        32 => IntKind::I32,
        64 => IntKind::I64,
        _ => IntKind::I128,
    }
}

fn unsigned_kind_for_bits(bits: u32) -> IntKind {
    match bits {
        8 => IntKind::U8,
        16 => IntKind::U16,
        32 => IntKind::U32,
        64 => IntKind::U64,
        _ => IntKind::U128,
    }
}

fn make_signed_int(value: i128, kind: IntKind) -> Value {
    Value::Integer { value, kind }
}

fn make_unsigned_int(value: u128, kind: IntKind) -> Value {
    Value::Unsigned { value, kind }
}

fn int_value_as_f64(value: &Value) -> Option<f64> {
    match value {
        Value::Integer { value, .. } => Some(*value as f64),
        Value::Unsigned { value, .. } => Some(*value as f64),
        _ => None,
    }
}

fn int_value_as_i64(value: &Value) -> Option<i64> {
    match value {
        Value::Integer { value, .. } => i64::try_from(*value).ok(),
        Value::Unsigned { value, .. } => i64::try_from(*value).ok(),
        _ => None,
    }
}

fn default_int(value: i128) -> Value {
    make_signed_int(value, IntKind::I64)
}

fn int_value_as_usize(value: &Value) -> Option<usize> {
    match value {
        Value::Integer { value, .. } if *value >= 0 => usize::try_from(*value).ok(),
        Value::Unsigned { value, .. } => usize::try_from(*value).ok(),
        _ => None,
    }
}

fn number_to_usize(value: &Value) -> Option<usize> {
    match value {
        Value::Float { value, .. } if *value >= 0.0 => Some(*value as usize),
        _ => int_value_as_usize(value),
    }
}


fn collect_declarations(expr: &Expr, decls: &mut HashSet<SymbolId>) {
        match &expr.kind {
            ExprKind::Assignment { name, .. } => {
                decls.insert(name.clone());
            }
            ExprKind::FunctionDef { name, .. } => {
            decls.insert(name.clone());
        }
        ExprKind::AnonymousFunction { .. } => {
        }
        ExprKind::IndexAssignment { target, index, value } => {
            collect_declarations(target, decls);
            collect_declarations(index, decls);
            collect_declarations(value, decls);
        }
        ExprKind::Block(stmts) => {
            for stmt in stmts {
                collect_declarations(stmt, decls);
            }
        }
        ExprKind::If { then_branch, else_branch, .. } => {
            collect_declarations(then_branch, decls);
            if let Some(else_expr) = else_branch {
                collect_declarations(else_expr, decls);
            }
        }
        ExprKind::While { body, .. } => {
            collect_declarations(body, decls);
        }
        ExprKind::For { var, iterable, body, .. } => {
            decls.insert(var.clone());
            collect_declarations(iterable, decls);
            collect_declarations(body, decls);
        }
        ExprKind::Loop { var, count, body, .. } => {
            if let Some(name) = var {
                decls.insert(name.clone());
            }
            collect_declarations(count, decls);
            collect_declarations(body, decls);
        }
        ExprKind::Array(elements) => {
            for e in elements {
                collect_declarations(e, decls);
            }
        }
        ExprKind::ArrayGenerator { generator, size } => {
            collect_declarations(generator, decls);
            collect_declarations(size, decls);
        }
        ExprKind::Map(entries) => {
            for (k, v) in entries {
                collect_declarations(k, decls);
                collect_declarations(v, decls);
            }
        }
        ExprKind::FormatString(parts) => {
            for part in parts {
                if let crate::ast::FormatPart::Expr { expr, .. } = part {
                    collect_declarations(expr, decls);
                }
            }
        }
        ExprKind::Load(_) => {}
        _ => {}
    }
}

fn build_slot_map(params: &[(SymbolId, bool)], locals: HashSet<SymbolId>) -> (FxHashMap<SymbolId, usize>, Vec<Rc<String>>) {
    let mut slot_map = FxHashMap::default();
    let mut slot_names = Vec::new();
    for (p, _) in params {
        if !slot_map.contains_key(p) {
            slot_map.insert(p.clone(), slot_names.len());
            slot_names.push(symbol_name(*p));
        }
    }
    for l in locals {
        if !slot_map.contains_key(&l) {
            slot_map.insert(l.clone(), slot_names.len());
            slot_names.push(symbol_name(l));
        }
    }
    (slot_map, slot_names)
}

pub fn resolve_slots(expr: &mut Expr) {
    resolve_functions(expr);
}

fn resolve_functions(expr: &mut Expr) {
    match &mut expr.kind {
        ExprKind::FunctionDef { params, body, slots, .. } => {
            if slots.is_none() {
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);
                let (slot_map, slot_names) = build_slot_map(params, locals);
                resolve(body.as_mut(), &slot_map);
                *slots = Some(Rc::new(slot_names));
            }
            resolve_functions(body);
        }
        ExprKind::AnonymousFunction { params, body, slots } => {
            if slots.is_none() {
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);
                let (slot_map, slot_names) = build_slot_map(params, locals);
                resolve(body.as_mut(), &slot_map);
                *slots = Some(Rc::new(slot_names));
            }
            resolve_functions(body);
        }
        ExprKind::Assignment { value, .. } => resolve_functions(value),
        ExprKind::IndexAssignment { target, index, value } => {
            resolve_functions(target);
            resolve_functions(index);
            resolve_functions(value);
        }
        ExprKind::BinaryOp { left, right, .. } => {
            resolve_functions(left);
            resolve_functions(right);
        }
        ExprKind::Block(stmts) => {
            for stmt in stmts {
                resolve_functions(stmt);
            }
        }
        ExprKind::If { condition, then_branch, else_branch } => {
            resolve_functions(condition);
            resolve_functions(then_branch);
            if let Some(eb) = else_branch {
                resolve_functions(eb);
            }
        }
        ExprKind::While { condition, body } => {
            resolve_functions(condition);
            resolve_functions(body);
        }
        ExprKind::For { iterable, body, .. } => {
            resolve_functions(iterable);
            resolve_functions(body);
        }
        ExprKind::Loop { count, body, .. } => {
            resolve_functions(count);
            resolve_functions(body);
        }
        ExprKind::Call { function, args, block, .. } => {
            resolve_functions(function);
            for arg in args {
                resolve_functions(arg);
            }
            if let Some(c) = block {
                resolve_functions(&mut c.body);
            }
        }
        ExprKind::Array(elements) => {
            for e in elements {
                resolve_functions(e);
            }
        }
        ExprKind::ArrayGenerator { generator, size } => {
            resolve_functions(generator);
            resolve_functions(size);
        }
        ExprKind::Map(entries) => {
            for (k, v) in entries {
                resolve_functions(k);
                resolve_functions(v);
            }
        }
        ExprKind::Index { target, index } => {
            resolve_functions(target);
            resolve_functions(index);
        }
        ExprKind::FormatString(parts) => {
            for part in parts {
                if let crate::ast::FormatPart::Expr { expr, .. } = part {
                    resolve_functions(expr);
                }
            }
        }
        ExprKind::Use(_) => {}
        ExprKind::Load(_) => {}
        ExprKind::Yield(args) => {
            for a in args {
                resolve_functions(a);
            }
        }
        _ => {}
    }
}

fn uses_environment(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Identifier { slot: None, .. } => true,
        ExprKind::Identifier { .. } => false,
        ExprKind::BinaryOp { left, right, .. } => uses_environment(left) || uses_environment(right),
        ExprKind::If { condition, then_branch, else_branch } => {
            uses_environment(condition) || uses_environment(then_branch) || else_branch.as_ref().map_or(false, |e| uses_environment(e))
        }
        ExprKind::Call { function, args, .. } => uses_environment(function) || args.iter().any(uses_environment),
        ExprKind::Use(_) => true,
        ExprKind::Load(_) => true,
        ExprKind::FormatString(parts) => parts.iter().any(|part| {
            if let crate::ast::FormatPart::Expr { expr, .. } = part {
                uses_environment(expr)
            } else {
                false
            }
        }),
        // Simple functions (is_simple) only have these constructs roughly. 
        // We can be conservative.
        ExprKind::Integer { .. } | ExprKind::Unsigned { .. } | ExprKind::Float { .. } | ExprKind::String(_) | ExprKind::Boolean(_) | ExprKind::Nil => false,
        _ => true, // Conservative fallback for blocks, loops, etc. if they slipped into is_simple
    }
}

fn builtin_from_symbol(name: SymbolId) -> Option<Builtin> {
    match symbol_name(name).as_str() {
        "puts" => Some(Builtin::Puts),
        "print" => Some(Builtin::Print),
        "len" => Some(Builtin::Len),
        "read_file" => Some(Builtin::ReadFile),
        "write_file" => Some(Builtin::WriteFile),
        _ => None,
    }
}

fn push_const(consts: &mut Vec<Value>, value: Value) -> usize {
    if let Some(idx) = consts.iter().position(|v| v == &value) {
        return idx;
    }
    consts.push(value);
    consts.len() - 1
}

fn match_for_range(condition: &Expr, body: &Expr, consts: &mut Vec<Value>) -> Option<(usize, RangeEnd, Expr)> {
    let (index_slot, end) = match &condition.kind {
        ExprKind::BinaryOp { left, op: Op::LessThan, right } => {
            let idx_slot = match &left.kind {
                ExprKind::Identifier { slot: Some(s), .. } => *s,
                _ => return None,
            };
            let end = match &right.kind {
                ExprKind::Identifier { slot: Some(s), .. } => RangeEnd::Slot(*s),
                ExprKind::Integer { value, kind } => {
                    let idx = push_const(consts, make_signed_int(*value, *kind));
                    RangeEnd::Const(idx)
                }
                ExprKind::Unsigned { value, kind } => {
                    let idx = push_const(consts, make_unsigned_int(*value, *kind));
                    RangeEnd::Const(idx)
                }
                ExprKind::Float { value, kind } => {
                    let idx = push_const(consts, make_float(*value, *kind));
                    RangeEnd::Const(idx)
                }
                _ => return None,
            };
            (idx_slot, end)
        }
        _ => return None,
    };

    let (stmts, line) = match &body.kind {
        ExprKind::Block(stmts) => (stmts, body.line),
        _ => return None,
    };
    if stmts.is_empty() {
        return None;
    }
    let (body_stmts, increment) = stmts.split_at(stmts.len() - 1);
    let inc_stmt = &increment[0];
    match &inc_stmt.kind {
        ExprKind::Assignment { slot: Some(s), value, .. } if *s == index_slot => {
            match &value.kind {
                ExprKind::BinaryOp { left, op: Op::Add, right } => {
                    let left_slot = match &left.kind {
                        ExprKind::Identifier { slot: Some(ls), .. } => *ls,
                        _ => return None,
                    };
                    if left_slot != index_slot {
                        return None;
                    }
                    let is_one = matches!(right.kind, ExprKind::Integer { value: 1, .. } | ExprKind::Unsigned { value: 1, .. } | ExprKind::Float { value: 1.0, .. });
                    if !is_one {
                        return None;
                    }
                }
                _ => return None,
            }
        }
        _ => return None,
    }

    let body_expr = Expr {
        kind: ExprKind::Block(body_stmts.to_vec()),
        line,
    };
    Some((index_slot, end, body_expr))
}

fn match_dot_assign(stmt: &Expr, index_slot: usize) -> Option<(usize, usize, usize)> {
    let (acc_slot, value) = match &stmt.kind {
        ExprKind::Assignment { slot: Some(s), value, .. } => (*s, value.as_ref()),
        _ => return None,
    };
    let (add_left, add_right) = match &value.kind {
        ExprKind::BinaryOp { left, op: Op::Add, right } => (left.as_ref(), right.as_ref()),
        _ => return None,
    };
    let is_acc = |expr: &Expr| matches!(expr.kind, ExprKind::Identifier { slot: Some(s), .. } if s == acc_slot);
    let mul_expr = if is_acc(add_left) {
        add_right
    } else if is_acc(add_right) {
        add_left
    } else {
        return None;
    };
    let (a_slot, b_slot) = match &mul_expr.kind {
        ExprKind::BinaryOp { left, op: Op::Multiply, right } => {
            let (a_slot, a_idx) = match_f64_index(left)?;
            let (b_slot, b_idx) = match_f64_index(right)?;
            if a_idx != index_slot || b_idx != index_slot {
                return None;
            }
            (a_slot, b_slot)
        }
        _ => return None,
    };
    Some((acc_slot, a_slot, b_slot))
}

fn match_dot_range_body(body: &Expr, index_slot: usize) -> Option<(usize, usize, usize)> {
    let stmt = match &body.kind {
        ExprKind::Block(stmts) if stmts.len() == 1 => &stmts[0],
        _ => body,
    };
    match_dot_assign(stmt, index_slot)
}

fn match_dot2_range_body(body: &Expr, index_slot: usize) -> Option<(usize, usize, usize, usize, usize, usize)> {
    let stmts = match &body.kind {
        ExprKind::Block(stmts) if stmts.len() == 2 => stmts,
        _ => return None,
    };
    let (acc1, a1, b1) = match_dot_assign(&stmts[0], index_slot)?;
    let (acc2, a2, b2) = match_dot_assign(&stmts[1], index_slot)?;
    if acc1 == acc2 {
        return None;
    }
    Some((acc1, a1, b1, acc2, a2, b2))
}

fn match_range_end(expr: &Expr, consts: &mut Vec<Value>) -> Option<RangeEnd> {
    match &expr.kind {
        ExprKind::Identifier { slot: Some(s), .. } => Some(RangeEnd::Slot(*s)),
        ExprKind::Integer { value, kind } => {
            let idx = push_const(consts, make_signed_int(*value, *kind));
            Some(RangeEnd::Const(idx))
        }
        ExprKind::Unsigned { value, kind } => {
            let idx = push_const(consts, make_unsigned_int(*value, *kind));
            Some(RangeEnd::Const(idx))
        }
        ExprKind::Float { value, kind } => {
            let idx = push_const(consts, make_float(*value, *kind));
            Some(RangeEnd::Const(idx))
        }
        _ => None,
    }
}

fn is_pure_expr(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Integer { .. } | ExprKind::Unsigned { .. } | ExprKind::Float { .. } | ExprKind::Boolean(_) | ExprKind::Nil => true,
        ExprKind::Identifier { .. } => true,
        ExprKind::BinaryOp { left, right, .. } => is_pure_expr(left) && is_pure_expr(right),
        _ => false,
    }
}

fn match_f64_index(expr: &Expr) -> Option<(usize, usize)> {
    match &expr.kind {
        ExprKind::Index { target, index } => {
            let target_slot = match &target.kind {
                ExprKind::Identifier { slot: Some(s), .. } => *s,
                _ => return None,
            };
            let index_slot = match &index.kind {
                ExprKind::Identifier { slot: Some(s), .. } => *s,
                _ => return None,
            };
            Some((target_slot, index_slot))
        }
        _ => None,
    }
}

fn match_f64_mul(expr: &Expr) -> Option<(Expr, usize, usize)> {
    match &expr.kind {
        ExprKind::BinaryOp { left, op: Op::Multiply, right } => {
            if let Some((src_slot, src_index_slot)) = match_f64_index(left) {
                return Some(((*right.as_ref()).clone(), src_slot, src_index_slot));
            }
            if let Some((src_slot, src_index_slot)) = match_f64_index(right) {
                return Some(((*left.as_ref()).clone(), src_slot, src_index_slot));
            }
            None
        }
        _ => None,
    }
}

fn match_f64_axpy(target: &Expr, value: &Expr) -> Option<(usize, usize, usize, usize, Expr)> {
    let (dst_slot, dst_index_slot) = match_f64_index(target)?;
    let (add_left, add_right) = match &value.kind {
        ExprKind::BinaryOp { left, op: Op::Add, right } => (left.as_ref(), right.as_ref()),
        _ => return None,
    };
    if let Some((slot, idx)) = match_f64_index(add_left) {
        if slot == dst_slot && idx == dst_index_slot {
            if let Some((scalar, src_slot, src_index_slot)) = match_f64_mul(add_right) {
                return Some((dst_slot, dst_index_slot, src_slot, src_index_slot, scalar));
            }
        }
    }
    if let Some((slot, idx)) = match_f64_index(add_right) {
        if slot == dst_slot && idx == dst_index_slot {
            if let Some((scalar, src_slot, src_index_slot)) = match_f64_mul(add_left) {
                return Some((dst_slot, dst_index_slot, src_slot, src_index_slot, scalar));
            }
        }
    }
    None
}

fn compile_expr(expr: &Expr, code: &mut Vec<Instruction>, consts: &mut Vec<Value>, want_value: bool) -> bool {
    match &expr.kind {
        ExprKind::Integer { value, kind } => {
            if want_value {
                let idx = push_const(consts, make_signed_int(*value, *kind));
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Unsigned { value, kind } => {
            if want_value {
                let idx = push_const(consts, make_unsigned_int(*value, *kind));
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Float { value, kind } => {
            if want_value {
                let idx = push_const(consts, make_float(*value, *kind));
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Identifier { slot: Some(s), .. } => {
            if want_value {
                code.push(Instruction::LoadSlot(*s));
            }
        }
        ExprKind::Boolean(b) => {
            if want_value {
                let idx = push_const(consts, Value::Boolean(*b));
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Nil => {
            if want_value {
                let idx = push_const(consts, Value::Nil);
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Assignment { value, slot: Some(s), .. } => {
            if !compile_expr(value, code, consts, true) { return false; }
            code.push(Instruction::StoreSlot(*s));
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Call { function, args, block, .. } => {
            if block.is_some() {
                return false;
            }
            if let ExprKind::Identifier { name, .. } = &function.kind {
                if let Some(builtin) = builtin_from_symbol(*name) {
                    for arg in args {
                        if !compile_expr(arg, code, consts, true) { return false; }
                    }
                    code.push(Instruction::CallBuiltin(builtin, args.len()));
                    if !want_value {
                        code.push(Instruction::Pop);
                    }
                    return true;
                }
            }
            if !compile_expr(function, code, consts, true) { return false; }
            for arg in args {
                if !compile_expr(arg, code, consts, true) { return false; }
            }
            code.push(Instruction::CallValue(args.len()));
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Index { target, index } => {
            if !compile_expr(target, code, consts, true) { return false; }
            if !compile_expr(index, code, consts, true) { return false; }
            code.push(Instruction::Index);
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::IndexAssignment { target, index, value } => {
            if let Some((dst_slot, dst_index_slot, src_slot, src_index_slot, scalar)) =
                match_f64_axpy(target, value)
            {
                if !compile_expr(&scalar, code, consts, true) { return false; }
                code.push(Instruction::F64Axpy { dst_slot, dst_index_slot, src_slot, src_index_slot });
            } else {
                if !compile_expr(target, code, consts, true) { return false; }
                if !compile_expr(index, code, consts, true) { return false; }
                if !compile_expr(value, code, consts, true) { return false; }
                code.push(Instruction::IndexAssign);
            }
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Use(_) => {
            return false;
        }
        ExprKind::Load(_) => {
            return false;
        }
        ExprKind::FormatString(_) => {
            return false;
        }
        ExprKind::BinaryOp { left, op, right } => {
            let mut handled = false;
            if *op == Op::Multiply {
                let one = |expr: &Expr| matches!(expr.kind, ExprKind::Integer { value: 1, .. } | ExprKind::Unsigned { value: 1, .. } | ExprKind::Float { value: 1.0, .. });
                if is_pure_expr(left) {
                    match &right.kind {
                        ExprKind::BinaryOp { left: r_left, op: Op::Add, right: r_right } => {
                            if (r_left.as_ref() == left.as_ref() && one(r_right))
                                || (r_right.as_ref() == left.as_ref() && one(r_left))
                            {
                                if !compile_expr(left, code, consts, true) { return false; }
                                code.push(Instruction::Dup);
                                let idx = push_const(consts, default_int(1));
                                code.push(Instruction::LoadConstIdx(idx));
                                code.push(Instruction::Add);
                                code.push(Instruction::Mul);
                                handled = true;
                            }
                        }
                        _ => {}
                    }
                }
                if !handled && is_pure_expr(right) {
                    match &left.kind {
                        ExprKind::BinaryOp { left: l_left, op: Op::Add, right: l_right } => {
                            if (l_left.as_ref() == right.as_ref() && one(l_right))
                                || (l_right.as_ref() == right.as_ref() && one(l_left))
                            {
                                if !compile_expr(right, code, consts, true) { return false; }
                                code.push(Instruction::Dup);
                                let idx = push_const(consts, default_int(1));
                                code.push(Instruction::LoadConstIdx(idx));
                                code.push(Instruction::Add);
                                code.push(Instruction::Mul);
                                handled = true;
                            }
                        }
                        _ => {}
                    }
                }
            }
            if !handled {
                if !compile_expr(left, code, consts, true) { return false; }
                if !compile_expr(right, code, consts, true) { return false; }
                match op {
                    Op::Add => code.push(Instruction::Add),
                    Op::Subtract => code.push(Instruction::Sub),
                    Op::Multiply => code.push(Instruction::Mul),
                    Op::Divide => code.push(Instruction::Div),
                    Op::Equal => code.push(Instruction::Eq),
                    Op::GreaterThan => code.push(Instruction::Gt),
                    Op::LessThan => code.push(Instruction::Lt),
                    _ => return false,
                }
            }
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Block(stmts) => {
            if stmts.is_empty() {
                if want_value {
                    let idx = push_const(consts, Value::Nil);
                    code.push(Instruction::LoadConstIdx(idx));
                }
            } else {
                let last_idx = stmts.len() - 1;
                for (idx, stmt) in stmts.iter().enumerate() {
                    let is_last = idx == last_idx;
                    if !compile_expr(stmt, code, consts, is_last && want_value) { return false; }
                }
            }
        }
        ExprKind::If { condition, then_branch, else_branch } => {
            if !compile_expr(condition, code, consts, true) { return false; }
            let jump_if_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            if !compile_expr(then_branch, code, consts, want_value) { return false; }
            let jump_end_idx = code.len();
            code.push(Instruction::Jump(usize::MAX));
            let else_target = code.len();
            if let Some(else_expr) = else_branch {
                if !compile_expr(else_expr, code, consts, want_value) { return false; }
            } else {
                if want_value {
                    let idx = push_const(consts, Value::Nil);
                    code.push(Instruction::LoadConstIdx(idx));
                }
            }
            let end_target = code.len();
            code[jump_if_false_idx] = Instruction::JumpIfFalse(else_target);
            code[jump_end_idx] = Instruction::Jump(end_target);
        }
        ExprKind::While { condition, body } => {
            if !want_value {
                if let Some((index_slot, end, body_expr)) = match_for_range(condition, body, consts) {
                    if let Some((acc1, a1, b1, acc2, a2, b2)) = match_dot2_range_body(&body_expr, index_slot) {
                        code.push(Instruction::F64Dot2Range {
                            acc1_slot: acc1,
                            a1_slot: a1,
                            b1_slot: b1,
                            acc2_slot: acc2,
                            a2_slot: a2,
                            b2_slot: b2,
                            index_slot,
                            end,
                        });
                        code.push(Instruction::Pop);
                        return true;
                    } else if let Some((acc_slot, a_slot, b_slot)) = match_dot_range_body(&body_expr, index_slot) {
                        code.push(Instruction::F64DotRange { acc_slot, a_slot, b_slot, index_slot, end });
                        code.push(Instruction::Pop);
                        return true;
                    } else {
                        let mut body_code = Vec::new();
                        if !compile_expr(&body_expr, &mut body_code, consts, true) { return false; }
                        code.push(Instruction::ForRange { index_slot, end, body: Rc::new(body_code) });
                        code.push(Instruction::Pop);
                        return true;
                    }
                }
            }
            if want_value {
                let nil_idx = push_const(consts, Value::Nil);
                code.push(Instruction::LoadConstIdx(nil_idx));
            }
            let loop_start = code.len();
            if !compile_expr(condition, code, consts, true) { return false; }
            let jump_if_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            if want_value {
                code.push(Instruction::Pop);
                if !compile_expr(body, code, consts, true) { return false; }
            } else {
                if !compile_expr(body, code, consts, false) { return false; }
            }
            code.push(Instruction::Jump(loop_start));
            let loop_end = code.len();
            code[jump_if_false_idx] = Instruction::JumpIfFalse(loop_end);
        }
        ExprKind::For { var_slot: Some(var_slot), iterable, body, .. } => {
            if !compile_expr(iterable, code, consts, true) { return false; }
            let mut body_code = Vec::new();
            if !compile_expr(body, &mut body_code, consts, true) { return false; }
            code.push(Instruction::ForEach { var_slot: *var_slot, body: Rc::new(body_code) });
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Loop { count, var_slot: Some(var_slot), body, .. } => {
            let end = match_range_end(count, consts);
            if let Some(end) = end {
                let zero_idx = push_const(consts, default_int(0));
                code.push(Instruction::LoadConstIdx(zero_idx));
                code.push(Instruction::StoreSlot(*var_slot));
                let mut body_code = Vec::new();
                if !compile_expr(body, &mut body_code, consts, true) { return false; }
                code.push(Instruction::ForRange { index_slot: *var_slot, end, body: Rc::new(body_code) });
                if !want_value {
                    code.push(Instruction::Pop);
                }
            } else {
                return false;
            }
        }
        ExprKind::Array(elements) => {
            for e in elements {
                if !compile_expr(e, code, consts, true) { return false; }
            }
            code.push(Instruction::MakeArray(elements.len()));
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::ArrayGenerator { generator, size } => {
            if !compile_expr(generator, code, consts, true) { return false; }
            if !compile_expr(size, code, consts, true) { return false; }
            code.push(Instruction::ArrayGen);
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Map(entries) => {
            for (k, v) in entries {
                if !compile_expr(k, code, consts, true) { return false; }
                if !compile_expr(v, code, consts, true) { return false; }
            }
            code.push(Instruction::MakeMap(entries.len()));
            if !want_value {
                code.push(Instruction::Pop);
            }
        }
        _ => return false,
    }
    true
}

fn substitute(expr: &Expr, args: &[Expr]) -> Expr {
    match &expr.kind {
        ExprKind::Identifier { slot: Some(s), .. } => {
            if *s < args.len() {
                args[*s].clone()
            } else {
                expr.clone()
            }
        }
        ExprKind::Assignment { name, value, slot } => Expr {
            kind: ExprKind::Assignment {
                name: name.clone(),
                value: Box::new(substitute(value, args)),
                slot: *slot,
            },
            line: expr.line,
        },
        ExprKind::IndexAssignment { target, index, value } => Expr {
            kind: ExprKind::IndexAssignment {
                target: Box::new(substitute(target, args)),
                index: Box::new(substitute(index, args)),
                value: Box::new(substitute(value, args)),
            },
            line: expr.line,
        },
        ExprKind::BinaryOp { left, op, right } => Expr {
            kind: ExprKind::BinaryOp {
                left: Box::new(substitute(left, args)),
                op: op.clone(),
                right: Box::new(substitute(right, args)),
            },
            line: expr.line,
        },
        ExprKind::If { condition, then_branch, else_branch } => Expr {
            kind: ExprKind::If {
                condition: Box::new(substitute(condition, args)),
                then_branch: Box::new(substitute(then_branch, args)),
                else_branch: else_branch.as_ref().map(|e| Box::new(substitute(e, args))),
            },
            line: expr.line,
        },
        ExprKind::While { condition, body } => Expr {
            kind: ExprKind::While {
                condition: Box::new(substitute(condition, args)),
                body: Box::new(substitute(body, args)),
            },
            line: expr.line,
        },
        ExprKind::For { var, var_slot, iterable, body } => Expr {
            kind: ExprKind::For {
                var: var.clone(),
                var_slot: *var_slot,
                iterable: Box::new(substitute(iterable, args)),
                body: Box::new(substitute(body, args)),
            },
            line: expr.line,
        },
        ExprKind::Loop { count, var, var_slot, body } => Expr {
            kind: ExprKind::Loop {
                count: Box::new(substitute(count, args)),
                var: var.clone(),
                var_slot: *var_slot,
                body: Box::new(substitute(body, args)),
            },
            line: expr.line,
        },
        ExprKind::Call { function, args: call_args, block, inlined_body } => Expr {
            kind: ExprKind::Call {
                function: Box::new(substitute(function, args)),
                args: call_args.iter().map(|a| substitute(a, args)).collect(),
                block: block.clone(), // Blocks shouldn't be here in is_simple, but safe to clone
                inlined_body: inlined_body.clone(),
            },
            line: expr.line,
        },
        ExprKind::Array(elements) => Expr {
            kind: ExprKind::Array(elements.iter().map(|e| substitute(e, args)).collect()),
            line: expr.line,
        },
        ExprKind::ArrayGenerator { generator, size } => Expr {
            kind: ExprKind::ArrayGenerator {
                generator: Box::new(substitute(generator, args)),
                size: Box::new(substitute(size, args)),
            },
            line: expr.line,
        },
        ExprKind::Map(entries) => Expr {
            kind: ExprKind::Map(
                entries
                    .iter()
                    .map(|(k, v)| (substitute(k, args), substitute(v, args)))
                    .collect(),
            ),
            line: expr.line,
        },
        ExprKind::Index { target, index } => Expr {
            kind: ExprKind::Index {
                target: Box::new(substitute(target, args)),
                index: Box::new(substitute(index, args)),
            },
            line: expr.line,
        },
        ExprKind::Yield(args_exprs) => Expr {
            kind: ExprKind::Yield(args_exprs.iter().map(|a| substitute(a, args)).collect()),
            line: expr.line,
        },
        ExprKind::Block(stmts) => Expr {
            kind: ExprKind::Block(stmts.iter().map(|s| substitute(s, args)).collect()),
            line: expr.line,
        },
        ExprKind::FormatString(parts) => Expr {
            kind: ExprKind::FormatString(parts.iter().map(|part| {
                match part {
                    crate::ast::FormatPart::Literal(s) => crate::ast::FormatPart::Literal(s.clone()),
                    crate::ast::FormatPart::Expr { expr, spec } => crate::ast::FormatPart::Expr {
                        expr: Box::new(substitute(expr, args)),
                        spec: spec.clone(),
                    },
                }
            }).collect()),
            line: expr.line,
        },
        // Literals
        _ => expr.clone(),
    }
}

fn expr_size(expr: &Expr) -> usize {
    match &expr.kind {
        ExprKind::BinaryOp { left, right, .. } => 1 + expr_size(left) + expr_size(right),
        ExprKind::Assignment { value, .. } => 1 + expr_size(value),
        ExprKind::IndexAssignment { target, index, value } => 1 + expr_size(target) + expr_size(index) + expr_size(value),
        ExprKind::If { condition, then_branch, else_branch } => {
            1 + expr_size(condition) + expr_size(then_branch) + else_branch.as_ref().map_or(0, |e| expr_size(e))
        }
        ExprKind::While { condition, body } => 1 + expr_size(condition) + expr_size(body),
        ExprKind::For { iterable, body, .. } => 1 + expr_size(iterable) + expr_size(body),
        ExprKind::Loop { count, body, .. } => 1 + expr_size(count) + expr_size(body),
        ExprKind::Call { function, args, .. } => 1 + expr_size(function) + args.iter().map(expr_size).sum::<usize>(),
        ExprKind::Array(elements) => 1 + elements.iter().map(expr_size).sum::<usize>(),
        ExprKind::ArrayGenerator { generator, size } => 1 + expr_size(generator) + expr_size(size),
        ExprKind::Map(entries) => 1 + entries.iter().map(|(k, v)| expr_size(k) + expr_size(v)).sum::<usize>(),
        ExprKind::Index { target, index } => 1 + expr_size(target) + expr_size(index),
        ExprKind::Yield(args) => 1 + args.iter().map(expr_size).sum::<usize>(),
        ExprKind::Block(stmts) => 1 + stmts.iter().map(expr_size).sum::<usize>(),
        ExprKind::FormatString(parts) => 1 + parts.iter().map(|part| {
            if let crate::ast::FormatPart::Expr { expr, .. } = part {
                expr_size(expr)
            } else {
                1
            }
        }).sum::<usize>(),
        _ => 1,
    }
}

fn is_inline_safe_arg(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Integer { .. } | ExprKind::Unsigned { .. } | ExprKind::Float { .. } | ExprKind::Boolean(_) | ExprKind::Nil => true,
        ExprKind::Identifier { .. } => true,
        ExprKind::BinaryOp { left, right, .. } => is_inline_safe_arg(left) && is_inline_safe_arg(right),
        ExprKind::Index { target, index } => is_inline_safe_arg(target) && is_inline_safe_arg(index),
        ExprKind::FormatString(parts) => parts.iter().all(|part| {
            if let crate::ast::FormatPart::Expr { expr, .. } = part {
                is_inline_safe_arg(expr)
            } else {
                true
            }
        }),
        _ => false,
    }
}

fn execute_instructions(
    interpreter: &mut Interpreter,
    code: &[Instruction],
    const_pool: &[Value],
    slots: &mut [Value],
) -> EvalResult {
    let mut stack = Vec::with_capacity(8);
    let mut ip = 0;
    while ip < code.len() {
        match &code[ip] {
            Instruction::LoadConstIdx(idx) => {
                let val = const_pool.get(*idx).cloned().unwrap_or(Value::Nil);
                stack.push(val);
            }
            Instruction::LoadSlot(s) => stack.push(slots[*s].clone()),
            Instruction::StoreSlot(s) => {
                let val = stack.pop().unwrap();
                if let Some(slot) = slots.get_mut(*s) {
                    *slot = val.clone();
                }
                stack.push(val);
            }
            Instruction::Pop => {
                stack.pop();
            }
            Instruction::Dup => {
                let val = stack.last().cloned().ok_or_else(|| RuntimeError {
                    message: "Stack underflow on dup".to_string(),
                    line: 0,
                })?;
                stack.push(val);
            }
            Instruction::JumpIfFalse(target) => {
                let cond = stack.pop().unwrap_or(Value::Nil);
                let is_false = matches!(cond, Value::Boolean(false) | Value::Nil);
                if is_false {
                    ip = *target;
                    continue;
                }
            }
            Instruction::Jump(target) => {
                ip = *target;
                continue;
            }
            Instruction::CallBuiltin(builtin, argc) => {
                if *argc > stack.len() {
                    return Err(RuntimeError { message: "Invalid argument count".to_string(), line: 0 });
                }
                let mut args = Vec::with_capacity(*argc);
                for _ in 0..*argc {
                    args.push(stack.pop().unwrap());
                }
                args.reverse();
                let result = interpreter.call_builtin(builtin, &args)?;
                stack.push(result);
            }
            Instruction::CallValue(argc) => {
                if *argc > stack.len() {
                    return Err(RuntimeError { message: "Invalid argument count".to_string(), line: 0 });
                }
                let mut args = smallvec::SmallVec::<[Value; 8]>::with_capacity(*argc);
                for _ in 0..*argc {
                    args.push(stack.pop().unwrap());
                }
                args.reverse();
                let func_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing function value for call".to_string(),
                    line: 0,
                })?;
                let result = interpreter.call_value(func_val, args, 0, None)?;
                stack.push(result);
            }
            Instruction::ForEach { var_slot, body } => {
                let iter_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing iterable for for-loop".to_string(),
                    line: 0,
                })?;
                let mut last = Value::Nil;
                match iter_val {
                    Value::Array(arr) => {
                        let len = arr.borrow().len();
                        for idx in 0..len {
                            let item = {
                                let vec = arr.borrow();
                                vec[idx].clone()
                            };
                            if let Some(slot) = slots.get_mut(*var_slot) {
                                *slot = item;
                            }
                            last = execute_instructions(interpreter, body, const_pool, slots)?;
                        }
                    }
                    Value::F64Array(arr) => {
                        let len = arr.borrow().len();
                        for idx in 0..len {
                            let item = {
                                let vec = arr.borrow();
                                make_float(vec[idx], FloatKind::F64)
                            };
                            if let Some(slot) = slots.get_mut(*var_slot) {
                                *slot = item;
                            }
                            last = execute_instructions(interpreter, body, const_pool, slots)?;
                        }
                    }
                    Value::Map(map) => {
                        let map_ref = map.borrow();
                        let mut keys = Vec::with_capacity(map_ref.len());
                        keys.extend(map_ref.keys().cloned());
                        for key in keys {
                            if let Some(slot) = slots.get_mut(*var_slot) {
                                *slot = Value::String(key);
                            }
                            last = execute_instructions(interpreter, body, const_pool, slots)?;
                        }
                    }
                    _ => return Err(RuntimeError { message: "Type is not iterable".to_string(), line: 0 }),
                }
                stack.push(last);
            }
            Instruction::ForRange { index_slot, end, body } => {
                let mut last = Value::Nil;
                loop {
                    let end_val = match end {
                        RangeEnd::Slot(s) => slots.get(*s).cloned().unwrap_or(Value::Nil),
                        RangeEnd::Const(idx) => const_pool.get(*idx).cloned().unwrap_or(Value::Nil),
                    };
                    let end_f = match end_val {
                        Value::Float { value, .. } => value,
                        _ => int_value_as_f64(&end_val).ok_or_else(|| RuntimeError {
                            message: "Range end must be a number".to_string(),
                            line: 0,
                        })?,
                    };
                    let current = match slots.get(*index_slot) {
                        Some(v) => int_value_as_i64(v).ok_or_else(|| RuntimeError {
                            message: "Range index must be a number".to_string(),
                            line: 0,
                        })?,
                        None => return Err(RuntimeError { message: "Range index must be a number".to_string(), line: 0 }),
                    };
                    if (current as f64) >= end_f {
                        if let Some(slot) = slots.get_mut(*index_slot) {
                            *slot = default_int(current as i128);
                        }
                        break;
                    }
                    if let Some(slot) = slots.get_mut(*index_slot) {
                        *slot = default_int(current as i128);
                    }
                    last = execute_instructions(interpreter, body, const_pool, slots)?;
                    if let Some(slot) = slots.get_mut(*index_slot) {
                        *slot = default_int(current as i128 + 1);
                    }
                }
                stack.push(last);
            }
            Instruction::MakeArray(count) => {
                if *count > stack.len() {
                    return Err(RuntimeError { message: "Invalid array length".to_string(), line: 0 });
                }
                let mut elems = Vec::with_capacity(*count);
                let mut f64_vals: Vec<f64> = Vec::new();
                let mut all_f64 = true;
                for _ in 0..*count {
                    let v = stack.pop().unwrap();
                    if all_f64 {
                        match v {
                            Value::Float { value, .. } => f64_vals.push(value),
                            v => {
                                if let Some(num) = int_value_as_f64(&v) {
                                    f64_vals.push(num);
                                } else {
                                    all_f64 = false;
                                    elems.extend(f64_vals.drain(..).map(|value| make_float(value, FloatKind::F64)));
                                    elems.push(v);
                                }
                            }
                        }
                    } else {
                        elems.push(v);
                    }
                }
                if all_f64 {
                    f64_vals.reverse();
                    stack.push(Value::F64Array(Rc::new(RefCell::new(f64_vals))));
                } else {
                    elems.reverse();
                    stack.push(Value::Array(Rc::new(RefCell::new(elems))));
                }
            }
            Instruction::MakeMap(count) => {
                let pair_count = count.saturating_mul(2);
                if pair_count > stack.len() {
                    return Err(RuntimeError { message: "Invalid map length".to_string(), line: 0 });
                }
                let mut entries = Vec::with_capacity(*count);
                for _ in 0..*count {
                    let val = stack.pop().unwrap();
                    let key_val = stack.pop().unwrap();
                    entries.push((key_val, val));
                }
                entries.reverse();
                let mut map = FxHashMap::default();
                for (k_val, v_val) in entries {
                    let key = match k_val {
                        Value::String(s) => s,
                        _ => intern::intern_owned(k_val.inspect()),
                    };
                    map.insert(key, v_val);
                }
                stack.push(Value::Map(Rc::new(RefCell::new(map))));
            }
            Instruction::Index => {
                let index_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing index for index expression".to_string(),
                    line: 0,
                })?;
                let target_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing target for index expression".to_string(),
                    line: 0,
                })?;
                let result = match target_val {
                    Value::Array(arr) => {
                        if let Some(i) = int_value_as_usize(&index_val) {
                            let vec = arr.borrow();
                            if i < vec.len() {
                                vec[i].clone()
                            } else {
                                Value::Nil
                            }
                        } else {
                            return Err(RuntimeError { message: "Array index must be an integer".to_string(), line: 0 });
                        }
                    }
                    Value::F64Array(arr) => {
                        if let Some(i) = int_value_as_usize(&index_val) {
                            let vec = arr.borrow();
                            if i < vec.len() {
                                make_float(vec[i], FloatKind::F64)
                            } else {
                                Value::Nil
                            }
                        } else {
                            return Err(RuntimeError { message: "Array index must be an integer".to_string(), line: 0 });
                        }
                    }
                    Value::Map(map) => {
                        let key = match index_val {
                             Value::String(s) => s,
                             _ => intern::intern_owned(index_val.inspect()),
                        };
                        map.borrow().get(&key).cloned().unwrap_or(Value::Nil)
                    }
                    _ => return Err(RuntimeError { message: "Index operator not supported on this type".to_string(), line: 0 }),
                };
                stack.push(result);
            }
            Instruction::IndexAssign => {
                let value = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing value for index assignment".to_string(),
                    line: 0,
                })?;
                let index_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing index for index assignment".to_string(),
                    line: 0,
                })?;
                let target_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing target for index assignment".to_string(),
                    line: 0,
                })?;
                match target_val {
                    Value::Array(arr) => {
                        if let Some(i) = int_value_as_usize(&index_val) {
                            let mut vec = arr.borrow_mut();
                            if i < vec.len() {
                                vec[i] = value.clone();
                            } else {
                                return Err(RuntimeError { message: "Array index out of bounds".to_string(), line: 0 });
                            }
                        } else {
                            return Err(RuntimeError { message: "Array index must be an integer".to_string(), line: 0 });
                        }
                    }
                    Value::F64Array(arr) => {
                        if let Some(i) = int_value_as_usize(&index_val) {
                            let mut vec = arr.borrow_mut();
                            if i < vec.len() {
                                match &value {
                                    Value::Float { value, .. } => vec[i] = *value,
                                    v => {
                                        if let Some(num) = int_value_as_f64(v) {
                                            vec[i] = num;
                                        } else {
                                            return Err(RuntimeError {
                                                message: "F64Array assignment requires a number".to_string(),
                                                line: 0,
                                            });
                                        }
                                    }
                                }
                            } else {
                                return Err(RuntimeError {
                                    message: "Array index out of bounds".to_string(),
                                    line: 0,
                                });
                            }
                        } else {
                            return Err(RuntimeError {
                                message: "Array index must be an integer".to_string(),
                                line: 0,
                            });
                        }
                    }
                    Value::Map(map) => {
                        let key = match index_val {
                             Value::String(s) => s,
                             _ => intern::intern_owned(index_val.inspect()),
                        };
                        map.borrow_mut().insert(key, value.clone());
                    }
                    _ => return Err(RuntimeError { message: "Index assignment not supported on this type".to_string(), line: 0 }),
                }
                stack.push(value);
            }
            Instruction::ArrayGen => {
                let size_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing size for array generator".to_string(),
                    line: 0,
                })?;
                let gen_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing generator for array".to_string(),
                    line: 0,
                })?;
                let n = int_value_as_usize(&size_val)
                    .ok_or_else(|| RuntimeError {
                        message: "Array size must be a non-negative integer".to_string(),
                        line: 0,
                    })?;
                let mut vals: Vec<Value> = Vec::new();
                let mut f64_vals: Vec<f64> = Vec::new();
                let mut all_f64 = true;
                if let Value::Function(data) = gen_val {
                    for i in 0..n {
                        let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(Value::Uninitialized, data.declarations.len());
                        if !data.params.is_empty() {
                             new_slots[data.param_offset] = default_int(i as i128);
                        }
                        let result = if let Some(code) = &data.code {
                            execute_instructions(interpreter, code, const_pool, &mut new_slots)?
                        } else if data.uses_env {
                            let new_env = interpreter.get_env(Some(data.env.clone()), false);
                            let original_env = interpreter.env.clone();
                            interpreter.env = new_env.clone();
                            let result = interpreter.eval(&data.body, &mut new_slots)?;
                            interpreter.env = original_env;
                            interpreter.recycle_env(new_env);
                            result
                        } else {
                            interpreter.eval(&data.body, &mut new_slots)?
                        };
                        if all_f64 {
                            match result {
                                Value::Float { value, .. } => f64_vals.push(value),
                                Value::Integer { value, .. } => f64_vals.push(value as f64),
                                Value::Unsigned { value, .. } => f64_vals.push(value as f64),
                                _ => {
                                    all_f64 = false;
                                    vals.extend(f64_vals.drain(..).map(|value| make_float(value, FloatKind::F64)));
                                    vals.push(result);
                                }
                            }
                        } else {
                            vals.push(result);
                        }
                    }
                } else {
                    for _ in 0..n {
                        if all_f64 {
                            match gen_val {
                                Value::Float { value, .. } => f64_vals.push(value),
                                Value::Integer { value, .. } => f64_vals.push(value as f64),
                                Value::Unsigned { value, .. } => f64_vals.push(value as f64),
                                _ => {
                                    all_f64 = false;
                                    vals.extend(f64_vals.drain(..).map(|value| make_float(value, FloatKind::F64)));
                                    vals.push(gen_val.clone());
                                }
                            }
                        } else {
                            vals.push(gen_val.clone());
                        }
                    }
                }
                if all_f64 {
                    stack.push(Value::F64Array(Rc::new(RefCell::new(f64_vals))));
                } else {
                    stack.push(Value::Array(Rc::new(RefCell::new(vals))));
                }
            }
            Instruction::F64Axpy { dst_slot, dst_index_slot, src_slot, src_index_slot } => {
                let scalar_val = stack.pop().ok_or_else(|| RuntimeError {
                    message: "Missing scalar for F64Axpy".to_string(),
                    line: 0,
                })?;
                let scalar = match scalar_val {
                    Value::Float { value, .. } => value,
                    v => int_value_as_f64(&v).ok_or_else(|| RuntimeError {
                        message: "F64Axpy requires numeric scalar".to_string(),
                        line: 0,
                    })?,
                };
                let dst_idx = match slots.get(*dst_index_slot) {
                    Some(v) => number_to_usize(v).ok_or_else(|| RuntimeError {
                        message: "F64Axpy dst index must be numeric".to_string(),
                        line: 0,
                    })?,
                    None => return Err(RuntimeError { message: "F64Axpy dst index must be numeric".to_string(), line: 0 }),
                };
                let src_idx = match slots.get(*src_index_slot) {
                    Some(v) => number_to_usize(v).ok_or_else(|| RuntimeError {
                        message: "F64Axpy src index must be numeric".to_string(),
                        line: 0,
                    })?,
                    None => return Err(RuntimeError { message: "F64Axpy src index must be numeric".to_string(), line: 0 }),
                };
                let dst = match slots.get_mut(*dst_slot) {
                    Some(Value::F64Array(arr)) => arr.clone(),
                    _ => return Err(RuntimeError { message: "F64Axpy requires F64Array dst".to_string(), line: 0 }),
                };
                let src = match slots.get(*src_slot) {
                    Some(Value::F64Array(arr)) => arr.clone(),
                    _ => return Err(RuntimeError { message: "F64Axpy requires F64Array src".to_string(), line: 0 }),
                };
                let mut dst_vec = dst.borrow_mut();
                let src_vec = src.borrow();
                if dst_idx >= dst_vec.len() || src_idx >= src_vec.len() {
                    return Err(RuntimeError { message: "F64Axpy index out of bounds".to_string(), line: 0 });
                }
                let result = dst_vec[dst_idx] + scalar * src_vec[src_idx];
                dst_vec[dst_idx] = result;
                stack.push(make_float(result, FloatKind::F64));
            }
            Instruction::F64DotRange { acc_slot, a_slot, b_slot, index_slot, end } => {
                let start = match slots.get(*index_slot) {
                    Some(v) => number_to_usize(v).unwrap_or(0),
                    None => 0,
                };
                let end_val = match end {
                    RangeEnd::Slot(s) => slots.get(*s).cloned().unwrap_or(Value::Nil),
                    RangeEnd::Const(idx) => const_pool.get(*idx).cloned().unwrap_or(Value::Nil),
                };
                let end_idx = number_to_usize(&end_val).ok_or_else(|| RuntimeError {
                    message: "Range end must be a non-negative number".to_string(),
                    line: 0,
                })?;
                let acc = match slots.get(*acc_slot) {
                    Some(Value::Float { value, .. }) => *value,
                    Some(v) => int_value_as_f64(v).unwrap_or(0.0),
                    _ => 0.0,
                };
                let a = match slots.get(*a_slot) {
                    Some(Value::F64Array(arr)) => arr.clone(),
                    _ => return Err(RuntimeError { message: "F64DotRange requires F64Array a".to_string(), line: 0 }),
                };
                let b = match slots.get(*b_slot) {
                    Some(Value::F64Array(arr)) => arr.clone(),
                    _ => return Err(RuntimeError { message: "F64DotRange requires F64Array b".to_string(), line: 0 }),
                };
                let a_vec = a.borrow();
                let b_vec = b.borrow();
                if end_idx > a_vec.len() || end_idx > b_vec.len() {
                    return Err(RuntimeError { message: "F64DotRange index out of bounds".to_string(), line: 0 });
                }
                let mut i = start;
                let mut sum = Simd::<f64, 4>::splat(0.0);
                while i + 4 <= end_idx {
                    let av = Simd::from_slice(&a_vec[i..i + 4]);
                    let bv = Simd::from_slice(&b_vec[i..i + 4]);
                    sum += av * bv;
                    i += 4;
                }
                let mut total = acc + sum.reduce_sum();
                while i < end_idx {
                    total += a_vec[i] * b_vec[i];
                    i += 1;
                }
                if let Some(slot) = slots.get_mut(*acc_slot) {
                    *slot = make_float(total, FloatKind::F64);
                }
                if let Some(slot) = slots.get_mut(*index_slot) {
                    *slot = default_int(end_idx as i128);
                }
                stack.push(make_float(total, FloatKind::F64));
            }
            Instruction::F64Dot2Range {
                acc1_slot,
                a1_slot,
                b1_slot,
                acc2_slot,
                a2_slot,
                b2_slot,
                index_slot,
                end,
            } => {
                let start = match slots.get(*index_slot) {
                    Some(v) => number_to_usize(v).unwrap_or(0),
                    None => 0,
                };
                let end_val = match end {
                    RangeEnd::Slot(s) => slots.get(*s).cloned().unwrap_or(Value::Nil),
                    RangeEnd::Const(idx) => const_pool.get(*idx).cloned().unwrap_or(Value::Nil),
                };
                let end_idx = number_to_usize(&end_val).ok_or_else(|| RuntimeError {
                    message: "Range end must be a non-negative number".to_string(),
                    line: 0,
                })?;
                let acc1 = match slots.get(*acc1_slot) {
                    Some(Value::Float { value, .. }) => *value,
                    Some(v) => int_value_as_f64(v).unwrap_or(0.0),
                    _ => 0.0,
                };
                let acc2 = match slots.get(*acc2_slot) {
                    Some(Value::Float { value, .. }) => *value,
                    Some(v) => int_value_as_f64(v).unwrap_or(0.0),
                    _ => 0.0,
                };
                let a1 = match slots.get(*a1_slot) {
                    Some(Value::F64Array(arr)) => arr.clone(),
                    _ => return Err(RuntimeError { message: "F64Dot2Range requires F64Array a1".to_string(), line: 0 }),
                };
                let b1 = match slots.get(*b1_slot) {
                    Some(Value::F64Array(arr)) => arr.clone(),
                    _ => return Err(RuntimeError { message: "F64Dot2Range requires F64Array b1".to_string(), line: 0 }),
                };
                let a2 = match slots.get(*a2_slot) {
                    Some(Value::F64Array(arr)) => arr.clone(),
                    _ => return Err(RuntimeError { message: "F64Dot2Range requires F64Array a2".to_string(), line: 0 }),
                };
                let b2 = match slots.get(*b2_slot) {
                    Some(Value::F64Array(arr)) => arr.clone(),
                    _ => return Err(RuntimeError { message: "F64Dot2Range requires F64Array b2".to_string(), line: 0 }),
                };
                let a1_vec = a1.borrow();
                let b1_vec = b1.borrow();
                let a2_vec = a2.borrow();
                let b2_vec = b2.borrow();
                if end_idx > a1_vec.len()
                    || end_idx > b1_vec.len()
                    || end_idx > a2_vec.len()
                    || end_idx > b2_vec.len()
                {
                    return Err(RuntimeError { message: "F64Dot2Range index out of bounds".to_string(), line: 0 });
                }
                let mut i = start;
                let mut sum1 = Simd::<f64, 4>::splat(0.0);
                let mut sum2 = Simd::<f64, 4>::splat(0.0);
                while i + 4 <= end_idx {
                    let a1v = Simd::from_slice(&a1_vec[i..i + 4]);
                    let b1v = Simd::from_slice(&b1_vec[i..i + 4]);
                    let a2v = Simd::from_slice(&a2_vec[i..i + 4]);
                    let b2v = Simd::from_slice(&b2_vec[i..i + 4]);
                    sum1 += a1v * b1v;
                    sum2 += a2v * b2v;
                    i += 4;
                }
                let mut total1 = acc1 + sum1.reduce_sum();
                let mut total2 = acc2 + sum2.reduce_sum();
                while i < end_idx {
                    total1 += a1_vec[i] * b1_vec[i];
                    total2 += a2_vec[i] * b2_vec[i];
                    i += 1;
                }
                if let Some(slot) = slots.get_mut(*acc1_slot) {
                    *slot = make_float(total1, FloatKind::F64);
                }
                if let Some(slot) = slots.get_mut(*acc2_slot) {
                    *slot = make_float(total2, FloatKind::F64);
                }
                if let Some(slot) = slots.get_mut(*index_slot) {
                    *slot = default_int(end_idx as i128);
                }
                stack.push(make_float(total2, FloatKind::F64));
            }
            inst => {
                let r = stack.pop().unwrap();
                let l = stack.pop().unwrap();
                let res = match (l, r) {
                    (Value::Integer { value: i1, kind: k1 }, Value::Integer { value: i2, kind: k2 }) => {
                        let kind = signed_kind_for_bits(int_kind_bits(k1).max(int_kind_bits(k2)));
                        match inst {
                            Instruction::Add => make_signed_int(i1 + i2, kind),
                            Instruction::Sub => make_signed_int(i1 - i2, kind),
                            Instruction::Mul => make_signed_int(i1 * i2, kind),
                            Instruction::Div => make_signed_int(i1 / i2, kind),
                            Instruction::Eq => Value::Boolean(i1 == i2),
                            Instruction::Gt => Value::Boolean(i1 > i2),
                            Instruction::Lt => Value::Boolean(i1 < i2),
                            _ => unreachable!(),
                        }
                    }
                    (Value::Unsigned { value: u1, kind: k1 }, Value::Unsigned { value: u2, kind: k2 }) => {
                        let kind = unsigned_kind_for_bits(int_kind_bits(k1).max(int_kind_bits(k2)));
                        match inst {
                            Instruction::Add => make_unsigned_int(u1 + u2, kind),
                            Instruction::Sub => make_unsigned_int(u1 - u2, kind),
                            Instruction::Mul => make_unsigned_int(u1 * u2, kind),
                            Instruction::Div => make_unsigned_int(u1 / u2, kind),
                            Instruction::Eq => Value::Boolean(u1 == u2),
                            Instruction::Gt => Value::Boolean(u1 > u2),
                            Instruction::Lt => Value::Boolean(u1 < u2),
                            _ => unreachable!(),
                        }
                    }
                    (Value::Integer { value: i1, .. }, Value::Unsigned { value: u2, .. }) => {
                        let u2_i = i128::try_from(u2).map_err(|_| RuntimeError {
                            message: "Unsigned value too large for signed operation".to_string(),
                            line: 0,
                        })?;
                        match inst {
                            Instruction::Add => make_signed_int(i1 + u2_i, IntKind::I128),
                            Instruction::Sub => make_signed_int(i1 - u2_i, IntKind::I128),
                            Instruction::Mul => make_signed_int(i1 * u2_i, IntKind::I128),
                            Instruction::Div => make_signed_int(i1 / u2_i, IntKind::I128),
                            Instruction::Eq => Value::Boolean(i1 == u2_i),
                            Instruction::Gt => Value::Boolean(i1 > u2_i),
                            Instruction::Lt => Value::Boolean(i1 < u2_i),
                            _ => unreachable!(),
                        }
                    }
                    (Value::Unsigned { value: u1, .. }, Value::Integer { value: i2, .. }) => {
                        let u1_i = i128::try_from(u1).map_err(|_| RuntimeError {
                            message: "Unsigned value too large for signed operation".to_string(),
                            line: 0,
                        })?;
                        match inst {
                            Instruction::Add => make_signed_int(u1_i + i2, IntKind::I128),
                            Instruction::Sub => make_signed_int(u1_i - i2, IntKind::I128),
                            Instruction::Mul => make_signed_int(u1_i * i2, IntKind::I128),
                            Instruction::Div => make_signed_int(u1_i / i2, IntKind::I128),
                            Instruction::Eq => Value::Boolean(u1_i == i2),
                            Instruction::Gt => Value::Boolean(u1_i > i2),
                            Instruction::Lt => Value::Boolean(u1_i < i2),
                            _ => unreachable!(),
                        }
                    }
                    (Value::Float { value: f1, kind: k1 }, Value::Float { value: f2, kind: k2 }) => {
                        let kind = promote_float_kind(k1, k2);
                        match inst {
                            Instruction::Add => make_float(f1 + f2, kind),
                            Instruction::Sub => make_float(f1 - f2, kind),
                            Instruction::Mul => make_float(f1 * f2, kind),
                            Instruction::Div => make_float(f1 / f2, kind),
                            Instruction::Eq => Value::Boolean(f1 == f2),
                            Instruction::Gt => Value::Boolean(f1 > f2),
                            Instruction::Lt => Value::Boolean(f1 < f2),
                            _ => unreachable!(),
                        }
                    }
                    (v @ Value::Integer { .. }, Value::Float { value: f, kind })
                    | (v @ Value::Unsigned { .. }, Value::Float { value: f, kind }) => {
                        let f1 = int_value_as_f64(&v).unwrap_or(0.0);
                        match inst {
                            Instruction::Add => make_float(f1 + f, kind),
                            Instruction::Sub => make_float(f1 - f, kind),
                            Instruction::Mul => make_float(f1 * f, kind),
                            Instruction::Div => make_float(f1 / f, kind),
                            Instruction::Eq => Value::Boolean(f1 == f),
                            Instruction::Gt => Value::Boolean(f1 > f),
                            Instruction::Lt => Value::Boolean(f1 < f),
                            _ => unreachable!(),
                        }
                    }
                    (Value::Float { value: f, kind }, v @ Value::Integer { .. })
                    | (Value::Float { value: f, kind }, v @ Value::Unsigned { .. }) => {
                        let f2 = int_value_as_f64(&v).unwrap_or(0.0);
                        match inst {
                            Instruction::Add => make_float(f + f2, kind),
                            Instruction::Sub => make_float(f - f2, kind),
                            Instruction::Mul => make_float(f * f2, kind),
                            Instruction::Div => make_float(f / f2, kind),
                            Instruction::Eq => Value::Boolean(f == f2),
                            Instruction::Gt => Value::Boolean(f > f2),
                            Instruction::Lt => Value::Boolean(f < f2),
                            _ => unreachable!(),
                        }
                    }
                    (Value::String(s1), Value::String(s2)) => match inst {
                        Instruction::Add => {
                            let mut out = s1.clone();
                            Rc::make_mut(&mut out).push_str(&s2);
                            Value::String(out)
                        }
                        _ => return Err(RuntimeError { message: "Invalid types for operation".to_string(), line: 0 }),
                    },
                    (Value::String(s), v2) => match inst {
                        Instruction::Add => {
                            let mut out = s.clone();
                            Rc::make_mut(&mut out).push_str(&v2.inspect());
                            Value::String(out)
                        }
                        _ => return Err(RuntimeError { message: "Invalid types for operation".to_string(), line: 0 }),
                    },
                    _ => return Err(RuntimeError { message: "Invalid types for operation".to_string(), line: 0 }),
                };
                stack.push(res);
            }
        }
        ip += 1;
    }
    Ok(stack.pop().unwrap())
}

fn is_simple(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Yield(_) | ExprKind::FunctionDef { .. } | ExprKind::AnonymousFunction { .. } | ExprKind::Use(_) | ExprKind::Load(_) => false,
        ExprKind::Assignment { slot: None, .. } => false,
        ExprKind::Block(stmts) => stmts.iter().all(is_simple),
        ExprKind::FormatString(parts) => parts.iter().all(|part| {
            if let crate::ast::FormatPart::Expr { expr, .. } = part {
                is_simple(expr)
            } else {
                true
            }
        }),
        ExprKind::If { condition, then_branch, else_branch } => {
            is_simple(condition) && is_simple(then_branch) && else_branch.as_ref().map_or(true, |eb| is_simple(eb))
        }
        ExprKind::While { condition, body } => is_simple(condition) && is_simple(body),
        ExprKind::For { iterable, body, .. } => is_simple(iterable) && is_simple(body),
        ExprKind::Loop { count, body, .. } => is_simple(count) && is_simple(body),
        ExprKind::BinaryOp { left, right, .. } => is_simple(left) && is_simple(right),
        ExprKind::Call { function, args, block, .. } => {
            if block.is_some() { return false; }
            is_simple(function) && args.iter().all(is_simple)
        }
        ExprKind::Array(elements) => elements.iter().all(is_simple),
        ExprKind::ArrayGenerator { generator, size } => is_simple(generator) && is_simple(size),
        ExprKind::Map(entries) => entries.iter().all(|(k, v)| is_simple(k) && is_simple(v)),
        ExprKind::Index { target, index } => is_simple(target) && is_simple(index),
        ExprKind::IndexAssignment { target, index, value } => is_simple(target) && is_simple(index) && is_simple(value),
        _ => true,
    }
}

fn resolve(expr: &mut Expr, slot_map: &FxHashMap<SymbolId, usize>) {
    match &mut expr.kind {
        ExprKind::Identifier { name, slot } => {
                if let Some(s) = slot_map.get(name) {
                    *slot = Some(*s);
                }
        }
        ExprKind::Assignment { name, value, slot } => {
            if let Some(s) = slot_map.get(name) {
                *slot = Some(*s);
            }
            resolve(value, slot_map);
        }
        ExprKind::BinaryOp { left, right, .. } => {
            resolve(left, slot_map);
            resolve(right, slot_map);
        }
        ExprKind::Block(stmts) => {
            for stmt in stmts {
                resolve(stmt, slot_map);
            }
        }
        ExprKind::If { condition, then_branch, else_branch } => {
            resolve(condition, slot_map);
            resolve(then_branch, slot_map);
            if let Some(eb) = else_branch {
                resolve(eb, slot_map);
            }
        }
        ExprKind::While { condition, body } => {
            resolve(condition, slot_map);
            resolve(body, slot_map);
        }
        ExprKind::For { var, var_slot, iterable, body, .. } => {
            if let Some(s) = slot_map.get(var) {
                *var_slot = Some(*s);
            }
            resolve(iterable, slot_map);
            resolve(body, slot_map);
        }
        ExprKind::Loop { var, var_slot, count, body } => {
            if let Some(name) = var {
                if let Some(s) = slot_map.get(name) {
                    *var_slot = Some(*s);
                }
            }
            resolve(count, slot_map);
            resolve(body, slot_map);
        }
        ExprKind::Call { function, args, block, .. } => {
            resolve(function, slot_map);
            for arg in args {
                resolve(arg, slot_map);
            }
            if let Some(c) = block {
                resolve(&mut c.body, slot_map);
            }
        }
        ExprKind::Array(elements) => {
            for e in elements {
                resolve(e, slot_map);
            }
        }
        ExprKind::ArrayGenerator { generator, size } => {
            resolve(generator, slot_map);
            resolve(size, slot_map);
        }
        ExprKind::Map(entries) => {
            for (k, v) in entries {
                resolve(k, slot_map);
                resolve(v, slot_map);
            }
        }
        ExprKind::Index { target, index } => {
            resolve(target, slot_map);
            resolve(index, slot_map);
        }
        ExprKind::IndexAssignment { target, index, value } => {
            resolve(target, slot_map);
            resolve(index, slot_map);
            resolve(value, slot_map);
        }
        ExprKind::FormatString(parts) => {
            for part in parts {
                if let crate::ast::FormatPart::Expr { expr, .. } = part {
                    resolve(expr, slot_map);
                }
            }
        }
        ExprKind::Use(_) => {}
        ExprKind::Load(_) => {}
        ExprKind::Yield(args) => {
            for a in args {
                resolve(a, slot_map);
            }
        }
        _ => {}
    }
}

pub struct Interpreter {
    // Current environment (scope)
    env: Rc<RefCell<Environment>>,
    // Stack of blocks passed to currently executing functions.
    block_stack: Vec<Option<(Closure, Rc<RefCell<Environment>>)>>,
    // Pool of spare environments for reuse
    env_pool: Vec<Rc<RefCell<Environment>>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new(None))),
            block_stack: Vec::new(),
            env_pool: Vec::with_capacity(32),
        }
    }

    fn get_env(&mut self, parent: Option<Rc<RefCell<Environment>>>, is_partial: bool) -> Rc<RefCell<Environment>> {
        if let Some(env_rc) = self.env_pool.pop() {
            env_rc.borrow_mut().reset(parent, is_partial);
            env_rc
        } else {
            Rc::new(RefCell::new(Environment::new(parent)))
        }
    }

    fn recycle_env(&mut self, env_rc: Rc<RefCell<Environment>>) {
        if Rc::strong_count(&env_rc) == 1 {
            if self.env_pool.len() < 128 {
                self.env_pool.push(env_rc);
            }
        }
    }

    pub fn define_global(&mut self, name: SymbolId, val: Value) {
        self.env.borrow_mut().define(name, val);
    }

    fn ensure_std_module(&mut self) {
        let std_sym = intern::intern_symbol("std");
        let existing = { self.env.borrow().get(std_sym) };
        match existing {
            Some(Value::Map(map)) => {
                let mut map_mut = map.borrow_mut();
                if !map_mut.contains_key(&intern::intern("Int8")) {
                    map_mut.insert(intern::intern("Int8"), build_int8_module());
                }
                if !map_mut.contains_key(&intern::intern("Int16")) {
                    map_mut.insert(intern::intern("Int16"), build_int16_module());
                }
                if !map_mut.contains_key(&intern::intern("Int32")) {
                    map_mut.insert(intern::intern("Int32"), build_int32_module());
                }
                if !map_mut.contains_key(&intern::intern("Int64")) {
                    map_mut.insert(intern::intern("Int64"), build_int64_module());
                }
                if !map_mut.contains_key(&intern::intern("Int128")) {
                    map_mut.insert(intern::intern("Int128"), build_int128_module());
                }
                if !map_mut.contains_key(&intern::intern("Uint8")) {
                    map_mut.insert(intern::intern("Uint8"), build_uint8_module());
                }
                if !map_mut.contains_key(&intern::intern("Uint16")) {
                    map_mut.insert(intern::intern("Uint16"), build_uint16_module());
                }
                if !map_mut.contains_key(&intern::intern("Uint32")) {
                    map_mut.insert(intern::intern("Uint32"), build_uint32_module());
                }
                if !map_mut.contains_key(&intern::intern("Uint64")) {
                    map_mut.insert(intern::intern("Uint64"), build_uint64_module());
                }
                if !map_mut.contains_key(&intern::intern("Uint128")) {
                    map_mut.insert(intern::intern("Uint128"), build_uint128_module());
                }
                if !map_mut.contains_key(&intern::intern("Float32")) {
                    map_mut.insert(intern::intern("Float32"), build_float32_module());
                }
                if !map_mut.contains_key(&intern::intern("Float64")) {
                    map_mut.insert(intern::intern("Float64"), build_float64_module());
                }
                if !map_mut.contains_key(&intern::intern("Float128")) {
                    map_mut.insert(intern::intern("Float128"), build_float128_module());
                }
            }
            Some(_) | None => {
                self.define_global(std_sym, build_std_module());
            }
        }
    }

    fn ensure_wasm_namespace(&mut self) -> Rc<RefCell<FxHashMap<Rc<String>, Value>>> {
        let wasm_sym = intern::intern_symbol("wasm");
        let existing = { self.env.borrow().get(wasm_sym) };
        match existing {
            Some(Value::Map(map)) => map,
            _ => {
                let map = Rc::new(RefCell::new(FxHashMap::default()));
                self.define_global(wasm_sym, Value::Map(map.clone()));
                map
            }
        }
    }

    fn load_wasm_module(&mut self, path: &[SymbolId], line: usize) -> EvalResult {
        if path.len() < 2 {
            return Err(RuntimeError { message: "load wasm requires a module name".to_string(), line });
        }
        let wasm_sym = intern::intern_symbol("wasm");
        if path[0] != wasm_sym {
            return Err(RuntimeError { message: "load currently supports only wasm:: modules".to_string(), line });
        }

        let mut rel = PathBuf::from("wasm");
        for segment in &path[1..path.len() - 1] {
            rel.push(symbol_name(*segment).as_str());
        }
        let module_name = symbol_name(*path.last().unwrap());
        rel.push(format!("{}.wasm", module_name.as_str()));

        let module = WasmModule::load(&rel).map_err(|message| RuntimeError { message, line })?;
        let mut exports = FxHashMap::default();
        {
            let module_ref = module.borrow();
            for name in module_ref.functions.keys() {
                exports.insert(
                    name.clone(),
                    Value::WasmFunction(Rc::new(WasmFunction {
                        name: name.clone(),
                        module: module.clone(),
                    })),
                );
            }
        }

        let wasm_map = self.ensure_wasm_namespace();
        wasm_map.borrow_mut().insert(module_name, Value::Map(Rc::new(RefCell::new(exports))));
        Ok(Value::Nil)
    }

    fn import_path(&mut self, path: &[SymbolId], line: usize) -> EvalResult {
        if path.is_empty() {
            return Err(RuntimeError { message: "use requires a module path".to_string(), line });
        }

        let std_sym = intern::intern_symbol("std");
        if path[0] == std_sym {
            self.ensure_std_module();
        }

        let mut current = self.env.borrow().get(path[0]).ok_or_else(|| RuntimeError {
            message: format!("Module '{}' not found", symbol_name(path[0]).as_str()),
            line,
        })?;
        let mut current_name = symbol_name(path[0]);

        for segment in &path[1..] {
            let seg_name = symbol_name(*segment);
            match current {
                Value::Map(map) => {
                    if let Some(next) = map.borrow().get(&seg_name).cloned() {
                        current = next;
                        current_name = seg_name;
                    } else {
                        return Err(RuntimeError {
                            message: format!(
                                "Module '{}' has no member '{}'",
                                current_name.as_str(),
                                seg_name.as_str()
                            ),
                            line,
                        });
                    }
                }
                _ => {
                    return Err(RuntimeError {
                        message: format!("'{}' is not a module", current_name.as_str()),
                        line,
                    });
                }
            }
        }

        Ok(Value::Nil)
    }

    fn call_builtin(&mut self, builtin: &Builtin, args: &[Value]) -> EvalResult {
        match builtin {
            Builtin::Puts => {
                let mut last = Value::Nil;
                for arg in args {
                    println!("{}", arg);
                    last = arg.clone();
                }
                Ok(last)
            }
            Builtin::Print => {
                let mut last = Value::Nil;
                for arg in args {
                    print!("{}", arg);
                    io::stdout().flush().unwrap();
                    last = arg.clone();
                }
                Ok(last)
            }
            Builtin::Len => {
                let val = args.get(0).cloned().unwrap_or(Value::Nil);
                match val {
                    Value::String(s) => Ok(default_int(s.len() as i128)),
                    Value::Array(arr) => Ok(default_int(arr.borrow().len() as i128)),
                    Value::F64Array(arr) => Ok(default_int(arr.borrow().len() as i128)),
                    Value::Map(map) => Ok(default_int(map.borrow().len() as i128)),
                    _ => Ok(default_int(0)),
                }
            }
            Builtin::ReadFile => {
                let path = args.get(0).cloned().unwrap_or(Value::Nil).to_string();
                match fs::read_to_string(&path) {
                    Ok(content) => Ok(Value::String(intern::intern_owned(content))),
                    Err(_) => Ok(Value::Nil),
                }
            }
            Builtin::WriteFile => {
                let path = args.get(0).cloned().unwrap_or(Value::Nil).to_string();
                let content = args.get(1).cloned().unwrap_or(Value::Nil).to_string();
                match fs::File::create(&path) {
                    Ok(mut file) => {
                        write!(file, "{}", content).unwrap();
                        Ok(Value::Boolean(true))
                    }
                    Err(_) => Ok(Value::Boolean(false)),
                }
            }
        }
    }

    fn call_value(
        &mut self,
        func_val: Value,
        arg_vals: smallvec::SmallVec<[Value; 8]>,
        line: usize,
        block: Option<Closure>,
    ) -> EvalResult {
        match func_val {
            Value::Function(data) => self.invoke_function(data, arg_vals, line, block),
            Value::NativeFunction(func) => {
                if block.is_some() {
                    return Err(RuntimeError { message: "Native function does not accept a block".to_string(), line });
                }
                func(&arg_vals).map_err(|message| RuntimeError { message, line })
            }
            Value::WasmFunction(func) => {
                if block.is_some() {
                    return Err(RuntimeError { message: "Wasm function does not accept a block".to_string(), line });
                }
                self.call_wasm_function(func, arg_vals, line)
            }
            _ => Err(RuntimeError { message: format!("Tried to call a non-function value: {}", func_val), line }),
        }
    }

    fn call_wasm_function(
        &mut self,
        func: Rc<WasmFunction>,
        arg_vals: smallvec::SmallVec<[Value; 8]>,
        line: usize,
    ) -> EvalResult {
        let mut module = func.module.borrow_mut();
        let wasm_func = module.functions.get(&func.name).ok_or_else(|| RuntimeError {
            message: format!("Wasm function '{}' not found", func.name),
            line,
        })?.clone();
        let func_type = module.func_types.get(&func.name).ok_or_else(|| RuntimeError {
            message: format!("Wasm function '{}' type not found", func.name),
            line,
        })?.clone();

        let params = func_type.params();
        let results = func_type.results();
        if results.len() > 1 {
            return Err(RuntimeError { message: "Wasm functions with multiple returns are not supported".to_string(), line });
        }

        let mut wasm_args: Vec<WasmValue> = Vec::new();
        let mut allocs: Vec<(i32, i32)> = Vec::new();
        let mut arg_index = 0usize;
        let mut param_index = 0usize;

        let estimated_params = arg_vals.iter().map(|v| if matches!(v, Value::String(_)) { 2 } else { 1 }).sum::<usize>();
        let wbindgen_mode = !params.is_empty()
            && params[0] == ValueType::I32
            && params.len() == estimated_params + 1
            && (results.is_empty() || results[0] == ValueType::I32);

        let mut retptr: Option<i32> = None;
        let mut retptr_alloc: Option<(i32, i32)> = None;
        if wbindgen_mode {
            if let Some(add) = module.wbindgen_add_to_stack_pointer {
                let mut result = [WasmValue::I32(0)];
                add.call(&mut module.store, &[WasmValue::I32(-16)], &mut result)
                    .map_err(|e| RuntimeError { message: format!("Wasm stack adjust failed: {}", e), line })?;
                let ptr = match result[0] {
                    WasmValue::I32(v) => v,
                    _ => return Err(RuntimeError { message: "Wasm stack adjust returned non-i32".to_string(), line }),
                };
                retptr = Some(ptr);
            } else {
                let (alloc, alloc_name) = if let Some(func) = module.alloc {
                    (func, "alloc")
                } else if let Some(func) = module.wbindgen_malloc {
                    (func, "__wbindgen_malloc")
                } else {
                    return Err(RuntimeError { message: "Wasm module has no alloc export".to_string(), line });
                };
                let mut results = [WasmValue::I32(0)];
                let alloc_params = module.func_types.get(&intern::intern(alloc_name)).map(|t| t.params().len()).unwrap_or(1);
                let alloc_args = if alloc_params == 2 {
                    vec![WasmValue::I32(8), WasmValue::I32(1)]
                } else {
                    vec![WasmValue::I32(8)]
                };
                alloc.call(&mut module.store, &alloc_args, &mut results)
                    .map_err(|e| RuntimeError { message: format!("Wasm alloc failed: {}", e), line })?;
                let ptr = match results[0] {
                    WasmValue::I32(v) => v,
                    _ => return Err(RuntimeError { message: "Wasm alloc returned non-i32".to_string(), line }),
                };
                retptr = Some(ptr);
                retptr_alloc = Some((ptr, 8));
            }
            if let Some(ptr) = retptr {
                wasm_args.push(WasmValue::I32(ptr));
                param_index += 1;
            }
        }

        while arg_index < arg_vals.len() {
            if param_index >= params.len() {
                return Err(RuntimeError { message: "Wasm function argument count mismatch".to_string(), line });
            }
            let arg = &arg_vals[arg_index];
            match arg {
                Value::String(s) => {
                    if param_index + 1 >= params.len() || params[param_index] != ValueType::I32 || params[param_index + 1] != ValueType::I32 {
                        return Err(RuntimeError { message: "Wasm string arguments require two i32 params".to_string(), line });
                    }
                    let memory = module.memory.ok_or_else(|| RuntimeError {
                        message: "Wasm module has no memory export".to_string(),
                        line,
                    })?;
                    let (alloc, alloc_name) = if let Some(func) = module.alloc {
                        (func, "alloc")
                    } else if let Some(func) = module.wbindgen_malloc {
                        (func, "__wbindgen_malloc")
                    } else {
                        return Err(RuntimeError { message: "Wasm module has no alloc export".to_string(), line });
                    };
                    let bytes = s.as_bytes();
                    let mut results = [WasmValue::I32(0)];
                    let alloc_params = module.func_types.get(&intern::intern(alloc_name)).map(|t| t.params().len()).unwrap_or(1);
                    let alloc_args = if alloc_params == 2 {
                        vec![WasmValue::I32(bytes.len() as i32), WasmValue::I32(1)]
                    } else {
                        vec![WasmValue::I32(bytes.len() as i32)]
                    };
                    alloc.call(&mut module.store, &alloc_args, &mut results)
                        .map_err(|e| RuntimeError { message: format!("Wasm alloc failed: {}", e), line })?;
                    let ptr = match results[0] {
                        WasmValue::I32(v) => v,
                        _ => return Err(RuntimeError { message: "Wasm alloc returned non-i32".to_string(), line }),
                    };
                    let mem = memory.data_mut(&mut module.store);
                    let start = ptr as usize;
                    let end = start + bytes.len();
                    if end > mem.len() {
                        return Err(RuntimeError { message: "Wasm memory overflow writing string".to_string(), line });
                    }
                    mem[start..end].copy_from_slice(bytes);
                    allocs.push((ptr, bytes.len() as i32));
                    wasm_args.push(WasmValue::I32(ptr));
                    wasm_args.push(WasmValue::I32(bytes.len() as i32));
                    arg_index += 1;
                    param_index += 2;
                }
                Value::Boolean(b) => {
                    let val = if *b { 1 } else { 0 };
                    match params[param_index] {
                        ValueType::I32 => wasm_args.push(WasmValue::I32(val)),
                        ValueType::I64 => wasm_args.push(WasmValue::I64(val as i64)),
                        _ => return Err(RuntimeError { message: "Wasm bool expects i32/i64 param".to_string(), line }),
                    }
                    arg_index += 1;
                    param_index += 1;
                }
                Value::Float { value, .. } => {
                    match params[param_index] {
                        ValueType::F32 => wasm_args.push(WasmValue::F32((*value as f32).into())),
                        ValueType::F64 => wasm_args.push(WasmValue::F64((*value).into())),
                        _ => return Err(RuntimeError { message: "Wasm float expects f32/f64 param".to_string(), line }),
                    }
                    arg_index += 1;
                    param_index += 1;
                }
                v => {
                    let num = int_value_as_i64(v).ok_or_else(|| RuntimeError {
                        message: "Wasm numeric arguments must be numbers".to_string(),
                        line,
                    })?;
                    match params[param_index] {
                        ValueType::I32 => wasm_args.push(WasmValue::I32(num as i32)),
                        ValueType::I64 => wasm_args.push(WasmValue::I64(num as i64)),
                        ValueType::F32 => wasm_args.push(WasmValue::F32((num as f32).into())),
                        ValueType::F64 => wasm_args.push(WasmValue::F64((num as f64).into())),
                        _ => return Err(RuntimeError { message: "Unsupported wasm param type".to_string(), line }),
                    }
                    arg_index += 1;
                    param_index += 1;
                }
            }
        }
        if param_index != params.len() {
            return Err(RuntimeError {
                message: format!(
                    "Wasm function argument count mismatch for '{}': expected {} params ({:?}), got {}",
                    func.name,
                    params.len(),
                    params,
                    param_index
                ),
                line,
            });
        }

        let mut wasm_results = vec![WasmValue::I32(0); results.len()];
        wasm_func
            .call(&mut module.store, &wasm_args, &mut wasm_results)
            .map_err(|e| RuntimeError { message: format!("Wasm call failed: {}", e), line })?;

        let (dealloc, dealloc_name) = if let Some(func) = module.dealloc {
            (Some(func), "dealloc")
        } else if let Some(func) = module.wbindgen_free {
            (Some(func), "__wbindgen_free")
        } else {
            (None, "")
        };
        if let Some(dealloc) = dealloc {
            for (ptr, len) in allocs {
                let dealloc_params = module.func_types.get(&intern::intern(dealloc_name)).map(|t| t.params().len()).unwrap_or(2);
                let args = if dealloc_params == 3 {
                    vec![WasmValue::I32(ptr), WasmValue::I32(len), WasmValue::I32(1)]
                } else {
                    vec![WasmValue::I32(ptr), WasmValue::I32(len)]
                };
                let _ = dealloc.call(&mut module.store, &args, &mut []);
            }
        }

        if let Some(ptr) = retptr {
            let memory = module.memory.ok_or_else(|| RuntimeError {
                message: "Wasm module has no memory export".to_string(),
                line,
            })?;
            let result_str = {
                let mem = memory.data(&module.store);
                let start = ptr as usize;
                if start + 8 > mem.len() {
                    return Err(RuntimeError { message: "Wasm memory overflow reading return".to_string(), line });
                }
                let ptr_bytes = &mem[start..start + 4];
                let len_bytes = &mem[start + 4..start + 8];
                let str_ptr = u32::from_le_bytes([ptr_bytes[0], ptr_bytes[1], ptr_bytes[2], ptr_bytes[3]]);
                let str_len = u32::from_le_bytes([len_bytes[0], len_bytes[1], len_bytes[2], len_bytes[3]]);
                let start = str_ptr as usize;
                let end = start + str_len as usize;
                if end > mem.len() {
                    return Err(RuntimeError { message: "Wasm memory overflow reading string".to_string(), line });
                }
                Some((String::from_utf8_lossy(&mem[start..end]).to_string(), str_ptr, str_len))
            };
            if let Some(add) = module.wbindgen_add_to_stack_pointer {
                let _ = add.call(&mut module.store, &[WasmValue::I32(16)], &mut []);
            }
            if let Some(dealloc) = dealloc {
                if let Some((ptr, len)) = retptr_alloc {
                    let dealloc_params = module.func_types.get(&intern::intern(dealloc_name)).map(|t| t.params().len()).unwrap_or(2);
                    let args = if dealloc_params == 3 {
                        vec![WasmValue::I32(ptr), WasmValue::I32(len), WasmValue::I32(1)]
                    } else {
                        vec![WasmValue::I32(ptr), WasmValue::I32(len)]
                    };
                    let _ = dealloc.call(&mut module.store, &args, &mut []);
                }
            }
            if let Some((s, str_ptr, str_len)) = result_str {
                if let Some(dealloc) = dealloc {
                    let dealloc_params = module.func_types.get(&intern::intern(dealloc_name)).map(|t| t.params().len()).unwrap_or(2);
                    let args = if dealloc_params == 3 {
                        vec![WasmValue::I32(str_ptr as i32), WasmValue::I32(str_len as i32), WasmValue::I32(1)]
                    } else {
                        vec![WasmValue::I32(str_ptr as i32), WasmValue::I32(str_len as i32)]
                    };
                    let _ = dealloc.call(&mut module.store, &args, &mut []);
                }
                return Ok(Value::String(intern::intern_owned(s)));
            }
        }

        if results.is_empty() {
            return Ok(Value::Nil);
        }
        let wasm_result = wasm_results.get(0).cloned().unwrap_or(WasmValue::I32(0));
        match (results[0], wasm_result) {
            (ValueType::I32, WasmValue::I32(v)) => Ok(make_signed_int(v as i128, IntKind::I32)),
            (ValueType::I64, WasmValue::I64(v)) => {
                if module.memory.is_some() && func.name.ends_with("_str") {
                    let ptr = (v & 0xFFFF_FFFF) as u32;
                    let len = ((v >> 32) & 0xFFFF_FFFF) as u32;
                    let memory = module.memory.ok_or_else(|| RuntimeError {
                        message: "Wasm module has no memory export".to_string(),
                        line,
                    })?;
                    let mem = memory.data(&module.store);
                    let start = ptr as usize;
                    let end = start + len as usize;
                    if end > mem.len() {
                        return Err(RuntimeError { message: "Wasm memory overflow reading string".to_string(), line });
                    }
                    let s = String::from_utf8_lossy(&mem[start..end]).to_string();
                    Ok(Value::String(intern::intern_owned(s)))
                } else {
                    Ok(make_signed_int(v as i128, IntKind::I64))
                }
            }
            (ValueType::F32, WasmValue::F32(bits)) => Ok(make_float(f32::from(bits) as f64, FloatKind::F32)),
            (ValueType::F64, WasmValue::F64(bits)) => Ok(make_float(f64::from(bits), FloatKind::F64)),
            _ => Ok(Value::Nil),
        }
    }

    fn invoke_function(
        &mut self,
        data: Rc<crate::value::FunctionData>,
        arg_vals: smallvec::SmallVec<[Value; 8]>,
        line: usize,
        block: Option<Closure>,
    ) -> EvalResult {
        if arg_vals.len() < data.params.len() {
            let new_env = self.get_env(Some(data.env.clone()), true);
                        for ((param, _), val) in data.params.iter().zip(arg_vals.iter()) {
                             new_env.borrow_mut().define(*param, val.clone());
                        }
            let num_bound = arg_vals.len();
            let remaining_params = data.params[num_bound..].to_vec();
                        return Ok(Value::Function(Rc::new(crate::value::FunctionData {
                            params: remaining_params,
                            body: data.body.clone(),
                            declarations: data.declarations.clone(),
                            param_offset: data.param_offset + num_bound,
                            is_simple: data.is_simple,
                            uses_env: data.uses_env,
                            code: data.code.clone(),
                            const_pool: data.const_pool.clone(),
                            env: new_env,
                        })));
        } else if arg_vals.len() > data.params.len() {
             return Err(RuntimeError { message: "Too many arguments".to_string(), line });
        }

        // FULL CALL
        if let Some(code) = &data.code {
            let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(Value::Uninitialized, data.declarations.len());
            for (i, val) in arg_vals.into_iter().enumerate() {
                new_slots[i + data.param_offset] = val;
            }
            return execute_instructions(self, code, &data.const_pool, &mut new_slots);
        }

        if data.is_simple {
            let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(Value::Uninitialized, data.declarations.len());
            for (i, val) in arg_vals.into_iter().enumerate() {
                new_slots[i + data.param_offset] = val;
            }
            if data.uses_env {
                let original_env = self.env.clone();
                self.env = data.env.clone();
                let result = self.eval(&data.body, &mut new_slots)?;
                self.env = original_env;
                return Ok(result);
            }
            return self.eval(&data.body, &mut new_slots);
        }

        let block_entry = if let Some(closure) = block {
            Some((closure, self.env.clone()))
        } else {
            None
        };
        self.block_stack.push(block_entry);

        let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(Value::Uninitialized, data.declarations.len());
        for (i, val) in arg_vals.into_iter().enumerate() {
            new_slots[i + data.param_offset] = val;
        }

        let result = if data.uses_env {
            let new_env = self.get_env(Some(data.env.clone()), false);
            let original_env = self.env.clone();
            self.env = new_env.clone();
            let result = self.eval(&data.body, &mut new_slots)?;
            self.env = original_env;
            self.recycle_env(new_env);
            result
        } else {
            self.eval(&data.body, &mut new_slots)?
        };
        self.block_stack.pop();
        Ok(result)
    }

    pub fn eval(&mut self, expr: &Expr, slots: &mut [Value]) -> EvalResult {
        let line = expr.line;
        match &expr.kind {
            ExprKind::Integer { value, kind } => Ok(make_signed_int(*value, *kind)),
            ExprKind::Unsigned { value, kind } => Ok(make_unsigned_int(*value, *kind)),
            ExprKind::Float { value, kind } => Ok(make_float(*value, *kind)),
            ExprKind::String(s) => Ok(Value::String(s.clone())),
            ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
            ExprKind::Nil => Ok(Value::Nil),
            ExprKind::Use(path) => {
                self.import_path(path, line)?;
                Ok(Value::Nil)
            }
            ExprKind::Load(path) => {
                self.load_wasm_module(path, line)?;
                Ok(Value::Nil)
            }
            ExprKind::FormatString(parts) => {
                let mut out = String::new();
                for part in parts {
                    match part {
                        crate::ast::FormatPart::Literal(s) => out.push_str(s.as_str()),
                        crate::ast::FormatPart::Expr { expr, spec } => {
                            let val = self.eval(expr, slots)?;
                            if let Some(spec) = spec {
                                if let Some(precision) = spec.precision {
                                    let formatted = match &val {
                                        Value::Float { value, .. } => format!("{:.p$}", value, p = precision),
                                        v if int_value_as_f64(v).is_some() => {
                                            let num = int_value_as_f64(v).unwrap_or(0.0);
                                            format!("{:.p$}", num, p = precision)
                                        }
                                        _ => val.to_string(),
                                    };
                                    out.push_str(&formatted);
                                    continue;
                                }
                            }
                            out.push_str(&val.to_string());
                        }
                    }
                }
                Ok(Value::String(intern::intern_owned(out)))
            }
            ExprKind::Shell(cmd_str) => {
                let output = if cfg!(target_os = "windows") {
                    Command::new("cmd").args(&["/C", cmd_str.as_str()]).output()
                } else {
                    Command::new("sh").arg("-c").arg(cmd_str.as_str()).output()
                };

                match output {
                    Ok(o) => {
                        let res = String::from_utf8_lossy(&o.stdout).to_string();
                        Ok(Value::String(intern::intern_owned(res.trim().to_string())))
                    }
                    Err(_) => Ok(Value::String(intern::intern_owned("".to_string()))),
                }
            }
            ExprKind::Identifier { name, slot } => {
                if let Some(s) = slot {
                    if let Some(val) = slots.get(*s) {
                        if let Value::Uninitialized = val {
                            // Fallback to name-based lookup (e.g., for currying)
                        } else {
                            return Ok(val.clone());
                        }
                    }
                }
                let val = self.env.borrow().get(*name).ok_or_else(|| RuntimeError {
                    message: format!("Undefined variable: {}", symbol_name(*name).as_str()),
                    line,
                })?;
                if let Value::Uninitialized = val {
                    return Err(RuntimeError {
                        message: format!("Variable '{}' used before assignment", symbol_name(*name).as_str()),
                        line,
                    });
                }
                Ok(val)
            }
            ExprKind::Reference(name) => {
                let val = self.env.borrow_mut().promote(*name).ok_or_else(|| RuntimeError {
                    message: format!("Undefined variable referenced: {}", symbol_name(*name).as_str()),
                    line,
                })?;
                Ok(val)
            }
            ExprKind::Assignment { name, value, slot } => {
                let val = self.eval(value, slots)?;
                if let Some(s) = slot {
                    if let Some(slot_val) = slots.get_mut(*s) {
                        *slot_val = val.clone();
                    }
                } else {
                    self.env.borrow_mut().set(*name, val.clone());
                }
                Ok(val)
            }
            ExprKind::IndexAssignment { target, index, value } => {
                let target_val = self.eval(target, slots)?;
                let index_val = self.eval(index, slots)?;
                let val = self.eval(value, slots)?;

                match target_val {
                    Value::Array(arr) => {
                        if let Some(i) = int_value_as_usize(&index_val) {
                            let mut vec = arr.borrow_mut();
                            if i < vec.len() {
                                vec[i] = val.clone();
                            } else {
                                return Err(RuntimeError {
                                    message: "Array index out of bounds".to_string(),
                                    line,
                                });
                            }
                        } else {
                            return Err(RuntimeError {
                                message: "Array index must be an integer".to_string(),
                                line,
                            });
                        }
                    }
                    Value::F64Array(arr) => {
                        if let Some(i) = int_value_as_usize(&index_val) {
                            let mut vec = arr.borrow_mut();
                            if i < vec.len() {
                                match &val {
                                    Value::Float { value, .. } => vec[i] = *value,
                                    v => {
                                        if let Some(num) = int_value_as_f64(v) {
                                            vec[i] = num;
                                        } else {
                                            return Err(RuntimeError {
                                                message: "F64Array assignment requires a number".to_string(),
                                                line,
                                            });
                                        }
                                    }
                                }
                            } else {
                                return Err(RuntimeError {
                                    message: "Array index out of bounds".to_string(),
                                    line,
                                });
                            }
                        } else {
                            return Err(RuntimeError {
                                message: "Array index must be an integer".to_string(),
                                line,
                            });
                        }
                    }
                    Value::Map(map) => {
                        let key = match index_val {
                             Value::String(s) => s,
                             _ => intern::intern_owned(index_val.inspect()),
                        };
                        map.borrow_mut().insert(key, val.clone());
                    }
                    _ => return Err(RuntimeError {
                        message: "Index assignment not supported on this type".to_string(),
                        line,
                    }),
                }
                Ok(val)
            }
        ExprKind::FunctionDef { name, params, body, slots } => {
            let func_env = self.env.clone();
            let (resolved_body, slot_names) = if let Some(slot_names) = slots {
                (body.clone(), slot_names.clone())
            } else {
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);
                let (slot_map, slot_names) = build_slot_map(params, locals);
                let mut resolved = body.clone();
                resolve(resolved.as_mut(), &slot_map);
                (resolved, Rc::new(slot_names))
            };
            let simple = is_simple(&resolved_body);
            let uses_env = uses_environment(&resolved_body);
                
            let mut code = Vec::new();
            let mut const_pool = Vec::new();
            let compiled = if simple { compile_expr(&resolved_body, &mut code, &mut const_pool, true) } else { false };

            let func = Value::Function(Rc::new(crate::value::FunctionData {
                params: params.clone(),
                body: *resolved_body,
                declarations: slot_names,
                param_offset: 0,
                is_simple: simple,
                uses_env,
                code: if compiled { Some(Rc::new(code)) } else { None },
                const_pool: Rc::new(const_pool),
                env: func_env,
            }));
                self.env.borrow_mut().define(*name, func.clone());
                Ok(func)
            }
        ExprKind::AnonymousFunction { params, body, slots } => {
            let func_env = self.env.clone();
            let (resolved_body, slot_names) = if let Some(slot_names) = slots {
                (body.clone(), slot_names.clone())
            } else {
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);
                let (slot_map, slot_names) = build_slot_map(params, locals);
                let mut resolved = body.clone();
                resolve(resolved.as_mut(), &slot_map);
                (resolved, Rc::new(slot_names))
            };
            let simple = is_simple(&resolved_body);
            let uses_env = uses_environment(&resolved_body);
                
            let mut code = Vec::new();
            let mut const_pool = Vec::new();
            let compiled = if simple { compile_expr(&resolved_body, &mut code, &mut const_pool, true) } else { false };

                Ok(Value::Function(Rc::new(crate::value::FunctionData {
                    params: params.clone(),
                    body: *resolved_body,
                    declarations: slot_names,
                    param_offset: 0,
                    is_simple: simple,
                    uses_env,
                    code: if compiled { Some(Rc::new(code)) } else { None },
                    const_pool: Rc::new(const_pool),
                    env: func_env,
                })))
            }
            ExprKind::Yield(args) => {
                let block_data = self.block_stack.last().cloned();
                
                if let Some(Some((closure, saved_env))) = block_data {
                     let mut arg_vals = Vec::new();
                     for a in args {
                         arg_vals.push(self.eval(a, slots)?);
                     }
                     
                     let new_env = self.get_env(Some(saved_env.clone()), false);
                     
                     let mut arg_iter = arg_vals.into_iter();
                     for (param_name, is_ref) in &closure.params {
                         if *is_ref {
                             let ref_val = saved_env.borrow_mut().promote(*param_name)
                                 .ok_or_else(|| RuntimeError {
                                     message: format!("Undefined variable captured: {}", symbol_name(*param_name).as_str()),
                                     line,
                                 })?;
                             new_env.borrow_mut().define(*param_name, ref_val);
                         } else {
                             let val = arg_iter.next().unwrap_or(Value::Nil);
                             new_env.borrow_mut().define(*param_name, val);
                         }
                     }
                     
                     let mut locals = HashSet::new();
                     collect_declarations(&closure.body, &mut locals);
                     for local in locals {
                         let idx = local as usize;
                         if idx >= new_env.borrow().values.len() {
                             new_env.borrow_mut().define(local, Value::Uninitialized);
                         }
                     }
                     
                     let original_env = self.env.clone();
                     self.env = new_env.clone();
                     let result = self.eval(&closure.body, &mut []);
                     self.env = original_env;
                     self.recycle_env(new_env);
                     result
                } else {
                    Err(RuntimeError {
                        message: "No block given for yield".to_string(),
                        line,
                    })
                }
            }
            ExprKind::Array(elements) => {
                let mut vals = Vec::new();
                let mut f64_vals: Vec<f64> = Vec::new();
                let mut all_f64 = true;
                for e in elements {
                    let v = self.eval(e, slots)?;
                    if all_f64 {
                        match v {
                            Value::Float { value, .. } => f64_vals.push(value),
                            v => {
                                if let Some(num) = int_value_as_f64(&v) {
                                    f64_vals.push(num);
                                } else {
                                    all_f64 = false;
                                    vals.extend(f64_vals.drain(..).map(|value| make_float(value, FloatKind::F64)));
                                    vals.push(v);
                                }
                            }
                        }
                    } else {
                        vals.push(v);
                    }
                }
                if all_f64 {
                    Ok(Value::F64Array(Rc::new(RefCell::new(f64_vals))))
                } else {
                    Ok(Value::Array(Rc::new(RefCell::new(vals))))
                }
            }
            ExprKind::ArrayGenerator { generator, size } => {
                let gen_val = self.eval(generator, slots)?;
                let size_val = self.eval(size, slots)?;
                let n = int_value_as_usize(&size_val).ok_or_else(|| RuntimeError {
                    message: "Array size must be a non-negative integer".to_string(),
                    line,
                })?;
                let mut vals: Vec<Value> = Vec::new();
                let mut f64_vals: Vec<f64> = Vec::new();
                let mut all_f64 = true;
                if let Value::Function(data) = gen_val {
                    for i in 0..n {
                        let mut new_slots = smallvec::SmallVec::<[Value; 8]>::from_elem(Value::Uninitialized, data.declarations.len());
                        if data.params.len() > 0 {
                             new_slots[data.param_offset] = default_int(i as i128);
                        }
                        
                        let result = if let Some(code) = &data.code {
                            execute_instructions(self, code, &data.const_pool, &mut new_slots)?
                        } else if data.uses_env {
                            let new_env = self.get_env(Some(data.env.clone()), false);
                            let original_env = self.env.clone();
                            self.env = new_env.clone();
                            let result = self.eval(&data.body, &mut new_slots)?;
                            self.env = original_env;
                            self.recycle_env(new_env);
                            result
                        } else {
                            self.eval(&data.body, &mut new_slots)?
                        };
                        if all_f64 {
                            match &result {
                                Value::Float { value, .. } => f64_vals.push(*value),
                                v => {
                                    if let Some(num) = int_value_as_f64(v) {
                                        f64_vals.push(num);
                                    } else {
                                        all_f64 = false;
                                        vals.extend(f64_vals.drain(..).map(|value| make_float(value, FloatKind::F64)));
                                        vals.push(result);
                                    }
                                }
                            }
                        } else {
                            vals.push(result);
                        }
                    }
                } else {
                    for _ in 0..n {
                        if all_f64 {
                            match &gen_val {
                                Value::Float { value, .. } => f64_vals.push(*value),
                                v => {
                                    if let Some(num) = int_value_as_f64(v) {
                                        f64_vals.push(num);
                                    } else {
                                        all_f64 = false;
                                        vals.extend(f64_vals.drain(..).map(|value| make_float(value, FloatKind::F64)));
                                        vals.push(gen_val.clone());
                                    }
                                }
                            }
                        } else {
                            vals.push(gen_val.clone());
                        }
                    }
                }
                if all_f64 {
                    Ok(Value::F64Array(Rc::new(RefCell::new(f64_vals))))
                } else {
                    Ok(Value::Array(Rc::new(RefCell::new(vals))))
                }
            }
            ExprKind::Map(entries) => {
                let mut map = FxHashMap::default();
                for (k_expr, v_expr) in entries {
                    let k_val = self.eval(k_expr, slots)?;
                    let v_val = self.eval(v_expr, slots)?;
                    let k_str = match k_val {
                        Value::String(s) => s,
                        _ => intern::intern_owned(k_val.inspect()),
                    };
                    map.insert(k_str, v_val);
                }
                Ok(Value::Map(Rc::new(RefCell::new(map))))
            }
            ExprKind::Index { target, index } => {
                let target_val = self.eval(target, slots)?;
                let index_val = self.eval(index, slots)?;
                match target_val {
                    Value::Array(arr) => {
                        if let Some(i) = int_value_as_usize(&index_val) {
                            let vec = arr.borrow();
                            if i < vec.len() {
                                Ok(vec[i].clone())
                            } else {
                                Ok(Value::Nil)
                            }
                        } else {
                            Err(RuntimeError {
                                message: "Array index must be an integer".to_string(),
                                line,
                            })
                        }
                    }
                    Value::F64Array(arr) => {
                        if let Some(i) = int_value_as_usize(&index_val) {
                            let vec = arr.borrow();
                            if i < vec.len() {
                                Ok(make_float(vec[i], FloatKind::F64))
                            } else {
                                Ok(Value::Nil)
                            }
                        } else {
                            Err(RuntimeError {
                                message: "Array index must be an integer".to_string(),
                                line,
                            })
                        }
                    }
                    Value::Map(map) => {
                        let key = match index_val {
                             Value::String(s) => s,
                             _ => intern::intern_owned(index_val.inspect()),
                        };
                        Ok(map.borrow().get(&key).cloned().unwrap_or(Value::Nil))
                    }
                    _ => Err(RuntimeError {
                        message: "Index operator not supported on this type".to_string(),
                        line,
                    }),
                }
            }
            ExprKind::Call { function, args, block, inlined_body } => {
                // 1. Check Cached Inlined Body
                if let Some(inlined) = inlined_body.borrow().as_ref() {
                    return self.eval(inlined, slots);
                }

                if let ExprKind::Identifier { name, .. } = &function.kind {
                     match symbol_name(*name).as_str() {
                        "puts" | "print" => {
                            let mut last_val = Value::Nil;
                            for arg in args {
                                let val = self.eval(arg, slots)?;
                                if symbol_name(*name).as_str() == "puts" {
                                    println!("{}", val);
                                } else {
                                    print!("{}", val);
                                    io::stdout().flush().unwrap();
                                }
                                last_val = val;
                            }
                            return Ok(last_val);
                        }
                        "len" => {
                            let val = self.eval(&args[0], slots)?;
                            return match val {
                                Value::String(s) => Ok(default_int(s.len() as i128)),
                                Value::Array(arr) => Ok(default_int(arr.borrow().len() as i128)),
                                Value::F64Array(arr) => Ok(default_int(arr.borrow().len() as i128)),
                                Value::Map(map) => Ok(default_int(map.borrow().len() as i128)),
                                _ => Ok(default_int(0)),
                            };
                        }
                        "read_file" => {
                            let path = self.eval(&args[0], slots)?.to_string();
                            return match fs::read_to_string(&path) {
                                Ok(content) => Ok(Value::String(intern::intern_owned(content))),
                                Err(_) => Ok(Value::Nil),
                            };
                        }
                        "write_file" => {
                            let path = self.eval(&args[0], slots)?.to_string();
                            let content = self.eval(&args[1], slots)?.to_string();
                            return match fs::File::create(&path) {
                                Ok(mut file) => {
                                    write!(file, "{}", content).unwrap();
                                    Ok(Value::Boolean(true))
                                }
                                Err(_) => Ok(Value::Boolean(false)),
                            };
                        }
                        _ => {}
                     }
                }

                let func_val = self.eval(function, slots)?;
                match func_val {
                    Value::Function(data) => {
                        // 2. Attempt JIT Inlining
                        // Inline if:
                        // - Function is simple (no locals/assignments).
                        // - Function does not capture environment (no slot=None identifiers).
                        // - Args are simple expressions (Identifiers/Literals) to avoid code explosion or side-effect duplication.
                        if data.is_simple && inlined_body.borrow().is_none() && !data.uses_env {
                            let small_body = expr_size(&data.body) <= 40;
                            let safe_args = args.iter().all(is_inline_safe_arg);
                            if safe_args && small_body {
                                let inlined = substitute(&data.body, args);
                                inlined_body.replace(Some(inlined));
                                // Run the newly minted inlined body immediately
                                return self.eval(inlined_body.borrow().as_ref().unwrap(), slots);
                            }
                        }

                        let mut arg_vals: smallvec::SmallVec<[Value; 8]> = smallvec::SmallVec::new();
                        for (i, arg_expr) in args.iter().enumerate() {
                            let val = self.eval(arg_expr, slots)?;
                            if i < data.params.len() {
                                let (_, is_ref) = data.params[i];
                                if is_ref {
                                    if let Value::Reference(_) = val {
                                        arg_vals.push(val);
                                    } else {
                                        return Err(RuntimeError {
                                            message: format!("Argument #{} expected to be a reference (&var), but got value", i + 1),
                                            line,
                                        });
                                    }
                                } else {
                                    arg_vals.push(val);
                                }
                            } else {
                                arg_vals.push(val);
                            }
                        }

                        self.invoke_function(data, arg_vals, line, block.clone())
                    }
                    Value::NativeFunction(func) => {
                        if block.is_some() {
                            return Err(RuntimeError {
                                message: "Native function does not accept a block".to_string(),
                                line,
                            });
                        }
                        let mut arg_vals: smallvec::SmallVec<[Value; 8]> = smallvec::SmallVec::new();
                        for arg_expr in args {
                            arg_vals.push(self.eval(arg_expr, slots)?);
                        }
                        func(&arg_vals).map_err(|message| RuntimeError { message, line })
                    }
                    Value::WasmFunction(func) => {
                        if block.is_some() {
                            return Err(RuntimeError {
                                message: "Wasm function does not accept a block".to_string(),
                                line,
                            });
                        }
                        let mut arg_vals: smallvec::SmallVec<[Value; 8]> = smallvec::SmallVec::new();
                        for arg_expr in args {
                            arg_vals.push(self.eval(arg_expr, slots)?);
                        }
                        self.call_wasm_function(func, arg_vals, line)
                    }
                    _ => Err(RuntimeError { message: format!("Tried to call a non-function value: {}", func_val), line }),
                }
            }
            ExprKind::BinaryOp { left, op, right } => {
                let l = match &left.kind {
                    ExprKind::Integer { value, kind } => make_signed_int(*value, *kind),
                    ExprKind::Unsigned { value, kind } => make_unsigned_int(*value, *kind),
                    ExprKind::Float { value, kind } => make_float(*value, *kind),
                    ExprKind::Identifier { slot: Some(s), .. } => {
                        let v = &slots[*s];
                        if let Value::Uninitialized = v { self.eval(left, slots)? } else { v.clone() }
                    }
                    _ => self.eval(left, slots)?,
                };
                let r = match &right.kind {
                    ExprKind::Integer { value, kind } => make_signed_int(*value, *kind),
                    ExprKind::Unsigned { value, kind } => make_unsigned_int(*value, *kind),
                    ExprKind::Float { value, kind } => make_float(*value, *kind),
                    ExprKind::Identifier { slot: Some(s), .. } => {
                        let v = &slots[*s];
                        if let Value::Uninitialized = v { self.eval(right, slots)? } else { v.clone() }
                    }
                    _ => self.eval(right, slots)?,
                };
                match (l, r) {
                    (Value::Integer { value: i1, kind: k1 }, Value::Integer { value: i2, kind: k2 }) => {
                        let kind = signed_kind_for_bits(int_kind_bits(k1).max(int_kind_bits(k2)));
                        match op {
                            Op::Add => Ok(make_signed_int(i1 + i2, kind)),
                            Op::Subtract => Ok(make_signed_int(i1 - i2, kind)),
                            Op::Multiply => Ok(make_signed_int(i1 * i2, kind)),
                            Op::Divide => Ok(make_signed_int(i1 / i2, kind)),
                            Op::GreaterThan => Ok(Value::Boolean(i1 > i2)),
                            Op::LessThan => Ok(Value::Boolean(i1 < i2)),
                            Op::Equal => Ok(Value::Boolean(i1 == i2)),
                            Op::NotEqual => Ok(Value::Boolean(i1 != i2)),
                        }
                    }
                    (Value::Unsigned { value: u1, kind: k1 }, Value::Unsigned { value: u2, kind: k2 }) => {
                        let kind = unsigned_kind_for_bits(int_kind_bits(k1).max(int_kind_bits(k2)));
                        match op {
                            Op::Add => Ok(make_unsigned_int(u1 + u2, kind)),
                            Op::Subtract => Ok(make_unsigned_int(u1 - u2, kind)),
                            Op::Multiply => Ok(make_unsigned_int(u1 * u2, kind)),
                            Op::Divide => Ok(make_unsigned_int(u1 / u2, kind)),
                            Op::GreaterThan => Ok(Value::Boolean(u1 > u2)),
                            Op::LessThan => Ok(Value::Boolean(u1 < u2)),
                            Op::Equal => Ok(Value::Boolean(u1 == u2)),
                            Op::NotEqual => Ok(Value::Boolean(u1 != u2)),
                        }
                    }
                    (Value::Integer { value: i1, .. }, Value::Unsigned { value: u2, .. }) => {
                        let u2_i = i128::try_from(u2).map_err(|_| RuntimeError {
                            message: "Unsigned value too large for signed operation".to_string(),
                            line,
                        })?;
                        let kind = IntKind::I128;
                        match op {
                            Op::Add => Ok(make_signed_int(i1 + u2_i, kind)),
                            Op::Subtract => Ok(make_signed_int(i1 - u2_i, kind)),
                            Op::Multiply => Ok(make_signed_int(i1 * u2_i, kind)),
                            Op::Divide => Ok(make_signed_int(i1 / u2_i, kind)),
                            Op::GreaterThan => Ok(Value::Boolean(i1 > u2_i)),
                            Op::LessThan => Ok(Value::Boolean(i1 < u2_i)),
                            Op::Equal => Ok(Value::Boolean(i1 == u2_i)),
                            Op::NotEqual => Ok(Value::Boolean(i1 != u2_i)),
                        }
                    }
                    (Value::Unsigned { value: u1, .. }, Value::Integer { value: i2, .. }) => {
                        let u1_i = i128::try_from(u1).map_err(|_| RuntimeError {
                            message: "Unsigned value too large for signed operation".to_string(),
                            line,
                        })?;
                        let kind = IntKind::I128;
                        match op {
                            Op::Add => Ok(make_signed_int(u1_i + i2, kind)),
                            Op::Subtract => Ok(make_signed_int(u1_i - i2, kind)),
                            Op::Multiply => Ok(make_signed_int(u1_i * i2, kind)),
                            Op::Divide => Ok(make_signed_int(u1_i / i2, kind)),
                            Op::GreaterThan => Ok(Value::Boolean(u1_i > i2)),
                            Op::LessThan => Ok(Value::Boolean(u1_i < i2)),
                            Op::Equal => Ok(Value::Boolean(u1_i == i2)),
                            Op::NotEqual => Ok(Value::Boolean(u1_i != i2)),
                        }
                    }
                    (Value::Float { value: f1, kind: k1 }, Value::Float { value: f2, kind: k2 }) => {
                        let kind = promote_float_kind(k1, k2);
                        match op {
                            Op::Add => Ok(make_float(f1 + f2, kind)),
                            Op::Subtract => Ok(make_float(f1 - f2, kind)),
                            Op::Multiply => Ok(make_float(f1 * f2, kind)),
                            Op::Divide => Ok(make_float(f1 / f2, kind)),
                            Op::GreaterThan => Ok(Value::Boolean(f1 > f2)),
                            Op::LessThan => Ok(Value::Boolean(f1 < f2)),
                            Op::Equal => Ok(Value::Boolean(f1 == f2)),
                            Op::NotEqual => Ok(Value::Boolean(f1 != f2)),
                        }
                    }
                    (v @ Value::Integer { .. }, Value::Float { value: f, kind })
                    | (v @ Value::Unsigned { .. }, Value::Float { value: f, kind }) => {
                        let f1 = int_value_as_f64(&v).unwrap_or(0.0);
                        match op {
                            Op::Add => Ok(make_float(f1 + f, kind)),
                            Op::Subtract => Ok(make_float(f1 - f, kind)),
                            Op::Multiply => Ok(make_float(f1 * f, kind)),
                            Op::Divide => Ok(make_float(f1 / f, kind)),
                            Op::GreaterThan => Ok(Value::Boolean(f1 > f)),
                            Op::LessThan => Ok(Value::Boolean(f1 < f)),
                            Op::Equal => Ok(Value::Boolean(f1 == f)),
                            Op::NotEqual => Ok(Value::Boolean(f1 != f)),
                        }
                    }
                    (Value::Float { value: f, kind }, v @ Value::Integer { .. })
                    | (Value::Float { value: f, kind }, v @ Value::Unsigned { .. }) => {
                        let f2 = int_value_as_f64(&v).unwrap_or(0.0);
                        match op {
                            Op::Add => Ok(make_float(f + f2, kind)),
                            Op::Subtract => Ok(make_float(f - f2, kind)),
                            Op::Multiply => Ok(make_float(f * f2, kind)),
                            Op::Divide => Ok(make_float(f / f2, kind)),
                            Op::GreaterThan => Ok(Value::Boolean(f > f2)),
                            Op::LessThan => Ok(Value::Boolean(f < f2)),
                            Op::Equal => Ok(Value::Boolean(f == f2)),
                            Op::NotEqual => Ok(Value::Boolean(f != f2)),
                        }
                    }
                    (Value::String(s1), Value::String(s2)) => match op {
                        Op::Add => {
                            let mut out = s1.clone();
                            Rc::make_mut(&mut out).push_str(&s2);
                            Ok(Value::String(out))
                        }
                        Op::Equal => Ok(Value::Boolean(s1 == s2)),
                        Op::NotEqual => Ok(Value::Boolean(s1 != s2)),
                        _ => Err(RuntimeError { message: "Invalid operation on two strings".to_string(), line }),
                    },
                    (Value::String(s), v2) => match op {
                        Op::Add => {
                            let mut out = s.clone();
                            Rc::make_mut(&mut out).push_str(&v2.inspect());
                            Ok(Value::String(out))
                        }
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError { message: format!("Invalid operation between String and {:?}", v2), line }),
                    },
                    (v1, v2) => match op {
                        Op::Equal => Ok(Value::Boolean(false)),
                        Op::NotEqual => Ok(Value::Boolean(true)),
                        _ => Err(RuntimeError { message: format!("Type mismatch: Cannot operate {:?} on {:?} and {:?}", op, v1, v2), line }),
                    },
                }
            }
            ExprKind::If { condition, then_branch, else_branch } => {
                let val = self.eval(condition, slots)?;
                let is_truthy = match val { Value::Boolean(false) | Value::Nil => false, _ => true };
                if is_truthy { self.eval(then_branch, slots) } else if let Some(else_expr) = else_branch { self.eval(else_expr, slots) } else { Ok(Value::Nil) }
            }
            ExprKind::While { condition, body } => {
                let mut last_val = Value::Nil;
                loop {
                    let cond_val = self.eval(condition, slots)?;
                    let is_true = match cond_val { Value::Boolean(false) | Value::Nil => false, _ => true };
                    if !is_true { break; }
                    last_val = self.eval(body, slots)?;
                }
                Ok(last_val)
            }
            ExprKind::For { var, iterable, body, .. } => {
                let iter_val = self.eval(iterable, slots)?;
                let mut last_val = Value::Nil;
                match iter_val {
                    Value::Array(arr) => {
                        let len = arr.borrow().len();
                        for idx in 0..len {
                            let item = {
                                let vec = arr.borrow();
                                vec[idx].clone()
                            };
                            self.env.borrow_mut().assign(*var, item);
                            last_val = self.eval(body, slots)?;
                        }
                        Ok(last_val)
                    }
                    Value::F64Array(arr) => {
                        let len = arr.borrow().len();
                        for idx in 0..len {
                            let item = {
                                let vec = arr.borrow();
                                make_float(vec[idx], FloatKind::F64)
                            };
                            self.env.borrow_mut().assign(*var, item);
                            last_val = self.eval(body, slots)?;
                        }
                        Ok(last_val)
                    }
                    Value::Map(map) => {
                        let map_ref = map.borrow();
                        let mut keys = Vec::with_capacity(map_ref.len());
                        keys.extend(map_ref.keys().cloned());
                        for key in keys {
                            self.env.borrow_mut().assign(*var, Value::String(key));
                            last_val = self.eval(body, slots)?;
                        }
                        Ok(last_val)
                    }
                    _ => Err(RuntimeError { message: "Type is not iterable".to_string(), line }),
                }
            }
            ExprKind::Loop { count, var, var_slot, body } => {
                let count_val = self.eval(count, slots)?;
                let n = number_to_usize(&count_val).ok_or_else(|| RuntimeError {
                    message: "Loop count must be a non-negative number".to_string(),
                    line,
                })?;
                let mut last_val = Value::Nil;
                for idx in 0..n {
                    if let Some(slot) = var_slot {
                        if let Some(slot_val) = slots.get_mut(*slot) {
                            *slot_val = default_int(idx as i128);
                        }
                    } else if let Some(name) = var {
                        self.env.borrow_mut().assign(*name, default_int(idx as i128));
                    }
                    last_val = self.eval(body, slots)?;
                }
                Ok(last_val)
            }
            ExprKind::Block(statements) => {
                let mut last = Value::Nil;
                for stmt in statements { last = self.eval(stmt, slots)?; }
                Ok(last)
            }
        }
    }
}
