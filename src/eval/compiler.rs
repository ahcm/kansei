//! Slot resolution, bytecode compilation, and compilation diagnostics.
use super::*;

pub(super) fn collect_declarations(expr: &Expr, decls: &mut HashSet<SymbolId>)
{
    match &expr.kind
    {
        ExprKind::Assignment { name, .. } =>
        {
            decls.insert(name.clone());
        }
        ExprKind::FunctionDef { name, .. } =>
        {
            decls.insert(name.clone());
        }
        ExprKind::AnonymousFunction { .. } =>
        {}
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            collect_declarations(target, decls);
            collect_declarations(index, decls);
            collect_declarations(value, decls);
        }
        ExprKind::Block(stmts) =>
        {
            for stmt in stmts
            {
                collect_declarations(stmt, decls);
            }
        }
        ExprKind::If {
            then_branch,
            else_branch,
            ..
        } =>
        {
            collect_declarations(then_branch, decls);
            if let Some(else_expr) = else_branch
            {
                collect_declarations(else_expr, decls);
            }
        }
        ExprKind::Result {
            body,
            else_expr,
            else_binding,
            ..
        } =>
        {
            if let Some(name) = else_binding
            {
                decls.insert(name.clone());
            }
            collect_declarations(body, decls);
            collect_declarations(else_expr, decls);
        }
        ExprKind::While { body, .. } =>
        {
            collect_declarations(body, decls);
        }
        ExprKind::For {
            var,
            iterable,
            body,
            ..
        } =>
        {
            decls.insert(var.clone());
            collect_declarations(iterable, decls);
            collect_declarations(body, decls);
        }
        ExprKind::Loop {
            var, count, body, ..
        } =>
        {
            if let Some(name) = var
            {
                decls.insert(name.clone());
            }
            collect_declarations(count, decls);
            collect_declarations(body, decls);
        }
        ExprKind::Collect {
            var, count, into, body, ..
        } =>
        {
            if let Some(name) = var
            {
                decls.insert(name.clone());
            }
            collect_declarations(count, decls);
            if let Some(into) = into
            {
                collect_declarations(into, decls);
            }
            collect_declarations(body, decls);
        }
        ExprKind::Array(elements) =>
        {
            for e in elements
            {
                collect_declarations(e, decls);
            }
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            collect_declarations(generator, decls);
            collect_declarations(size, decls);
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                collect_declarations(k, decls);
                collect_declarations(v, decls);
            }
        }
        ExprKind::Clone(expr) =>
        {
            collect_declarations(expr, decls);
        }
        ExprKind::ErrorRaise(expr) =>
        {
            collect_declarations(expr, decls);
        }
        ExprKind::EnvFreeze(expr) =>
        {
            collect_declarations(expr, decls);
        }
        ExprKind::FilePublic(expr) =>
        {
            collect_declarations(expr, decls);
        }
        ExprKind::FunctionPublic(expr) =>
        {
            collect_declarations(expr, decls);
        }
        ExprKind::Not(expr) =>
        {
            collect_declarations(expr, decls);
        }
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } =>
        {
            collect_declarations(left, decls);
            collect_declarations(right, decls);
        }
        ExprKind::FormatString(parts) =>
        {
            for part in parts
            {
                if let crate::ast::FormatPart::Expr { expr, .. } = part
                {
                    collect_declarations(expr, decls);
                }
            }
        }
        ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. } =>
        {}
        _ =>
        {}
    }
}

pub(super) fn build_slot_map(
    params: &[Param],
    locals: HashSet<SymbolId>,
) -> (FxHashMap<SymbolId, usize>, Vec<Rc<String>>)
{
    let mut slot_map = FxHashMap::default();
    let mut slot_names = Vec::new();
    for param in params
    {
        if !slot_map.contains_key(&param.name)
        {
            slot_map.insert(param.name, slot_names.len());
            slot_names.push(symbol_name(param.name));
        }
    }
    for l in locals
    {
        if !slot_map.contains_key(&l)
        {
            slot_map.insert(l.clone(), slot_names.len());
            slot_names.push(symbol_name(l));
        }
    }
    for param in params.iter().filter(|param| param.is_ref)
    {
        slot_map.remove(&param.name);
    }
    (slot_map, slot_names)
}

pub fn resolve_slots(expr: &mut Expr)
{
    resolve_functions(expr);
}

pub(super) fn resolve_functions(expr: &mut Expr)
{
    match &mut expr.kind
    {
        ExprKind::FunctionDef {
            params,
            body,
            slots,
            ..
        } =>
        {
            if slots.is_none()
            {
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);
                let (slot_map, slot_names) = build_slot_map(params, locals);
                resolve(body.as_mut(), &slot_map);
                *slots = Some(Rc::new(slot_names));
            }
            resolve_functions(body);
        }
        ExprKind::AnonymousFunction {
            params,
            body,
            slots,
        } =>
        {
            if slots.is_none()
            {
                let mut locals = HashSet::new();
                collect_declarations(body, &mut locals);
                let (slot_map, slot_names) = build_slot_map(params, locals);
                resolve(body.as_mut(), &slot_map);
                *slots = Some(Rc::new(slot_names));
            }
            resolve_functions(body);
        }
        ExprKind::Assignment { value, .. } => resolve_functions(value),
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            resolve_functions(target);
            resolve_functions(index);
            resolve_functions(value);
        }
        ExprKind::BinaryOp { left, right, .. } =>
        {
            resolve_functions(left);
            resolve_functions(right);
        }
        ExprKind::Not(expr) =>
        {
            resolve_functions(expr);
        }
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } =>
        {
            resolve_functions(left);
            resolve_functions(right);
        }
        ExprKind::Clone(expr) =>
        {
            resolve_functions(expr);
        }
        ExprKind::ErrorRaise(expr) =>
        {
            resolve_functions(expr);
        }
        ExprKind::EnvFreeze(expr) =>
        {
            resolve_functions(expr);
        }
        ExprKind::FilePublic(expr) =>
        {
            resolve_functions(expr.as_mut());
        }
        ExprKind::FunctionPublic(expr) =>
        {
            resolve_functions(expr.as_mut());
        }
        ExprKind::Block(stmts) =>
        {
            for stmt in stmts
            {
                resolve_functions(stmt);
            }
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            resolve_functions(condition);
            resolve_functions(then_branch);
            if let Some(eb) = else_branch
            {
                resolve_functions(eb);
            }
        }
        ExprKind::Result {
            body,
            else_expr,
            ..
        } =>
        {
            resolve_functions(body);
            resolve_functions(else_expr);
        }
        ExprKind::While { condition, body } =>
        {
            resolve_functions(condition);
            resolve_functions(body);
        }
        ExprKind::For { iterable, body, .. } =>
        {
            resolve_functions(iterable);
            resolve_functions(body);
        }
        ExprKind::Loop { count, body, .. } =>
        {
            resolve_functions(count);
            resolve_functions(body);
        }
        ExprKind::Collect { count, into, body, .. } =>
        {
            resolve_functions(count);
            if let Some(into) = into
            {
                resolve_functions(into);
            }
            resolve_functions(body);
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            resolve_functions(function);
            for arg in args
            {
                resolve_functions(arg);
            }
            if let Some(c) = block
            {
                resolve_functions(&mut c.body);
            }
        }
        ExprKind::Array(elements) =>
        {
            for e in elements
            {
                resolve_functions(e);
            }
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            resolve_functions(generator);
            resolve_functions(size);
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                resolve_functions(k);
                resolve_functions(v);
            }
        }
        ExprKind::Index { target, index } =>
        {
            resolve_functions(target);
            resolve_functions(index);
        }
        ExprKind::FormatString(parts) =>
        {
            for part in parts
            {
                if let crate::ast::FormatPart::Expr { expr, .. } = part
                {
                    resolve_functions(expr);
                }
            }
        }
        ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. } =>
        {}
        ExprKind::Yield(args) =>
        {
            for a in args
            {
                resolve_functions(a);
            }
        }
        _ =>
        {}
    }
}

pub(super) fn uses_environment(expr: &Expr) -> bool
{
    match &expr.kind
    {
        ExprKind::Identifier { slot: None, .. } => true,
        ExprKind::Identifier { .. } => false,
        ExprKind::BinaryOp { left, right, .. } => uses_environment(left) || uses_environment(right),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            uses_environment(condition)
                || uses_environment(then_branch)
                || else_branch.as_ref().map_or(false, |e| uses_environment(e))
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            uses_environment(function)
                || args.iter().any(uses_environment)
                || block.as_ref().map_or(false, |c| uses_environment(&c.body))
        }
        ExprKind::Not(expr) => uses_environment(expr),
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } => uses_environment(left) || uses_environment(right),
        ExprKind::Clone(expr) | ExprKind::EnvFreeze(expr) => uses_environment(expr),
        ExprKind::Use(_) => true,
        ExprKind::Load(_) => true,
        ExprKind::Import { .. } => true,
        ExprKind::Export { .. } => true,
        ExprKind::FilePublic(_) => true,
        ExprKind::FunctionPublic(_) => true,
        ExprKind::FormatString(parts) => parts.iter().any(|part| {
            if let crate::ast::FormatPart::Expr { expr, .. } = part
            {
                uses_environment(expr)
            }
            else
            {
                false
            }
        }),
        // Simple functions (is_simple) only have these constructs roughly.
        // We can be conservative.
        ExprKind::Integer { .. }
        | ExprKind::Unsigned { .. }
        | ExprKind::Float { .. }
        | ExprKind::String(_)
        | ExprKind::Boolean(_)
        | ExprKind::Nil => false,
        _ => true, // Conservative fallback for blocks, loops, etc. if they slipped into is_simple
    }
}

pub(super) fn builtin_from_symbol(name: SymbolId) -> Option<Builtin>
{
    match symbol_name(name).as_str()
    {
        "puts" => Some(Builtin::Puts),
        "print" => Some(Builtin::Print),
        "eputs" => Some(Builtin::Eputs),
        "eprint" => Some(Builtin::Eprint),
        "log" => Some(Builtin::Log),
        "assert" => Some(Builtin::Assert),
        "assert_eq" => Some(Builtin::AssertEq),
        "len" => Some(Builtin::Len),
        "read_file" => Some(Builtin::ReadFile),
        "write_file" => Some(Builtin::WriteFile),
        "typeof" => Some(Builtin::Typeof),
        "f64" => Some(Builtin::F64),
        "f32" => Some(Builtin::F32),
        "i64" => Some(Builtin::I64),
        "i32" => Some(Builtin::I32),
        "u64" => Some(Builtin::U64),
        "u32" => Some(Builtin::U32),
        _ => None,
    }
}

pub(super) fn push_const(consts: &mut Vec<Value>, value: Value) -> usize
{
    if let Some(idx) = consts.iter().position(|v| v == &value)
    {
        return idx;
    }
    consts.push(value);
    consts.len() - 1
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum RangeStep
{
    Int(i64),
    Float(f64, FloatKind),
}

pub(super) fn range_step_from_literal(expr: &Expr) -> Option<RangeStep>
{
    match &expr.kind
    {
        ExprKind::Integer { value, .. } =>
        {
            let step = i64::try_from(*value).ok()?;
            if step > 0
            {
                Some(RangeStep::Int(step))
            }
            else
            {
                None
            }
        }
        ExprKind::Unsigned { value, .. } =>
        {
            let step = i64::try_from(*value).ok()?;
            if step > 0
            {
                Some(RangeStep::Int(step))
            }
            else
            {
                None
            }
        }
        ExprKind::Float { value, kind } =>
        {
            if *value > 0.0
            {
                Some(RangeStep::Float(*value, *kind))
            }
            else
            {
                None
            }
        }
        _ => None,
    }
}

pub(super) fn match_for_range(
    condition: &Expr,
    body: &Expr,
    consts: &mut Vec<Value>,
) -> Option<(usize, RangeEnd, RangeStep, Expr)>
{
    let (index_slot, end) = match &condition.kind
    {
        ExprKind::BinaryOp {
            left,
            op: Op::LessThan,
            right,
        } =>
        {
            let idx_slot = match &left.kind
            {
                ExprKind::Identifier { slot: Some(s), .. } => *s,
                _ => return None,
            };
            let end = match &right.kind
            {
                ExprKind::Identifier { slot: Some(s), .. } => RangeEnd::Slot(*s),
                ExprKind::Integer { value, kind } =>
                {
                    let idx = push_const(consts, make_signed_int(*value, *kind));
                    RangeEnd::Const(idx)
                }
                ExprKind::Unsigned { value, kind } =>
                {
                    let idx = push_const(consts, make_unsigned_int(*value, *kind));
                    RangeEnd::Const(idx)
                }
                ExprKind::Float { value, kind } =>
                {
                    let idx = push_const(consts, make_float(*value, *kind));
                    RangeEnd::Const(idx)
                }
                _ => return None,
            };
            (idx_slot, end)
        }
        _ => return None,
    };

    let (stmts, line) = match &body.kind
    {
        ExprKind::Block(stmts) => (stmts, body.line),
        _ => return None,
    };
    if stmts.is_empty()
    {
        return None;
    }
    let (body_stmts, increment) = stmts.split_at(stmts.len() - 1);
    let inc_stmt = &increment[0];
    let step = match &inc_stmt.kind
    {
        ExprKind::Assignment {
            slot: Some(s),
            value,
            ..
        } if *s == index_slot => match &value.kind
        {
            ExprKind::BinaryOp {
                left,
                op: Op::Add,
                right,
            } =>
            {
                let left_is_index = matches!(left.kind, ExprKind::Identifier { slot: Some(ls), .. } if ls == index_slot);
                let right_is_index = matches!(right.kind, ExprKind::Identifier { slot: Some(rs), .. } if rs == index_slot);
                if left_is_index
                {
                    range_step_from_literal(right)?
                }
                else if right_is_index
                {
                    range_step_from_literal(left)?
                }
                else
                {
                    return None;
                }
            }
            _ => return None,
        },
        _ => return None,
    };

    let body_expr = Expr {
        kind: ExprKind::Block(body_stmts.to_vec()),
        line,
        column: body.column,
        source: body.source.clone(),
    };
    Some((index_slot, end, step, body_expr))
}

pub(super) fn match_dot_assign(stmt: &Expr, index_slot: usize) -> Option<(usize, usize, usize)>
{
    let (acc_slot, value) = match &stmt.kind
    {
        ExprKind::Assignment {
            slot: Some(s),
            value,
            ..
        } => (*s, value.as_ref()),
        _ => return None,
    };
    let (add_left, add_right) = match &value.kind
    {
        ExprKind::BinaryOp {
            left,
            op: Op::Add,
            right,
        } => (left.as_ref(), right.as_ref()),
        _ => return None,
    };
    let is_acc = |expr: &Expr| matches!(expr.kind, ExprKind::Identifier { slot: Some(s), .. } if s == acc_slot);
    let mul_expr = if is_acc(add_left)
    {
        add_right
    }
    else if is_acc(add_right)
    {
        add_left
    }
    else
    {
        return None;
    };
    let (a_slot, b_slot) = match &mul_expr.kind
    {
        ExprKind::BinaryOp {
            left,
            op: Op::Multiply,
            right,
        } =>
        {
            let (a_slot, a_idx) = match_f64_index(left)?;
            let (b_slot, b_idx) = match_f64_index(right)?;
            if a_idx != index_slot || b_idx != index_slot
            {
                return None;
            }
            (a_slot, b_slot)
        }
        _ => return None,
    };
    Some((acc_slot, a_slot, b_slot))
}

pub(super) fn match_dot_range_body(body: &Expr, index_slot: usize) -> Option<(usize, usize, usize)>
{
    let stmt = match &body.kind
    {
        ExprKind::Block(stmts) if stmts.len() == 1 => &stmts[0],
        _ => body,
    };
    match_dot_assign(stmt, index_slot)
}

pub(super) fn match_dot2_range_body(
    body: &Expr,
    index_slot: usize,
) -> Option<(usize, usize, usize, usize, usize, usize)>
{
    let stmts = match &body.kind
    {
        ExprKind::Block(stmts) if stmts.len() == 2 => stmts,
        _ => return None,
    };
    let (acc1, a1, b1) = match_dot_assign(&stmts[0], index_slot)?;
    let (acc2, a2, b2) = match_dot_assign(&stmts[1], index_slot)?;
    if acc1 == acc2
    {
        return None;
    }
    Some((acc1, a1, b1, acc2, a2, b2))
}

pub(super) fn match_range_end(expr: &Expr, consts: &mut Vec<Value>) -> Option<RangeEnd>
{
    match &expr.kind
    {
        ExprKind::Identifier { slot: Some(s), .. } => Some(RangeEnd::Slot(*s)),
        ExprKind::Integer { value, kind } =>
        {
            let idx = push_const(consts, make_signed_int(*value, *kind));
            Some(RangeEnd::Const(idx))
        }
        ExprKind::Unsigned { value, kind } =>
        {
            let idx = push_const(consts, make_unsigned_int(*value, *kind));
            Some(RangeEnd::Const(idx))
        }
        ExprKind::Float { value, kind } =>
        {
            let idx = push_const(consts, make_float(*value, *kind));
            Some(RangeEnd::Const(idx))
        }
        _ => None,
    }
}

pub(super) fn is_pure_expr(expr: &Expr) -> bool
{
    match &expr.kind
    {
        ExprKind::Integer { .. }
        | ExprKind::Unsigned { .. }
        | ExprKind::Float { .. }
        | ExprKind::Boolean(_)
        | ExprKind::Nil => true,
        ExprKind::Identifier { .. } => true,
        ExprKind::Not(expr) => is_pure_expr(expr),
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } => is_pure_expr(left) && is_pure_expr(right),
        ExprKind::BinaryOp { left, right, .. } => is_pure_expr(left) && is_pure_expr(right),
        _ => false,
    }
}

pub(super) fn match_f64_index(expr: &Expr) -> Option<(usize, usize)>
{
    match &expr.kind
    {
        ExprKind::Index { target, index } =>
        {
            let target_slot = match &target.kind
            {
                ExprKind::Identifier { slot: Some(s), .. } => *s,
                _ => return None,
            };
            let index_slot = match &index.kind
            {
                ExprKind::Identifier { slot: Some(s), .. } => *s,
                _ => return None,
            };
            Some((target_slot, index_slot))
        }
        _ => None,
    }
}

pub(super) fn match_f64_mul(expr: &Expr) -> Option<(Expr, usize, usize)>
{
    match &expr.kind
    {
        ExprKind::BinaryOp {
            left,
            op: Op::Multiply,
            right,
        } =>
        {
            if let Some((src_slot, src_index_slot)) = match_f64_index(left)
            {
                return Some(((*right.as_ref()).clone(), src_slot, src_index_slot));
            }
            if let Some((src_slot, src_index_slot)) = match_f64_index(right)
            {
                return Some(((*left.as_ref()).clone(), src_slot, src_index_slot));
            }
            None
        }
        _ => None,
    }
}

pub(super) fn match_f64_axpy(target: &Expr, value: &Expr) -> Option<(usize, usize, usize, usize, Expr)>
{
    let (dst_slot, dst_index_slot) = match_f64_index(target)?;
    let (add_left, add_right) = match &value.kind
    {
        ExprKind::BinaryOp {
            left,
            op: Op::Add,
            right,
        } => (left.as_ref(), right.as_ref()),
        _ => return None,
    };
    if let Some((slot, idx)) = match_f64_index(add_left)
    {
        if slot == dst_slot && idx == dst_index_slot
        {
            if let Some((scalar, src_slot, src_index_slot)) = match_f64_mul(add_right)
            {
                return Some((dst_slot, dst_index_slot, src_slot, src_index_slot, scalar));
            }
        }
    }
    if let Some((slot, idx)) = match_f64_index(add_right)
    {
        if slot == dst_slot && idx == dst_index_slot
        {
            if let Some((scalar, src_slot, src_index_slot)) = match_f64_mul(add_left)
            {
                return Some((dst_slot, dst_index_slot, src_slot, src_index_slot, scalar));
            }
        }
    }
    None
}

pub(super) fn array_expr_is_f64(expr: &Expr) -> Option<bool>
{
    match &expr.kind
    {
        ExprKind::Array(elements) =>
        {
            if elements.is_empty()
            {
                return Some(false);
            }
            let mut all_numeric = true;
            let mut all_f32 = true;
            let mut all_i32 = true;
            let mut any_float = false;
            let mut any_int = false;
            for e in elements
            {
                match &e.kind
                {
                    ExprKind::Float { kind, .. } =>
                    {
                        any_float = true;
                        if *kind != FloatKind::F32
                        {
                            all_f32 = false;
                        }
                    }
                    ExprKind::Integer { kind, .. } =>
                    {
                        any_int = true;
                        if *kind != IntKind::I32
                        {
                            all_i32 = false;
                        }
                    }
                    ExprKind::Unsigned { kind, .. } =>
                    {
                        any_int = true;
                        if *kind != IntKind::U32
                        {
                            all_i32 = false;
                        }
                    }
                    _ =>
                    {
                        all_numeric = false;
                        break;
                    }
                }
            }
            if !all_numeric
            {
                return None;
            }
            if any_float
            {
                if any_int
                {
                    return Some(true);
                }
                return if all_f32 { None } else { Some(true) };
            }
            if all_i32
            {
                return None;
            }
            None
        }
        ExprKind::ArrayGenerator { generator, .. } => match &generator.kind
        {
            ExprKind::Float {
                kind: FloatKind::F64,
                ..
            } => Some(true),
            ExprKind::Float { .. } => None,
            ExprKind::Integer { .. } | ExprKind::Unsigned { .. } => None,
            _ => None,
        },
        _ => None,
    }
}

thread_local! {
    static COMPILE_USE_CACHES: Cell<bool> = Cell::new(true);
}

pub(super) fn with_compile_use_caches<F, R>(use_caches: bool, f: F) -> R
where
    F: FnOnce() -> R,
{
    COMPILE_USE_CACHES.with(|flag| {
        let prev = flag.replace(use_caches);
        let result = f();
        flag.set(prev);
        result
    })
}

pub(super) fn compile_expr(
    expr: &Expr,
    code: &mut Vec<Instruction>,
    consts: &mut Vec<Value>,
    want_value: bool,
) -> bool
{
    let use_caches = COMPILE_USE_CACHES.with(|flag| flag.get());
    match &expr.kind
    {
        ExprKind::Integer { value, kind } =>
        {
            if want_value
            {
                let idx = push_const(consts, make_signed_int(*value, *kind));
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Unsigned { value, kind } =>
        {
            if want_value
            {
                let idx = push_const(consts, make_unsigned_int(*value, *kind));
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Float { value, kind } =>
        {
            if want_value
            {
                let idx = push_const(consts, make_float(*value, *kind));
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Identifier { slot: Some(s), .. } =>
        {
            if want_value
            {
                code.push(Instruction::LoadSlot(*s));
            }
        }
        ExprKind::Identifier { slot: None, name } =>
        {
            if want_value
            {
                if use_caches
                {
                    code.push(Instruction::LoadGlobalCached(
                        *name,
                        Rc::new(RefCell::new(GlobalCache::default())),
                    ));
                }
                else
                {
                    code.push(Instruction::LoadGlobal(*name));
                }
            }
        }
        ExprKind::Boolean(b) =>
        {
            if want_value
            {
                let idx = push_const(consts, Value::Boolean(*b));
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Nil =>
        {
            if want_value
            {
                let idx = push_const(consts, Value::Nil);
                code.push(Instruction::LoadConstIdx(idx));
            }
        }
        ExprKind::Assignment {
            value,
            slot: Some(s),
            ..
        } =>
        {
            if !compile_expr(value, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::StoreSlot(*s));
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Clone(expr) =>
        {
            if !compile_expr(expr, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::CloneValue);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::EnvFreeze(_) =>
        {
            return false;
        }
        ExprKind::Result { .. } | ExprKind::ErrorRaise(_) =>
        {
            return false;
        }
        ExprKind::Not(expr) =>
        {
            if !compile_expr(expr, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::Not);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::And { left, right } =>
        {
            if !compile_expr(left, code, consts, true)
            {
                return false;
            }
            let jump_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            if !compile_expr(right, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::Not);
            code.push(Instruction::Not);
            let jump_end_idx = code.len();
            code.push(Instruction::Jump(usize::MAX));
            let false_target = code.len();
            let false_idx = push_const(consts, Value::Boolean(false));
            code.push(Instruction::LoadConstIdx(false_idx));
            let end_target = code.len();
            code[jump_false_idx] = Instruction::JumpIfFalse(false_target);
            code[jump_end_idx] = Instruction::Jump(end_target);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::AndBool { left, right } =>
        {
            if !compile_expr(left, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::CheckBool);
            let jump_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            if !compile_expr(right, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::CheckBool);
            let jump_end_idx = code.len();
            code.push(Instruction::Jump(usize::MAX));
            let false_target = code.len();
            let false_idx = push_const(consts, Value::Boolean(false));
            code.push(Instruction::LoadConstIdx(false_idx));
            let end_target = code.len();
            code[jump_false_idx] = Instruction::JumpIfFalse(false_target);
            code[jump_end_idx] = Instruction::Jump(end_target);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Or { left, right } =>
        {
            if !compile_expr(left, code, consts, true)
            {
                return false;
            }
            let jump_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            let true_idx = push_const(consts, Value::Boolean(true));
            code.push(Instruction::LoadConstIdx(true_idx));
            let jump_end_idx = code.len();
            code.push(Instruction::Jump(usize::MAX));
            let false_target = code.len();
            if !compile_expr(right, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::Not);
            code.push(Instruction::Not);
            let end_target = code.len();
            code[jump_false_idx] = Instruction::JumpIfFalse(false_target);
            code[jump_end_idx] = Instruction::Jump(end_target);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::OrBool { left, right } =>
        {
            if !compile_expr(left, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::CheckBool);
            let jump_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            let true_idx = push_const(consts, Value::Boolean(true));
            code.push(Instruction::LoadConstIdx(true_idx));
            let jump_end_idx = code.len();
            code.push(Instruction::Jump(usize::MAX));
            let false_target = code.len();
            if !compile_expr(right, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::CheckBool);
            let end_target = code.len();
            code[jump_false_idx] = Instruction::JumpIfFalse(false_target);
            code[jump_end_idx] = Instruction::Jump(end_target);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Assignment {
            name,
            value,
            slot: None,
        } =>
        {
            if !compile_expr(value, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::StoreGlobal(*name));
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            if block.is_some()
            {
                if let ExprKind::Index { target, index } = &function.kind
                {
                    if let ExprKind::String(name) = &index.kind
                    {
                        if !compile_expr(target, code, consts, true)
                        {
                            return false;
                        }
                        if use_caches
                        {
                            for arg in args
                            {
                                if !compile_expr(arg, code, consts, true)
                                {
                                    return false;
                                }
                            }
                            let map_cache = Rc::new(RefCell::new(MapAccessCache::default()));
                            let call_cache = Rc::new(RefCell::new(CallSiteCache::default()));
                            let block_ref = Rc::new(block.as_ref().unwrap().clone());
                            match args.len()
                            {
                                0 => code.push(Instruction::CallMethodWithBlockCached0(
                                    name.clone(),
                                    map_cache,
                                    call_cache,
                                    block_ref,
                                )),
                                1 => code.push(Instruction::CallMethodWithBlockCached1(
                                    name.clone(),
                                    map_cache,
                                    call_cache,
                                    block_ref,
                                )),
                                _ => code.push(Instruction::CallMethodWithBlockCached(
                                    name.clone(),
                                    map_cache,
                                    call_cache,
                                    block_ref,
                                    args.len(),
                                )),
                            }
                        }
                        else
                        {
                            if !compile_expr(index, code, consts, true)
                            {
                                return false;
                            }
                            code.push(Instruction::Index);
                            for arg in args
                            {
                                if !compile_expr(arg, code, consts, true)
                                {
                                    return false;
                                }
                            }
                            code.push(Instruction::CallValueWithBlock(
                                Rc::new(block.as_ref().unwrap().clone()),
                                args.len(),
                            ));
                        }
                        if !want_value
                        {
                            code.push(Instruction::Pop);
                        }
                        return true;
                    }
                }
                if !compile_expr(function, code, consts, true)
                {
                    return false;
                }
                for arg in args
                {
                    if !compile_expr(arg, code, consts, true)
                    {
                        return false;
                    }
                }
                if use_caches
                {
                    let call_cache = Rc::new(RefCell::new(crate::value::CallSiteCache::default()));
                    let block_ref = Rc::new(block.as_ref().unwrap().clone());
                    match args.len()
                    {
                        0 =>
                        {
                            code.push(Instruction::CallValueWithBlockCached0(call_cache, block_ref))
                        }
                        1 =>
                        {
                            code.push(Instruction::CallValueWithBlockCached1(call_cache, block_ref))
                        }
                        _ => code.push(Instruction::CallValueWithBlockCached(
                            call_cache,
                            block_ref,
                            args.len(),
                        )),
                    }
                }
                else
                {
                    code.push(Instruction::CallValueWithBlock(
                        Rc::new(block.as_ref().unwrap().clone()),
                        args.len(),
                    ));
                }
                if !want_value
                {
                    code.push(Instruction::Pop);
                }
                return true;
            }
            if let ExprKind::Index { target, index } = &function.kind
            {
                if let ExprKind::String(name) = &index.kind
                {
                    if let ExprKind::Identifier {
                        name: target_name, ..
                    } = &target.kind
                    {
                        if symbol_name(*target_name).as_str() == "Bytes"
                        {
                            match name.as_str()
                            {
                                "len" if args.len() == 1 =>
                                {
                                    if !compile_expr(&args[0], code, consts, true)
                                    {
                                        return false;
                                    }
                                    code.push(Instruction::Len);
                                    if !want_value
                                    {
                                        code.push(Instruction::Pop);
                                    }
                                    return true;
                                }
                                "get" if args.len() == 2 =>
                                {
                                    if !compile_expr(&args[0], code, consts, true)
                                    {
                                        return false;
                                    }
                                    if !compile_expr(&args[1], code, consts, true)
                                    {
                                        return false;
                                    }
                                    code.push(Instruction::Index);
                                    if !want_value
                                    {
                                        code.push(Instruction::Pop);
                                    }
                                    return true;
                                }
                                _ =>
                                {}
                            }
                        }
                    }
                }
            }
            if let ExprKind::Identifier { name, .. } = &function.kind
            {
                if let Some(builtin) = builtin_from_symbol(*name)
                {
                    for arg in args
                    {
                        if !compile_expr(arg, code, consts, true)
                        {
                            return false;
                        }
                    }
                    match builtin
                    {
                        Builtin::Len if args.len() == 1 => code.push(Instruction::Len),
                        _ => code.push(Instruction::CallBuiltin(builtin, args.len())),
                    }
                    if !want_value
                    {
                        code.push(Instruction::Pop);
                    }
                    return true;
                }
                if let ExprKind::Identifier { slot: None, name } = &function.kind
                {
                    if use_caches
                    {
                        for arg in args
                        {
                            if !compile_expr(arg, code, consts, true)
                            {
                                return false;
                            }
                        }
                        let global_cache = Rc::new(RefCell::new(GlobalCache::default()));
                        let call_cache = Rc::new(RefCell::new(CallSiteCache::default()));
                        match args.len()
                        {
                            0 => code.push(Instruction::CallGlobalCached0(
                                *name,
                                global_cache,
                                call_cache,
                            )),
                            1 => code.push(Instruction::CallGlobalCached1(
                                *name,
                                global_cache,
                                call_cache,
                            )),
                            _ => code.push(Instruction::CallGlobalCached(
                                *name,
                                global_cache,
                                call_cache,
                                args.len(),
                            )),
                        }
                    }
                    else
                    {
                        code.push(Instruction::LoadGlobal(*name));
                        for arg in args
                        {
                            if !compile_expr(arg, code, consts, true)
                            {
                                return false;
                            }
                        }
                        code.push(Instruction::CallValue(args.len()));
                    }
                    if !want_value
                    {
                        code.push(Instruction::Pop);
                    }
                    return true;
                }
            }
            if let ExprKind::Index { target, index } = &function.kind
            {
                if let ExprKind::String(name) = &index.kind
                {
                    if !compile_expr(target, code, consts, true)
                    {
                        return false;
                    }
                    if use_caches
                    {
                        for arg in args
                        {
                            if !compile_expr(arg, code, consts, true)
                            {
                                return false;
                            }
                        }
                        let map_cache = Rc::new(RefCell::new(MapAccessCache::default()));
                        let call_cache = Rc::new(RefCell::new(CallSiteCache::default()));
                        match args.len()
                        {
                            0 => code.push(Instruction::CallMethodCached0(
                                name.clone(),
                                map_cache,
                                call_cache,
                            )),
                            1 => code.push(Instruction::CallMethodCached1(
                                name.clone(),
                                map_cache,
                                call_cache,
                            )),
                            _ => code.push(Instruction::CallMethodCached(
                                name.clone(),
                                map_cache,
                                call_cache,
                                args.len(),
                            )),
                        }
                    }
                    else
                    {
                        if !compile_expr(index, code, consts, true)
                        {
                            return false;
                        }
                        code.push(Instruction::Index);
                        for arg in args
                        {
                            if !compile_expr(arg, code, consts, true)
                            {
                                return false;
                            }
                        }
                        code.push(Instruction::CallValue(args.len()));
                    }
                    if !want_value
                    {
                        code.push(Instruction::Pop);
                    }
                    return true;
                }
            }
            if !compile_expr(function, code, consts, true)
            {
                return false;
            }
            for arg in args
            {
                if !compile_expr(arg, code, consts, true)
                {
                    return false;
                }
            }
            if let Some(block) = block
            {
                code.push(Instruction::CallValueWithBlock(Rc::new(block.clone()), args.len()));
            }
            else
            {
                if use_caches
                {
                    let call_cache = Rc::new(RefCell::new(crate::value::CallSiteCache::default()));
                    match args.len()
                    {
                        0 => code.push(Instruction::CallValueCached0(call_cache)),
                        1 => code.push(Instruction::CallValueCached1(call_cache)),
                        _ => code.push(Instruction::CallValueCached(call_cache, args.len())),
                    }
                }
                else
                {
                    code.push(Instruction::CallValue(args.len()));
                }
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Index { target, index } =>
        {
            if !compile_expr(target, code, consts, true)
            {
                return false;
            }
            if let ExprKind::String(name) = &index.kind
            {
                match name.as_str()
                {
                    "keys" =>
                    {
                        code.push(Instruction::MapKeys);
                        if !want_value
                        {
                            code.push(Instruction::Pop);
                        }
                        return true;
                    }
                    "values" =>
                    {
                        code.push(Instruction::MapValues);
                        if !want_value
                        {
                            code.push(Instruction::Pop);
                        }
                        return true;
                    }
                    _ =>
                    {}
                }
                if !compile_expr(index, code, consts, true)
                {
                    return false;
                }
                if use_caches
                {
                    code.push(Instruction::MapIndexCached(Rc::new(RefCell::new(
                        MapAccessCache::default(),
                    ))));
                }
                else
                {
                    code.push(Instruction::Index);
                }
                if !want_value
                {
                    code.push(Instruction::Pop);
                }
                return true;
            }
            if !compile_expr(index, code, consts, true)
            {
                return false;
            }
            if use_caches
            {
                let use_f64_cache = match array_expr_is_f64(target)
                {
                    Some(true) | None => true,
                    Some(false) => false,
                };
                if use_f64_cache
                {
                    code.push(Instruction::F64IndexCached(Rc::new(RefCell::new(
                        crate::value::IndexCache::default(),
                    ))));
                }
                else
                {
                    code.push(Instruction::IndexCached(Rc::new(RefCell::new(
                        crate::value::IndexCache::default(),
                    ))));
                }
            }
            else
            {
                code.push(Instruction::Index);
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Slice { target, start, end } =>
        {
            if !compile_expr(target, code, consts, true)
            {
                return false;
            }
            if !compile_expr(start, code, consts, true)
            {
                return false;
            }
            if !compile_expr(end, code, consts, true)
            {
                return false;
            }
            code.push(Instruction::Slice);
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            if let Some((dst_slot, dst_index_slot, src_slot, src_index_slot, scalar)) =
                match_f64_axpy(target, value)
            {
                if !compile_expr(&scalar, code, consts, true)
                {
                    return false;
                }
                code.push(Instruction::F64Axpy {
                    dst_slot,
                    dst_index_slot,
                    src_slot,
                    src_index_slot,
                });
            }
            else
            {
                if !compile_expr(target, code, consts, true)
                {
                    return false;
                }
                if !compile_expr(index, code, consts, true)
                {
                    return false;
                }
                if !compile_expr(value, code, consts, true)
                {
                    return false;
                }
                if use_caches
                {
                    code.push(Instruction::F64IndexAssignCached(Rc::new(RefCell::new(
                        crate::value::IndexCache::default(),
                    ))));
                }
                else
                {
                    code.push(Instruction::IndexAssign);
                }
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. }
        | ExprKind::FilePublic(_)
        | ExprKind::FunctionPublic(_) =>
        {
            return false;
        }
        ExprKind::FormatString(_) =>
        {
            return false;
        }
        ExprKind::BinaryOp { left, op, right } =>
        {
            let mut handled = false;
            if *op == Op::Multiply
            {
                let one = |expr: &Expr| {
                    matches!(
                        expr.kind,
                        ExprKind::Integer { value: 1, .. }
                            | ExprKind::Unsigned { value: 1, .. }
                            | ExprKind::Float { value: 1.0, .. }
                    )
                };
                if is_pure_expr(left)
                {
                    match &right.kind
                    {
                        ExprKind::BinaryOp {
                            left: r_left,
                            op: Op::Add,
                            right: r_right,
                        } =>
                        {
                            if (r_left.as_ref() == left.as_ref() && one(r_right))
                                || (r_right.as_ref() == left.as_ref() && one(r_left))
                            {
                                if !compile_expr(left, code, consts, true)
                                {
                                    return false;
                                }
                                code.push(Instruction::Dup);
                                let idx = push_const(consts, default_int(1));
                                code.push(Instruction::LoadConstIdx(idx));
                                if use_caches
                                {
                                    code.push(Instruction::AddCached(Rc::new(RefCell::new(
                                        BinaryOpCache::default(),
                                    ))));
                                    code.push(Instruction::MulCached(Rc::new(RefCell::new(
                                        BinaryOpCache::default(),
                                    ))));
                                }
                                else
                                {
                                    code.push(Instruction::Add);
                                    code.push(Instruction::Mul);
                                }
                                handled = true;
                            }
                        }
                        _ =>
                        {}
                    }
                }
                if !handled && is_pure_expr(right)
                {
                    match &left.kind
                    {
                        ExprKind::BinaryOp {
                            left: l_left,
                            op: Op::Add,
                            right: l_right,
                        } =>
                        {
                            if (l_left.as_ref() == right.as_ref() && one(l_right))
                                || (l_right.as_ref() == right.as_ref() && one(l_left))
                            {
                                if !compile_expr(right, code, consts, true)
                                {
                                    return false;
                                }
                                code.push(Instruction::Dup);
                                let idx = push_const(consts, default_int(1));
                                code.push(Instruction::LoadConstIdx(idx));
                                if use_caches
                                {
                                    code.push(Instruction::AddCached(Rc::new(RefCell::new(
                                        BinaryOpCache::default(),
                                    ))));
                                    code.push(Instruction::MulCached(Rc::new(RefCell::new(
                                        BinaryOpCache::default(),
                                    ))));
                                }
                                else
                                {
                                    code.push(Instruction::Add);
                                    code.push(Instruction::Mul);
                                }
                                handled = true;
                            }
                        }
                        _ =>
                        {}
                    }
                }
            }
            if !handled
            {
                if !compile_expr(left, code, consts, true)
                {
                    return false;
                }
                if !compile_expr(right, code, consts, true)
                {
                    return false;
                }
                match op
                {
                    Op::Add =>
                    {
                        if use_caches
                        {
                            code.push(Instruction::AddCached(Rc::new(RefCell::new(
                                BinaryOpCache::default(),
                            ))));
                        }
                        else
                        {
                            code.push(Instruction::Add);
                        }
                    }
                    Op::Subtract =>
                    {
                        if use_caches
                        {
                            code.push(Instruction::SubCached(Rc::new(RefCell::new(
                                BinaryOpCache::default(),
                            ))));
                        }
                        else
                        {
                            code.push(Instruction::Sub);
                        }
                    }
                    Op::Multiply =>
                    {
                        if use_caches
                        {
                            code.push(Instruction::MulCached(Rc::new(RefCell::new(
                                BinaryOpCache::default(),
                            ))));
                        }
                        else
                        {
                            code.push(Instruction::Mul);
                        }
                    }
                    Op::Divide =>
                    {
                        if use_caches
                        {
                            code.push(Instruction::DivCached(Rc::new(RefCell::new(
                                BinaryOpCache::default(),
                            ))));
                        }
                        else
                        {
                            code.push(Instruction::Div);
                        }
                    }
                    Op::Power =>
                    {
                        if use_caches
                        {
                            code.push(Instruction::PowCached(Rc::new(RefCell::new(
                                BinaryOpCache::default(),
                            ))));
                        }
                        else
                        {
                            code.push(Instruction::Pow);
                        }
                    }
                    Op::Equal => code.push(Instruction::Eq),
                    Op::GreaterThan => code.push(Instruction::Gt),
                    Op::LessThan => code.push(Instruction::Lt),
                    _ => return false,
                }
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Block(stmts) =>
        {
            if stmts.is_empty()
            {
                if want_value
                {
                    let idx = push_const(consts, Value::Nil);
                    code.push(Instruction::LoadConstIdx(idx));
                }
            }
            else
            {
                let last_idx = stmts.len() - 1;
                for (idx, stmt) in stmts.iter().enumerate()
                {
                    let is_last = idx == last_idx;
                    if !compile_expr(stmt, code, consts, is_last && want_value)
                    {
                        return false;
                    }
                }
            }
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            if !compile_expr(condition, code, consts, true)
            {
                return false;
            }
            let jump_if_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            if !compile_expr(then_branch, code, consts, want_value)
            {
                return false;
            }
            let jump_end_idx = code.len();
            code.push(Instruction::Jump(usize::MAX));
            let else_target = code.len();
            if let Some(else_expr) = else_branch
            {
                if !compile_expr(else_expr, code, consts, want_value)
                {
                    return false;
                }
            }
            else
            {
                if want_value
                {
                    let idx = push_const(consts, Value::Nil);
                    code.push(Instruction::LoadConstIdx(idx));
                }
            }
            let end_target = code.len();
            code[jump_if_false_idx] = Instruction::JumpIfFalse(else_target);
            code[jump_end_idx] = Instruction::Jump(end_target);
        }
        ExprKind::While { condition, body } =>
        {
            if !want_value
            {
                if let Some((index_slot, end, step, body_expr)) =
                    match_for_range(condition, body, consts)
                {
                    let is_unit_step = matches!(step, RangeStep::Int(1));
                    if is_unit_step
                    {
                        if let Some((acc1, a1, b1, acc2, a2, b2)) =
                            match_dot2_range_body(&body_expr, index_slot)
                        {
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
                        }
                        else if let Some((acc_slot, a_slot, b_slot)) =
                            match_dot_range_body(&body_expr, index_slot)
                        {
                            code.push(Instruction::F64DotRange {
                                acc_slot,
                                a_slot,
                                b_slot,
                                index_slot,
                                end,
                            });
                            code.push(Instruction::Pop);
                            return true;
                        }
                    }
                    let mut body_code = Vec::new();
                    if !compile_expr(&body_expr, &mut body_code, consts, true)
                    {
                        return false;
                    }
                    match step
                    {
                        RangeStep::Int(step) =>
                        {
                            code.push(Instruction::ForRangeInt {
                                index_slot,
                                end,
                                step,
                                body: Rc::new(body_code),
                            });
                        }
                        RangeStep::Float(step, kind) =>
                        {
                            code.push(Instruction::ForRangeFloat {
                                index_slot,
                                end,
                                step,
                                kind,
                                body: Rc::new(body_code),
                            });
                        }
                    }
                    code.push(Instruction::Pop);
                    return true;
                }
            }
            if want_value
            {
                let nil_idx = push_const(consts, Value::Nil);
                code.push(Instruction::LoadConstIdx(nil_idx));
            }
            let loop_start = code.len();
            if !compile_expr(condition, code, consts, true)
            {
                return false;
            }
            let jump_if_false_idx = code.len();
            code.push(Instruction::JumpIfFalse(usize::MAX));
            if want_value
            {
                code.push(Instruction::Pop);
                if !compile_expr(body, code, consts, true)
                {
                    return false;
                }
            }
            else
            {
                if !compile_expr(body, code, consts, false)
                {
                    return false;
                }
            }
            code.push(Instruction::Jump(loop_start));
            let loop_end = code.len();
            code[jump_if_false_idx] = Instruction::JumpIfFalse(loop_end);
        }
        ExprKind::For {
            var_slot: Some(var_slot),
            iterable,
            body,
            ..
        } =>
        {
            if !compile_expr(iterable, code, consts, true)
            {
                return false;
            }
            let mut body_code = Vec::new();
            if !compile_expr(body, &mut body_code, consts, true)
            {
                return false;
            }
            if let Some(is_f64) = array_expr_is_f64(iterable)
            {
                if is_f64
                {
                    code.push(Instruction::ForEachF64Array {
                        var_slot: *var_slot,
                        body: Rc::new(body_code),
                    });
                }
                else
                {
                    code.push(Instruction::ForEachArray {
                        var_slot: *var_slot,
                        body: Rc::new(body_code),
                    });
                }
            }
            else
            {
                code.push(Instruction::ForEach {
                    var_slot: *var_slot,
                    body: Rc::new(body_code),
                });
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Loop {
            count,
            var_slot: Some(var_slot),
            body,
            ..
        } =>
        {
            let end = match_range_end(count, consts);
            if let Some(end) = end
            {
                let zero_idx = push_const(consts, default_int(0));
                code.push(Instruction::LoadConstIdx(zero_idx));
                code.push(Instruction::StoreSlot(*var_slot));
                let mut body_code = Vec::new();
                if !compile_expr(body, &mut body_code, consts, true)
                {
                    return false;
                }
                code.push(Instruction::ForRange {
                    index_slot: *var_slot,
                    end,
                    body: Rc::new(body_code),
                });
                code.push(Instruction::Pop);
                if want_value
                {
                    let idx = push_const(consts, Value::Nil);
                    code.push(Instruction::LoadConstIdx(idx));
                }
            }
            else
            {
                return false;
            }
        }
        ExprKind::Array(elements) =>
        {
            let mut all_f64 = true;
            for e in elements
            {
                match &e.kind
                {
                    ExprKind::Float { value, kind } =>
                    {
                        let idx = push_const(consts, make_float(*value, *kind));
                        code.push(Instruction::LoadConstIdx(idx));
                    }
                    ExprKind::Integer { value, kind } =>
                    {
                        let idx = push_const(consts, make_signed_int(*value, *kind));
                        code.push(Instruction::LoadConstIdx(idx));
                    }
                    ExprKind::Unsigned { value, kind } =>
                    {
                        let idx = push_const(consts, make_unsigned_int(*value, *kind));
                        code.push(Instruction::LoadConstIdx(idx));
                    }
                    _ =>
                    {
                        all_f64 = false;
                        if !compile_expr(e, code, consts, true)
                        {
                            return false;
                        }
                    }
                }
                if all_f64
                {
                    match &e.kind
                    {
                        ExprKind::Float { .. }
                        | ExprKind::Integer { .. }
                        | ExprKind::Unsigned { .. } =>
                        {}
                        _ => all_f64 = false,
                    }
                }
            }
            if all_f64
            {
                code.push(Instruction::F64ArrayGen {
                    count: Some(elements.len()),
                });
            }
            else
            {
                code.push(Instruction::MakeArray(elements.len()));
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::StructDef { .. }
        | ExprKind::StructLiteral { .. }
        | ExprKind::MethodDef { .. } =>
        {
            return false;
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            let mut use_f64_gen = false;
            match &generator.kind
            {
                ExprKind::Float { value, kind } =>
                {
                    let idx = push_const(consts, make_float(*value, *kind));
                    code.push(Instruction::LoadConstIdx(idx));
                    use_f64_gen = true;
                }
                ExprKind::Integer { value, kind } =>
                {
                    let idx = push_const(consts, make_signed_int(*value, *kind));
                    code.push(Instruction::LoadConstIdx(idx));
                    use_f64_gen = true;
                }
                ExprKind::Unsigned { value, kind } =>
                {
                    let idx = push_const(consts, make_unsigned_int(*value, *kind));
                    code.push(Instruction::LoadConstIdx(idx));
                    use_f64_gen = true;
                }
                _ =>
                {
                    if !compile_expr(generator, code, consts, true)
                    {
                        return false;
                    }
                }
            }
            if !compile_expr(size, code, consts, true)
            {
                return false;
            }
            if use_f64_gen
            {
                code.push(Instruction::F64ArrayGen { count: None });
            }
            else
            {
                code.push(Instruction::ArrayGen);
            }
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                if !compile_expr(k, code, consts, true)
                {
                    return false;
                }
                if !compile_expr(v, code, consts, true)
                {
                    return false;
                }
            }
            code.push(Instruction::MakeMap(entries.len()));
            if !want_value
            {
                code.push(Instruction::Pop);
            }
        }
        _ => return false,
    }
    true
}

pub(super) fn find_compile_failure(expr: &Expr) -> Option<String>
{
    match &expr.kind
    {
        ExprKind::Identifier { slot: None, .. } =>
        {}
        ExprKind::FunctionDef { .. } =>
        {
            return Some("function definition not supported in bytecode dump; functions are dumped separately".to_string());
        }
        ExprKind::AnonymousFunction { .. } =>
        {
            return Some("anonymous function not supported in bytecode".to_string());
        }
        ExprKind::Use(_) =>
        {
            return Some("use not supported in bytecode".to_string());
        }
        ExprKind::Load(_) =>
        {
            return Some("load not supported in bytecode".to_string());
        }
        ExprKind::Import { .. } =>
        {
            return Some("import not supported in bytecode".to_string());
        }
        ExprKind::Export { .. } =>
        {
            return Some("export not supported in bytecode".to_string());
        }
        ExprKind::FormatString(_) =>
        {
            return Some("format string not supported in bytecode".to_string());
        }
        ExprKind::Shell(_) =>
        {
            return Some("shell command not supported in bytecode".to_string());
        }
        ExprKind::Result { .. } =>
        {
            return Some("result not supported in bytecode".to_string());
        }
        ExprKind::ErrorRaise(_) =>
        {
            return Some("error not supported in bytecode".to_string());
        }
        ExprKind::Reference(_) =>
        {
            return Some("reference expression not supported in bytecode".to_string());
        }
        ExprKind::For { var_slot: None, .. } =>
        {
            return Some("for-loop variable not resolved to slot".to_string());
        }
        ExprKind::Loop { var_slot: None, .. } =>
        {
            return Some("loop variable not resolved to slot".to_string());
        }
        ExprKind::Collect { var_slot: None, .. } =>
        {
            return Some("collect variable not resolved to slot".to_string());
        }
        ExprKind::Loop {
            count,
            var_slot: Some(_),
            ..
        } => match &count.kind
        {
            ExprKind::Identifier { slot: Some(_), .. }
            | ExprKind::Integer { .. }
            | ExprKind::Unsigned { .. }
            | ExprKind::Float { .. } =>
            {}
            _ =>
            {
                return Some(
                    "loop count not supported for bytecode (needs literal or slot)".to_string(),
                );
            }
        },
        ExprKind::Collect { .. } =>
        {
            return Some("collect not supported in bytecode".to_string());
        }
        ExprKind::StructDef { .. } =>
        {
            return Some("struct definition not supported in bytecode".to_string());
        }
        ExprKind::StructLiteral { .. } =>
        {
            return Some("struct literal not supported in bytecode".to_string());
        }
        ExprKind::MethodDef { .. } =>
        {
            return Some("method definition not supported in bytecode".to_string());
        }
        _ =>
        {}
    }

    match &expr.kind
    {
        ExprKind::BinaryOp { left, right, .. } =>
        {
            find_compile_failure(left).or_else(|| find_compile_failure(right))
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => find_compile_failure(condition)
            .or_else(|| find_compile_failure(then_branch))
            .or_else(|| else_branch.as_ref().and_then(|e| find_compile_failure(e))),
        ExprKind::Result {
            body,
            else_expr,
            ..
        } => find_compile_failure(body).or_else(|| find_compile_failure(else_expr)),
        ExprKind::While { condition, body } =>
        {
            find_compile_failure(condition).or_else(|| find_compile_failure(body))
        }
        ExprKind::For { iterable, body, .. } =>
        {
            find_compile_failure(iterable).or_else(|| find_compile_failure(body))
        }
        ExprKind::Loop { count, body, .. } =>
        {
            find_compile_failure(count).or_else(|| find_compile_failure(body))
        }
        ExprKind::Collect { count, into, body, .. } =>
        {
            find_compile_failure(count)
                .or_else(|| into.as_ref().and_then(|e| find_compile_failure(e)))
                .or_else(|| find_compile_failure(body))
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } => find_compile_failure(function)
            .or_else(|| args.iter().find_map(find_compile_failure))
            .or_else(|| block.as_ref().and_then(|c| find_compile_failure(&c.body))),
        ExprKind::Array(elements) => elements.iter().find_map(find_compile_failure),
        ExprKind::ArrayGenerator { generator, size } =>
        {
            find_compile_failure(generator).or_else(|| find_compile_failure(size))
        }
        ExprKind::Map(entries) => entries
            .iter()
            .find_map(|(k, v)| find_compile_failure(k).or_else(|| find_compile_failure(v))),
        ExprKind::Index { target, index } =>
        {
            find_compile_failure(target).or_else(|| find_compile_failure(index))
        }
        ExprKind::Slice { target, start, end } => find_compile_failure(target)
            .or_else(|| find_compile_failure(start))
            .or_else(|| find_compile_failure(end)),
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } => find_compile_failure(target)
            .or_else(|| find_compile_failure(index))
            .or_else(|| find_compile_failure(value)),
        ExprKind::Clone(expr)
        | ExprKind::EnvFreeze(expr)
        | ExprKind::ErrorRaise(expr) => find_compile_failure(expr),
        ExprKind::Not(expr) => find_compile_failure(expr),
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } =>
        {
            find_compile_failure(left).or_else(|| find_compile_failure(right))
        }
        ExprKind::Block(stmts) => stmts.iter().find_map(find_compile_failure),
        ExprKind::Assignment { value, .. } => find_compile_failure(value),
        ExprKind::Yield(args) => args.iter().find_map(find_compile_failure),
        ExprKind::Return(expr) => expr.as_ref().and_then(|e| find_compile_failure(e)),
        ExprKind::FormatString(parts) => parts.iter().find_map(|part| {
            if let crate::ast::FormatPart::Expr { expr, .. } = part
            {
                find_compile_failure(expr)
            }
            else
            {
                None
            }
        }),
        ExprKind::FilePublic(expr) => find_compile_failure(expr),
        ExprKind::FunctionPublic(expr) => find_compile_failure(expr),
        _ => None,
    }
}

pub(super) fn collect_function_exprs(
    expr: &Expr,
    out: &mut Vec<(Option<SymbolId>, Vec<Param>, Expr, Option<Rc<Vec<Rc<String>>>>, usize)>,
)
{
    match &expr.kind
    {
        ExprKind::FunctionDef {
            name,
            params,
            body,
            slots,
        } =>
        {
            out.push((Some(*name), params.clone(), *body.clone(), slots.clone(), expr.line));
            collect_function_exprs(body, out);
        }
        ExprKind::AnonymousFunction {
            params,
            body,
            slots,
        } =>
        {
            out.push((None, params.clone(), *body.clone(), slots.clone(), expr.line));
            collect_function_exprs(body, out);
        }
        ExprKind::MethodDef {
            params,
            body,
            slots,
            ..
        } =>
        {
            out.push((None, params.clone(), *body.clone(), slots.clone(), expr.line));
            collect_function_exprs(body, out);
        }
        ExprKind::Block(stmts) =>
        {
            for stmt in stmts
            {
                collect_function_exprs(stmt, out);
            }
        }
        ExprKind::FilePublic(expr) =>
        {
            collect_function_exprs(expr, out);
        }
        ExprKind::FunctionPublic(expr) =>
        {
            collect_function_exprs(expr, out);
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            collect_function_exprs(condition, out);
            collect_function_exprs(then_branch, out);
            if let Some(else_expr) = else_branch
            {
                collect_function_exprs(else_expr, out);
            }
        }
        ExprKind::Result {
            body,
            else_expr,
            ..
        } =>
        {
            collect_function_exprs(body, out);
            collect_function_exprs(else_expr, out);
        }
        ExprKind::While { condition, body } =>
        {
            collect_function_exprs(condition, out);
            collect_function_exprs(body, out);
        }
        ExprKind::For { iterable, body, .. } =>
        {
            collect_function_exprs(iterable, out);
            collect_function_exprs(body, out);
        }
        ExprKind::Loop { count, body, .. } =>
        {
            collect_function_exprs(count, out);
            collect_function_exprs(body, out);
        }
        ExprKind::Collect { count, into, body, .. } =>
        {
            collect_function_exprs(count, out);
            if let Some(into) = into
            {
                collect_function_exprs(into, out);
            }
            collect_function_exprs(body, out);
        }
        ExprKind::ErrorRaise(expr) =>
        {
            collect_function_exprs(expr, out);
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            collect_function_exprs(function, out);
            for arg in args
            {
                collect_function_exprs(arg, out);
            }
            if let Some(c) = block
            {
                collect_function_exprs(&c.body, out);
            }
        }
        ExprKind::Array(elements) =>
        {
            for e in elements
            {
                collect_function_exprs(e, out);
            }
        }
        ExprKind::StructLiteral { fields, .. } =>
        {
            for (_, expr) in fields
            {
                collect_function_exprs(expr, out);
            }
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            collect_function_exprs(generator, out);
            collect_function_exprs(size, out);
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                collect_function_exprs(k, out);
                collect_function_exprs(v, out);
            }
        }
        ExprKind::Index { target, index } =>
        {
            collect_function_exprs(target, out);
            collect_function_exprs(index, out);
        }
        ExprKind::Slice { target, start, end } =>
        {
            collect_function_exprs(target, out);
            collect_function_exprs(start, out);
            collect_function_exprs(end, out);
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            collect_function_exprs(target, out);
            collect_function_exprs(index, out);
            collect_function_exprs(value, out);
        }
        ExprKind::Clone(expr) =>
        {
            collect_function_exprs(expr, out);
        }
        ExprKind::EnvFreeze(expr) =>
        {
            collect_function_exprs(expr, out);
        }
        ExprKind::Not(expr) =>
        {
            collect_function_exprs(expr, out);
        }
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } =>
        {
            collect_function_exprs(left, out);
            collect_function_exprs(right, out);
        }
        ExprKind::FormatString(parts) =>
        {
            for part in parts
            {
                if let crate::ast::FormatPart::Expr { expr, .. } = part
                {
                    collect_function_exprs(expr, out);
                }
            }
        }
        ExprKind::Yield(args) =>
        {
            for arg in args
            {
                collect_function_exprs(arg, out);
            }
        }
        ExprKind::Return(expr) =>
        {
            if let Some(e) = expr
            {
                collect_function_exprs(e, out);
            }
        }
        ExprKind::Assignment { value, .. } =>
        {
            collect_function_exprs(value, out);
        }
        ExprKind::BinaryOp { left, right, .. } =>
        {
            collect_function_exprs(left, out);
            collect_function_exprs(right, out);
        }
        ExprKind::Reference(_)
        | ExprKind::Identifier { .. }
        | ExprKind::Integer { .. }
        | ExprKind::Unsigned { .. }
        | ExprKind::Float { .. }
        | ExprKind::String(_)
        | ExprKind::Boolean(_)
        | ExprKind::Nil
        | ExprKind::Shell(_)
        | ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. }
        | ExprKind::StructDef { .. } =>
        {}
    }
}

pub(super) fn collect_cache_metrics(code: &[Instruction])
-> (u64, u64, u64, u64, u64, u64, u64, u64, u64, u64)
{
    let mut bin_hits = 0u64;
    let mut bin_misses = 0u64;
    let mut idx_hits = 0u64;
    let mut idx_misses = 0u64;
    let mut map_hits = 0u64;
    let mut map_misses = 0u64;
    let mut call_hits = 0u64;
    let mut call_misses = 0u64;
    let mut global_hits = 0u64;
    let mut global_misses = 0u64;

    for inst in code
    {
        match inst
        {
            Instruction::AddCached(cache)
            | Instruction::SubCached(cache)
            | Instruction::MulCached(cache)
            | Instruction::DivCached(cache)
            | Instruction::PowCached(cache) =>
            {
                let cache = cache.borrow();
                bin_hits += cache.hits;
                bin_misses += cache.misses;
            }
            Instruction::IndexCached(cache)
            | Instruction::F64IndexCached(cache)
            | Instruction::F64IndexAssignCached(cache) =>
            {
                let cache = cache.borrow();
                idx_hits += cache.hits;
                idx_misses += cache.misses;
            }
            Instruction::MapIndexCached(cache) =>
            {
                let cache = cache.borrow();
                map_hits += cache.hits;
                map_misses += cache.misses;
            }
            Instruction::CallValueCached(cache, _) =>
            {
                let cache = cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
            }
            Instruction::CallValueCached0(cache) | Instruction::CallValueCached1(cache) =>
            {
                let cache = cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
            }
            Instruction::CallValueWithBlockCached(cache, _, _) =>
            {
                let cache = cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
            }
            Instruction::CallValueWithBlockCached0(cache, _)
            | Instruction::CallValueWithBlockCached1(cache, _) =>
            {
                let cache = cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
            }
            Instruction::CallGlobalCached(_, global_cache, call_cache, _) =>
            {
                let cache = call_cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
                let cache = global_cache.borrow();
                global_hits += cache.hits;
                global_misses += cache.misses;
            }
            Instruction::CallMethodCached(_, map_cache, call_cache, _) =>
            {
                let cache = call_cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
                let cache = map_cache.borrow();
                map_hits += cache.hits;
                map_misses += cache.misses;
            }
            Instruction::CallMethodCached0(_, map_cache, call_cache)
            | Instruction::CallMethodCached1(_, map_cache, call_cache) =>
            {
                let cache = call_cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
                let cache = map_cache.borrow();
                map_hits += cache.hits;
                map_misses += cache.misses;
            }
            Instruction::CallMethodWithBlockCached(_, map_cache, call_cache, _, _) =>
            {
                let cache = call_cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
                let cache = map_cache.borrow();
                map_hits += cache.hits;
                map_misses += cache.misses;
            }
            Instruction::CallMethodWithBlockCached0(_, map_cache, call_cache, _)
            | Instruction::CallMethodWithBlockCached1(_, map_cache, call_cache, _) =>
            {
                let cache = call_cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
                let cache = map_cache.borrow();
                map_hits += cache.hits;
                map_misses += cache.misses;
            }
            Instruction::LoadGlobalCached(_, cache) =>
            {
                let cache = cache.borrow();
                global_hits += cache.hits;
                global_misses += cache.misses;
            }
            Instruction::CallGlobalCached0(_, global_cache, call_cache)
            | Instruction::CallGlobalCached1(_, global_cache, call_cache) =>
            {
                let cache = call_cache.borrow();
                call_hits += cache.hits;
                call_misses += cache.misses;
                let cache = global_cache.borrow();
                global_hits += cache.hits;
                global_misses += cache.misses;
            }
            _ =>
            {}
        }
    }

    (
        bin_hits,
        bin_misses,
        idx_hits,
        idx_misses,
        map_hits,
        map_misses,
        call_hits,
        call_misses,
        global_hits,
        global_misses,
    )
}

pub(super) fn format_bytecode_instruction(inst: &Instruction) -> String
{
    match inst
    {
        Instruction::CallValueWithBlock(_, argc) =>
        {
            format!("CallValueWithBlock(<block>, {argc})")
        }
        Instruction::CallValueWithBlockCached(_, _, argc) =>
        {
            format!("CallValueWithBlockCached(<cache>, <block>, {argc})")
        }
        Instruction::CallValueWithBlockCached0(_, _) =>
        {
            "CallValueWithBlockCached0(<cache>, <block>)".to_string()
        }
        Instruction::CallValueWithBlockCached1(_, _) =>
        {
            "CallValueWithBlockCached1(<cache>, <block>)".to_string()
        }
        Instruction::CallValueCached0(_) => "CallValueCached0(<cache>)".to_string(),
        Instruction::CallValueCached1(_) => "CallValueCached1(<cache>)".to_string(),
        Instruction::CallMethodWithBlockCached(name, _, _, _, argc) =>
        {
            format!(
                "CallMethodWithBlockCached({:?}, <map_cache>, <call_cache>, <block>, {argc})",
                name
            )
        }
        Instruction::CallMethodWithBlockCached0(name, _, _, _) =>
        {
            format!("CallMethodWithBlockCached0({:?}, <map_cache>, <call_cache>, <block>)", name)
        }
        Instruction::CallMethodWithBlockCached1(name, _, _, _) =>
        {
            format!("CallMethodWithBlockCached1({:?}, <map_cache>, <call_cache>, <block>)", name)
        }
        Instruction::CallMethodCached0(name, _, _) =>
        {
            format!("CallMethodCached0({:?}, <map_cache>, <call_cache>)", name)
        }
        Instruction::CallMethodCached1(name, _, _) =>
        {
            format!("CallMethodCached1({:?}, <map_cache>, <call_cache>)", name)
        }
        Instruction::CallGlobalCached0(name, _, _) =>
        {
            format!("CallGlobalCached0({:?}, <global_cache>, <call_cache>)", name)
        }
        Instruction::CallGlobalCached1(name, _, _) =>
        {
            format!("CallGlobalCached1({:?}, <global_cache>, <call_cache>)", name)
        }
        Instruction::CheckBool => "CheckBool".to_string(),
        _ => format!("{:?}", inst),
    }
}

pub fn dump_bytecode(ast: &Expr, mode: BytecodeMode) -> String
{
    let mut out = String::new();

    out.push_str(&format!("Bytecode mode: {:?}\n", mode));
    out.push_str("Top-level bytecode:\n");
    {
        let simple = is_simple(ast);
        let uses_env = uses_environment(ast);
        if mode == BytecodeMode::Off
        {
            out.push_str("  <bytecode disabled>\n");
        }
        else if !should_compile(simple, uses_env, mode)
        {
            if !simple
            {
                out.push_str("  <not compiled: not simple>\n");
            }
            else if uses_env
            {
                out.push_str("  <not compiled: uses_env>\n");
            }
            else
            {
                out.push_str("  <not compiled>\n");
            }
        }
        else
        {
            let mut code = Vec::new();
            let mut consts = Vec::new();
            let use_caches = mode == BytecodeMode::Advanced;
            if with_compile_use_caches(use_caches, || {
                compile_expr(ast, &mut code, &mut consts, true)
            })
            {
                out.push_str("  Constants:\n");
                for (idx, value) in consts.iter().enumerate()
                {
                    out.push_str(&format!("  [{idx}] {}\n", value.inspect()));
                }
                out.push_str("  Bytecode:\n");
                for (idx, inst) in code.iter().enumerate()
                {
                    out.push_str(&format!("  {idx:04} {}\n", format_bytecode_instruction(inst)));
                }
                let (
                    bin_hits,
                    bin_misses,
                    idx_hits,
                    idx_misses,
                    map_hits,
                    map_misses,
                    call_hits,
                    call_misses,
                    global_hits,
                    global_misses,
                ) = collect_cache_metrics(&code);
                out.push_str(&format!(
                "  CacheMetrics bin(hits={}, misses={}) index(hits={}, misses={}) map(hits={}, misses={}) call(hits={}, misses={}) global(hits={}, misses={})\n",
                bin_hits,
                bin_misses,
                idx_hits,
                idx_misses,
                map_hits,
                map_misses,
                call_hits,
                call_misses,
                global_hits,
                global_misses
            ));
            }
            else if let Some(reason) = find_compile_failure(ast)
            {
                out.push_str(&format!("  <compile failed: {reason}>\n"));
            }
            else
            {
                out.push_str("  <compile failed: unknown reason>\n");
            }
        }
    }

    let mut functions = Vec::new();
    collect_function_exprs(ast, &mut functions);
    if !functions.is_empty()
    {
        out.push_str("Functions:\n");
    }

    for (name, params, body, slots, line) in functions
    {
        let label = match name
        {
            Some(sym) => format!("{}", symbol_name(sym).as_str()),
            None => format!("<anon@line {}>", line),
        };
        out.push_str(&format!("- {label} (line {line})\n"));

        let (resolved_body, _slot_names) = if let Some(slot_names) = slots
        {
            (body, slot_names)
        }
        else
        {
            let mut locals = HashSet::new();
            collect_declarations(&body, &mut locals);
            let (slot_map, slot_names) = build_slot_map(&params, locals);
            let mut resolved = body;
            resolve(&mut resolved, &slot_map);
            (resolved, Rc::new(slot_names))
        };

        let simple = is_simple(&resolved_body);
        let uses_env = uses_environment(&resolved_body);
        out.push_str(&format!("  simple: {}, uses_env: {}\n", simple, uses_env));

        if mode == BytecodeMode::Off
        {
            out.push_str("  <bytecode disabled>\n");
        }
        else if !should_compile(simple, uses_env, mode)
        {
            if !simple
            {
                out.push_str("  <not compiled: not simple>\n");
            }
            else if uses_env
            {
                out.push_str("  <not compiled: uses_env>\n");
            }
            else
            {
                out.push_str("  <not compiled>\n");
            }
        }
        else
        {
            let mut code = Vec::new();
            let mut consts = Vec::new();
            let use_caches = mode == BytecodeMode::Advanced;
            if with_compile_use_caches(use_caches, || {
                compile_expr(&resolved_body, &mut code, &mut consts, true)
            })
            {
                out.push_str("  Constants:\n");
                for (idx, value) in consts.iter().enumerate()
                {
                    out.push_str(&format!("  [{idx}] {}\n", value.inspect()));
                }
                out.push_str("  Bytecode:\n");
                for (idx, inst) in code.iter().enumerate()
                {
                    out.push_str(&format!("  {idx:04} {}\n", format_bytecode_instruction(inst)));
                }
                let (
                    bin_hits,
                    bin_misses,
                    idx_hits,
                    idx_misses,
                    map_hits,
                    map_misses,
                    call_hits,
                    call_misses,
                    global_hits,
                    global_misses,
                ) = collect_cache_metrics(&code);
                out.push_str(&format!(
                    "  CacheMetrics bin(hits={}, misses={}) index(hits={}, misses={}) map(hits={}, misses={}) call(hits={}, misses={}) global(hits={}, misses={})\n",
                    bin_hits,
                    bin_misses,
                    idx_hits,
                    idx_misses,
                    map_hits,
                    map_misses,
                    call_hits,
                    call_misses,
                    global_hits,
                    global_misses
                ));
            }
            else if let Some(reason) = find_compile_failure(&resolved_body)
            {
                out.push_str(&format!("  <compile failed: {reason}>\n"));
            }
            else
            {
                out.push_str("  <compile failed: unknown reason>\n");
            }
        }
    }

    out
}



pub(super) fn substitute(expr: &Expr, args: &[Expr]) -> Expr
{
    match &expr.kind
    {
        ExprKind::Identifier { slot: Some(s), .. } =>
        {
            if *s < args.len()
            {
                args[*s].clone()
            }
            else
            {
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
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } => Expr {
            kind: ExprKind::IndexAssignment {
                target: Box::new(substitute(target, args)),
                index: Box::new(substitute(index, args)),
                value: Box::new(substitute(value, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::BinaryOp { left, op, right } => Expr {
            kind: ExprKind::BinaryOp {
                left: Box::new(substitute(left, args)),
                op: op.clone(),
                right: Box::new(substitute(right, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Not(expr) => Expr {
            kind: ExprKind::Not(Box::new(substitute(expr, args))),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::And { left, right } => Expr {
            kind: ExprKind::And {
                left: Box::new(substitute(left, args)),
                right: Box::new(substitute(right, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::AndBool { left, right } => Expr {
            kind: ExprKind::AndBool {
                left: Box::new(substitute(left, args)),
                right: Box::new(substitute(right, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Or { left, right } => Expr {
            kind: ExprKind::Or {
                left: Box::new(substitute(left, args)),
                right: Box::new(substitute(right, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::OrBool { left, right } => Expr {
            kind: ExprKind::OrBool {
                left: Box::new(substitute(left, args)),
                right: Box::new(substitute(right, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Clone(expr) => Expr {
            kind: ExprKind::Clone(Box::new(substitute(expr, args))),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::EnvFreeze(expr) => Expr {
            kind: ExprKind::EnvFreeze(Box::new(substitute(expr, args))),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => Expr {
            kind: ExprKind::If {
                condition: Box::new(substitute(condition, args)),
                then_branch: Box::new(substitute(then_branch, args)),
                else_branch: else_branch.as_ref().map(|e| Box::new(substitute(e, args))),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::While { condition, body } => Expr {
            kind: ExprKind::While {
                condition: Box::new(substitute(condition, args)),
                body: Box::new(substitute(body, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::For {
            var,
            var_slot,
            iterable,
            body,
        } => Expr {
            kind: ExprKind::For {
                var: var.clone(),
                var_slot: *var_slot,
                iterable: Box::new(substitute(iterable, args)),
                body: Box::new(substitute(body, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Loop {
            count,
            var,
            var_slot,
            body,
        } => Expr {
            kind: ExprKind::Loop {
                count: Box::new(substitute(count, args)),
                var: var.clone(),
                var_slot: *var_slot,
                body: Box::new(substitute(body, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Collect {
            count,
            into,
            var,
            var_slot,
            body,
        } => Expr {
            kind: ExprKind::Collect {
                count: Box::new(substitute(count, args)),
                into: into.as_ref().map(|expr| Box::new(substitute(expr, args))),
                var: var.clone(),
                var_slot: *var_slot,
                body: Box::new(substitute(body, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Call {
            function,
            args: call_args,
            block,
            inlined_body,
        } => Expr {
            kind: ExprKind::Call {
                function: Box::new(substitute(function, args)),
                args: call_args.iter().map(|a| substitute(a, args)).collect(),
                block: block.clone(), // Blocks shouldn't be here in is_simple, but safe to clone
                inlined_body: inlined_body.clone(),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Array(elements) => Expr {
            kind: ExprKind::Array(elements.iter().map(|e| substitute(e, args)).collect()),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::ArrayGenerator { generator, size } => Expr {
            kind: ExprKind::ArrayGenerator {
                generator: Box::new(substitute(generator, args)),
                size: Box::new(substitute(size, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Map(entries) => Expr {
            kind: ExprKind::Map(
                entries
                    .iter()
                    .map(|(k, v)| (substitute(k, args), substitute(v, args)))
                    .collect(),
            ),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::StructLiteral { name, fields } => Expr {
            kind: ExprKind::StructLiteral {
                name: *name,
                fields: fields
                    .iter()
                    .map(|(field, value)| (*field, substitute(value, args)))
                    .collect(),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Index { target, index } => Expr {
            kind: ExprKind::Index {
                target: Box::new(substitute(target, args)),
                index: Box::new(substitute(index, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Slice { target, start, end } => Expr {
            kind: ExprKind::Slice {
                target: Box::new(substitute(target, args)),
                start: Box::new(substitute(start, args)),
                end: Box::new(substitute(end, args)),
            },
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Yield(args_exprs) => Expr {
            kind: ExprKind::Yield(args_exprs.iter().map(|a| substitute(a, args)).collect()),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::Block(stmts) => Expr {
            kind: ExprKind::Block(stmts.iter().map(|s| substitute(s, args)).collect()),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::FilePublic(expr) => Expr {
            kind: ExprKind::FilePublic(Box::new(substitute(expr, args))),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::FunctionPublic(expr) => Expr {
            kind: ExprKind::FunctionPublic(Box::new(substitute(expr, args))),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::FormatString(parts) => Expr {
            kind: ExprKind::FormatString(
                parts
                    .iter()
                    .map(|part| match part
                    {
                        crate::ast::FormatPart::Literal(s) =>
                        {
                            crate::ast::FormatPart::Literal(s.clone())
                        }
                        crate::ast::FormatPart::Expr { expr, spec } =>
                        {
                            crate::ast::FormatPart::Expr {
                                expr: Box::new(substitute(expr, args)),
                                spec: spec.clone(),
                            }
                        }
                    })
                    .collect(),
            ),
            line: expr.line,
            column: expr.column,
            source: expr.source.clone(),
        },
        ExprKind::StructDef { .. } | ExprKind::MethodDef { .. } => expr.clone(),
        // Literals
        _ => expr.clone(),
    }
}

pub(super) fn expr_size(expr: &Expr) -> usize
{
    match &expr.kind
    {
        ExprKind::BinaryOp { left, right, .. } => 1 + expr_size(left) + expr_size(right),
        ExprKind::Not(expr)
        | ExprKind::Clone(expr)
        | ExprKind::EnvFreeze(expr)
        | ExprKind::ErrorRaise(expr) =>
        {
            1 + expr_size(expr)
        }
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } => 1 + expr_size(left) + expr_size(right),
        ExprKind::Assignment { value, .. } => 1 + expr_size(value),
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } => 1 + expr_size(target) + expr_size(index) + expr_size(value),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            1 + expr_size(condition)
                + expr_size(then_branch)
                + else_branch.as_ref().map_or(0, |e| expr_size(e))
        }
        ExprKind::Result {
            body,
            else_expr,
            ..
        } => 1 + expr_size(body) + expr_size(else_expr),
        ExprKind::While { condition, body } => 1 + expr_size(condition) + expr_size(body),
        ExprKind::For { iterable, body, .. } => 1 + expr_size(iterable) + expr_size(body),
        ExprKind::Loop { count, body, .. } => 1 + expr_size(count) + expr_size(body),
        ExprKind::Collect { count, into, body, .. } =>
        {
            let into_size = into.as_ref().map_or(0, |expr| expr_size(expr));
            1 + expr_size(count) + into_size + expr_size(body)
        }
        ExprKind::Call { function, args, .. } =>
        {
            1 + expr_size(function) + args.iter().map(expr_size).sum::<usize>()
        }
        ExprKind::Array(elements) => 1 + elements.iter().map(expr_size).sum::<usize>(),
        ExprKind::StructLiteral { fields, .. } =>
        {
            1 + fields.iter().map(|(_, v)| expr_size(v)).sum::<usize>()
        }
        ExprKind::ArrayGenerator { generator, size } => 1 + expr_size(generator) + expr_size(size),
        ExprKind::Map(entries) =>
        {
            1 + entries
                .iter()
                .map(|(k, v)| expr_size(k) + expr_size(v))
                .sum::<usize>()
        }
        ExprKind::Index { target, index } => 1 + expr_size(target) + expr_size(index),
        ExprKind::Slice { target, start, end } =>
        {
            1 + expr_size(target) + expr_size(start) + expr_size(end)
        }
        ExprKind::Yield(args) => 1 + args.iter().map(expr_size).sum::<usize>(),
        ExprKind::Block(stmts) => 1 + stmts.iter().map(expr_size).sum::<usize>(),
        ExprKind::FilePublic(expr) => 1 + expr_size(expr),
        ExprKind::FunctionPublic(expr) => 1 + expr_size(expr),
        ExprKind::FormatString(parts) =>
        {
            1 + parts
                .iter()
                .map(|part| {
                    if let crate::ast::FormatPart::Expr { expr, .. } = part
                    {
                        expr_size(expr)
                    }
                    else
                    {
                        1
                    }
                })
                .sum::<usize>()
        }
        _ => 1,
    }
}

pub(super) fn is_reg_simple(expr: &Expr) -> bool
{
    match &expr.kind
    {
        ExprKind::Yield(_)
        | ExprKind::FunctionDef { .. }
        | ExprKind::MethodDef { .. }
        | ExprKind::AnonymousFunction { .. }
        | ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. }
        | ExprKind::FilePublic(_)
        | ExprKind::FunctionPublic(_)
        | ExprKind::StructDef { .. } => false,
        ExprKind::If { .. }
        | ExprKind::While { .. }
        | ExprKind::For { .. }
        | ExprKind::Loop { .. }
        | ExprKind::Collect { .. }
        | ExprKind::Result { .. } => false,
        ExprKind::Array(_)
        | ExprKind::ArrayGenerator { .. }
        | ExprKind::Map(_)
        | ExprKind::StructLiteral { .. }
        | ExprKind::FormatString(_)
        | ExprKind::EnvFreeze(_)
        | ExprKind::ErrorRaise(_)
        | ExprKind::Not(_)
        | ExprKind::And { .. }
        | ExprKind::AndBool { .. }
        | ExprKind::Or { .. }
        | ExprKind::OrBool { .. } => false,
        ExprKind::Block(stmts) => stmts.iter().all(is_reg_simple),
        ExprKind::BinaryOp { left, right, .. } => is_reg_simple(left) && is_reg_simple(right),
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            if block.is_some()
            {
                return false;
            }
            is_reg_simple(function) && args.iter().all(is_reg_simple)
        }
        ExprKind::Index { target, index } => is_reg_simple(target) && is_reg_simple(index),
        ExprKind::Slice { target, start, end } =>
        {
            is_reg_simple(target) && is_reg_simple(start) && is_reg_simple(end)
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } => is_reg_simple(target) && is_reg_simple(index) && is_reg_simple(value),
        ExprKind::Assignment { value, .. } => is_reg_simple(value),
        ExprKind::Clone(expr) => is_reg_simple(expr),
        _ => true,
    }
}

pub(super) struct RegAllocator
{
    next_reg: usize,
}

impl RegAllocator
{
    fn new() -> Self
    {
        Self { next_reg: 0 }
    }

    fn alloc(&mut self) -> usize
    {
        let reg = self.next_reg;
        self.next_reg += 1;
        reg
    }
}

pub(super) fn compile_reg_expr(
    expr: &Expr,
    code: &mut Vec<RegInstruction>,
    consts: &mut Vec<Value>,
    alloc: &mut RegAllocator,
) -> Option<usize>
{
    match &expr.kind
    {
        ExprKind::Integer { value, kind } =>
        {
            let dst = alloc.alloc();
            let idx = push_const(consts, make_signed_int(*value, *kind));
            code.push(RegInstruction::LoadConst { dst, idx });
            Some(dst)
        }
        ExprKind::Unsigned { value, kind } =>
        {
            let dst = alloc.alloc();
            let idx = push_const(consts, make_unsigned_int(*value, *kind));
            code.push(RegInstruction::LoadConst { dst, idx });
            Some(dst)
        }
        ExprKind::Float { value, kind } =>
        {
            let dst = alloc.alloc();
            let idx = push_const(consts, make_float(*value, *kind));
            code.push(RegInstruction::LoadConst { dst, idx });
            Some(dst)
        }
        ExprKind::Boolean(b) =>
        {
            let dst = alloc.alloc();
            let idx = push_const(consts, Value::Boolean(*b));
            code.push(RegInstruction::LoadConst { dst, idx });
            Some(dst)
        }
        ExprKind::Nil =>
        {
            let dst = alloc.alloc();
            let idx = push_const(consts, Value::Nil);
            code.push(RegInstruction::LoadConst { dst, idx });
            Some(dst)
        }
        ExprKind::String(s) =>
        {
            let dst = alloc.alloc();
            let idx = push_const(consts, Value::String(s.clone()));
            code.push(RegInstruction::LoadConst { dst, idx });
            Some(dst)
        }
        ExprKind::Identifier { slot: Some(s), .. } =>
        {
            let dst = alloc.alloc();
            code.push(RegInstruction::LoadSlot { dst, slot: *s });
            Some(dst)
        }
        ExprKind::Identifier { slot: None, .. } => None,
        ExprKind::Assignment {
            slot: Some(s),
            value,
            ..
        } =>
        {
            let src = compile_reg_expr(value, code, consts, alloc)?;
            code.push(RegInstruction::StoreSlot { slot: *s, src });
            Some(src)
        }
        ExprKind::Assignment { slot: None, .. } => None,
        ExprKind::Clone(expr) =>
        {
            let src = compile_reg_expr(expr, code, consts, alloc)?;
            let dst = alloc.alloc();
            code.push(RegInstruction::CloneValue { dst, src });
            Some(dst)
        }
        ExprKind::EnvFreeze(_) => None,
        ExprKind::Not(_) => None,
        ExprKind::And { .. }
        | ExprKind::AndBool { .. }
        | ExprKind::Or { .. }
        | ExprKind::OrBool { .. } => None,
        ExprKind::BinaryOp { left, op, right } =>
        {
            let left = compile_reg_expr(left, code, consts, alloc)?;
            let right = compile_reg_expr(right, code, consts, alloc)?;
            let op = reg_binop_from_op(op)?;
            let dst = alloc.alloc();
            code.push(RegInstruction::BinOpCached {
                dst,
                op,
                left,
                right,
                cache: Rc::new(RefCell::new(BinaryOpCache::default())),
            });
            Some(dst)
        }
        ExprKind::Index { target, index } =>
        {
            let target = compile_reg_expr(target, code, consts, alloc)?;
            if let ExprKind::String(name) = &index.kind
            {
                match name.as_str()
                {
                    "keys" =>
                    {
                        let dst = alloc.alloc();
                        code.push(RegInstruction::MapKeys { dst, src: target });
                        return Some(dst);
                    }
                    "values" =>
                    {
                        let dst = alloc.alloc();
                        code.push(RegInstruction::MapValues { dst, src: target });
                        return Some(dst);
                    }
                    _ =>
                    {}
                }
                let index = compile_reg_expr(index, code, consts, alloc)?;
                let dst = alloc.alloc();
                code.push(RegInstruction::MapIndexCached {
                    dst,
                    target,
                    index,
                    cache: Rc::new(RefCell::new(MapAccessCache::default())),
                });
                return Some(dst);
            }
            let index = compile_reg_expr(index, code, consts, alloc)?;
            let dst = alloc.alloc();
            code.push(RegInstruction::F64IndexCached {
                dst,
                target,
                index,
                cache: Rc::new(RefCell::new(IndexCache::default())),
            });
            Some(dst)
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            let target = compile_reg_expr(target, code, consts, alloc)?;
            let index = compile_reg_expr(index, code, consts, alloc)?;
            let value = compile_reg_expr(value, code, consts, alloc)?;
            let dst = alloc.alloc();
            code.push(RegInstruction::F64IndexAssignCached {
                dst,
                target,
                index,
                value,
                cache: Rc::new(RefCell::new(IndexCache::default())),
            });
            Some(dst)
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            if block.is_some()
            {
                return None;
            }
            if let ExprKind::Index { target, index } = &function.kind
            {
                if let ExprKind::String(name) = &index.kind
                {
                    if let ExprKind::Identifier {
                        name: target_name, ..
                    } = &target.kind
                    {
                        if symbol_name(*target_name).as_str() == "Bytes"
                        {
                            match name.as_str()
                            {
                                "len" if args.len() == 1 =>
                                {
                                    let src = compile_reg_expr(&args[0], code, consts, alloc)?;
                                    let dst = alloc.alloc();
                                    code.push(RegInstruction::Len { dst, src });
                                    return Some(dst);
                                }
                                "get" if args.len() == 2 =>
                                {
                                    let target_reg =
                                        compile_reg_expr(&args[0], code, consts, alloc)?;
                                    let index_reg =
                                        compile_reg_expr(&args[1], code, consts, alloc)?;
                                    let dst = alloc.alloc();
                                    code.push(RegInstruction::F64IndexCached {
                                        dst,
                                        target: target_reg,
                                        index: index_reg,
                                        cache: Rc::new(RefCell::new(IndexCache::default())),
                                    });
                                    return Some(dst);
                                }
                                _ =>
                                {}
                            }
                        }
                    }
                }
            }
            if let ExprKind::Identifier { name, .. } = &function.kind
            {
                if let Some(builtin) = builtin_from_symbol(*name)
                {
                    if matches!(builtin, Builtin::Len) && args.len() == 1
                    {
                        let src = compile_reg_expr(&args[0], code, consts, alloc)?;
                        let dst = alloc.alloc();
                        code.push(RegInstruction::Len { dst, src });
                        return Some(dst);
                    }
                }
            }
            let func = compile_reg_expr(function, code, consts, alloc)?;
            let dst = alloc.alloc();
            let cache = Rc::new(RefCell::new(crate::value::CallSiteCache::default()));
            match args.len()
            {
                0 =>
                {
                    code.push(RegInstruction::CallValueCached0 { dst, func, cache });
                }
                1 =>
                {
                    let arg0 = compile_reg_expr(&args[0], code, consts, alloc)?;
                    code.push(RegInstruction::CallValueCached1 {
                        dst,
                        func,
                        arg0,
                        cache,
                    });
                }
                2 =>
                {
                    let arg0 = compile_reg_expr(&args[0], code, consts, alloc)?;
                    let arg1 = compile_reg_expr(&args[1], code, consts, alloc)?;
                    code.push(RegInstruction::CallValueCached2 {
                        dst,
                        func,
                        arg0,
                        arg1,
                        cache,
                    });
                }
                3 =>
                {
                    let arg0 = compile_reg_expr(&args[0], code, consts, alloc)?;
                    let arg1 = compile_reg_expr(&args[1], code, consts, alloc)?;
                    let arg2 = compile_reg_expr(&args[2], code, consts, alloc)?;
                    code.push(RegInstruction::CallValueCached3 {
                        dst,
                        func,
                        arg0,
                        arg1,
                        arg2,
                        cache,
                    });
                }
                _ =>
                {
                    let mut arg_regs = Vec::with_capacity(args.len());
                    for arg in args
                    {
                        arg_regs.push(compile_reg_expr(arg, code, consts, alloc)?);
                    }
                    code.push(RegInstruction::CallValueCached {
                        dst,
                        func,
                        args: arg_regs,
                        cache,
                    });
                }
            }
            Some(dst)
        }
        ExprKind::Block(stmts) =>
        {
            let mut last = None;
            for stmt in stmts
            {
                last = compile_reg_expr(stmt, code, consts, alloc);
                if last.is_none()
                {
                    return None;
                }
            }
            last
        }
        _ => None,
    }
}

pub(super) fn compile_reg_function(expr: &Expr) -> Option<RegFunction>
{
    let mut code = Vec::new();
    let mut consts = Vec::new();
    let mut alloc = RegAllocator::new();
    let ret_reg = compile_reg_expr(expr, &mut code, &mut consts, &mut alloc)?;
    Some(RegFunction {
        code,
        reg_count: alloc.next_reg,
        ret_reg,
        const_pool: Rc::new(consts),
    })
}

pub(super) fn compile_fast_float_expr(
    expr: &Expr,
    code: &mut Vec<FastRegInstruction>,
    next_reg: &mut usize,
) -> Option<usize>
{
    match &expr.kind
    {
        ExprKind::Float {
            value,
            kind: FloatKind::F64,
        } =>
        {
            let dst = *next_reg;
            *next_reg += 1;
            code.push(FastRegInstruction::LoadConst { dst, value: *value });
            Some(dst)
        }
        ExprKind::Identifier {
            slot: Some(slot), ..
        } =>
        {
            let dst = *next_reg;
            *next_reg += 1;
            code.push(FastRegInstruction::LoadSlot { dst, slot: *slot });
            Some(dst)
        }
        ExprKind::BinaryOp { left, op, right } =>
        {
            let left = compile_fast_float_expr(left, code, next_reg)?;
            let right = compile_fast_float_expr(right, code, next_reg)?;
            let op = match op
            {
                Op::Add => RegBinOp::Add,
                Op::Subtract => RegBinOp::Sub,
                Op::Multiply => RegBinOp::Mul,
                Op::Divide => RegBinOp::Div,
                Op::Power => RegBinOp::Pow,
                _ => return None,
            };
            let dst = *next_reg;
            *next_reg += 1;
            code.push(FastRegInstruction::BinOp {
                dst,
                op,
                left,
                right,
            });
            Some(dst)
        }
        _ => None,
    }
}

pub(super) fn compile_fast_float_function(expr: &Expr) -> Option<FastRegFunction>
{
    let mut code = Vec::new();
    let mut next_reg = 0usize;
    let ret_reg = compile_fast_float_expr(expr, &mut code, &mut next_reg)?;
    Some(FastRegFunction {
        code,
        reg_count: next_reg,
        ret_reg,
    })
}

pub(super) fn is_inline_safe_arg(expr: &Expr) -> bool
{
    match &expr.kind
    {
        ExprKind::Integer { .. }
        | ExprKind::Unsigned { .. }
        | ExprKind::Float { .. }
        | ExprKind::Boolean(_)
        | ExprKind::Nil => true,
        ExprKind::Identifier { .. } => true,
        ExprKind::EnvFreeze(_) => false,
        ExprKind::Not(expr) => is_inline_safe_arg(expr),
        ExprKind::And { left, right } | ExprKind::AndBool { left, right } =>
        {
            is_inline_safe_arg(left) && is_inline_safe_arg(right)
        }
        ExprKind::Or { left, right } | ExprKind::OrBool { left, right } =>
        {
            is_inline_safe_arg(left) && is_inline_safe_arg(right)
        }
        ExprKind::BinaryOp { left, right, .. } =>
        {
            is_inline_safe_arg(left) && is_inline_safe_arg(right)
        }
        ExprKind::Index { target, index } =>
        {
            is_inline_safe_arg(target) && is_inline_safe_arg(index)
        }
        ExprKind::FormatString(parts) => parts.iter().all(|part| {
            if let crate::ast::FormatPart::Expr { expr, .. } = part
            {
                is_inline_safe_arg(expr)
            }
            else
            {
                true
            }
        }),
        _ => false,
    }
}

pub(super) fn is_simple(expr: &Expr) -> bool
{
    match &expr.kind
    {
        ExprKind::Yield(_)
        | ExprKind::FunctionDef { .. }
        | ExprKind::MethodDef { .. }
        | ExprKind::AnonymousFunction { .. }
        | ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. }
        | ExprKind::FilePublic(_)
        | ExprKind::FunctionPublic(_)
        | ExprKind::StructDef { .. } => false,
        ExprKind::Block(stmts) => stmts.iter().all(is_simple),
        ExprKind::FormatString(parts) => parts.iter().all(|part| {
            if let crate::ast::FormatPart::Expr { expr, .. } = part
            {
                is_simple(expr)
            }
            else
            {
                true
            }
        }),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            is_simple(condition)
                && is_simple(then_branch)
                && else_branch.as_ref().map_or(true, |eb| is_simple(eb))
        }
        ExprKind::While { condition, body } => is_simple(condition) && is_simple(body),
        ExprKind::For { iterable, body, .. } => is_simple(iterable) && is_simple(body),
        ExprKind::Loop { count, body, .. } => is_simple(count) && is_simple(body),
        ExprKind::Collect { count, into, body, .. } =>
        {
            let into_simple = into.as_ref().map_or(true, |expr| is_simple(expr));
            is_simple(count) && into_simple && is_simple(body)
        }
        ExprKind::Result { .. } => false,
        ExprKind::BinaryOp { left, right, .. } => is_simple(left) && is_simple(right),
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            if block.is_some()
            {
                return false;
            }
            is_simple(function) && args.iter().all(is_simple)
        }
        ExprKind::Array(elements) => elements.iter().all(is_simple),
        ExprKind::StructLiteral { fields, .. } => fields.iter().all(|(_, v)| is_simple(v)),
        ExprKind::ArrayGenerator { generator, size } => is_simple(generator) && is_simple(size),
        ExprKind::Map(entries) => entries.iter().all(|(k, v)| is_simple(k) && is_simple(v)),
        ExprKind::Index { target, index } => is_simple(target) && is_simple(index),
        ExprKind::Slice { target, start, end } =>
        {
            is_simple(target) && is_simple(start) && is_simple(end)
        }
        ExprKind::EnvFreeze(_) => false,
        ExprKind::ErrorRaise(_) => false,
        ExprKind::Not(expr) => is_simple(expr),
        ExprKind::And { left, right } | ExprKind::AndBool { left, right } =>
        {
            is_simple(left) && is_simple(right)
        }
        ExprKind::Or { left, right } | ExprKind::OrBool { left, right } =>
        {
            is_simple(left) && is_simple(right)
        }
        ExprKind::Clone(expr) => is_simple(expr),
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } => is_simple(target) && is_simple(index) && is_simple(value),
        _ => true,
    }
}

pub(super) fn should_compile(simple: bool, _uses_env: bool, mode: BytecodeMode) -> bool
{
    match mode
    {
        BytecodeMode::Off => false,
        BytecodeMode::Simple => simple && !_uses_env,
        BytecodeMode::Advanced => simple && !_uses_env,
    }
}

pub(super) fn resolve(expr: &mut Expr, slot_map: &FxHashMap<SymbolId, usize>)
{
    // Only address-taken locals need environment storage. This analysis is
    // transient: ordinary locals and execution caches keep their existing layout.
    let mut referenced = FxHashSet::default();
    walk_local_exprs(expr, &mut |expr| {
        if let ExprKind::Reference(name) = &expr.kind
        {
            referenced.insert(*name);
        }
    });
    let slot_for = |name: &SymbolId| {
        if referenced.contains(name) { None } else { slot_map.get(name).copied() }
    };
    walk_local_exprs(expr, &mut |expr| {
        match &mut expr.kind
        {
            ExprKind::Identifier { name, slot }
            | ExprKind::Assignment { name, slot, .. } => *slot = slot_for(name),
            ExprKind::For { var, var_slot, .. } => *var_slot = slot_for(var),
            ExprKind::Loop { var, var_slot, .. }
            | ExprKind::Collect { var, var_slot, .. } =>
                *var_slot = var.as_ref().and_then(&slot_for),
            ExprKind::Result { else_binding, else_slot, .. } =>
                *else_slot = else_binding.as_ref().and_then(&slot_for),
            _ => {}
        }
    });
}

// Visit one lexical scope; nested functions resolve their own parameters/locals.
fn walk_local_exprs(expr: &mut Expr, visit: &mut impl FnMut(&mut Expr))
{
    visit(expr);
    match &mut expr.kind
    {
        ExprKind::Assignment { value, .. } =>
        {
            walk_local_exprs(value, visit);
        }
        ExprKind::FilePublic(expr) =>
        {
            walk_local_exprs(expr, visit);
        }
        ExprKind::FunctionPublic(expr) =>
        {
            walk_local_exprs(expr, visit);
        }
        ExprKind::BinaryOp { left, right, .. } =>
        {
            walk_local_exprs(left, visit);
            walk_local_exprs(right, visit);
        }
        ExprKind::Block(stmts) =>
        {
            for stmt in stmts
            {
                walk_local_exprs(stmt, visit);
            }
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            walk_local_exprs(condition, visit);
            walk_local_exprs(then_branch, visit);
            if let Some(eb) = else_branch
            {
                walk_local_exprs(eb, visit);
            }
        }
        ExprKind::Result {
            body,
            else_expr,
            ..
        } =>
        {
            walk_local_exprs(body, visit);
            walk_local_exprs(else_expr, visit);
        }
        ExprKind::While { condition, body } =>
        {
            walk_local_exprs(condition, visit);
            walk_local_exprs(body, visit);
        }
        ExprKind::For {
            iterable,
            body,
            ..
        } =>
        {
            walk_local_exprs(iterable, visit);
            walk_local_exprs(body, visit);
        }
        ExprKind::Loop {
            count,
            body,
            ..
        } =>
        {
            walk_local_exprs(count, visit);
            walk_local_exprs(body, visit);
        }
        ExprKind::Collect {
            count,
            into,
            body,
            ..
        } =>
        {
            walk_local_exprs(count, visit);
            if let Some(into) = into
            {
                walk_local_exprs(into, visit);
            }
            walk_local_exprs(body, visit);
        }
        ExprKind::Call {
            function,
            args,
            block,
            ..
        } =>
        {
            walk_local_exprs(function, visit);
            for arg in args
            {
                walk_local_exprs(arg, visit);
            }
            if let Some(c) = block
            {
                walk_local_exprs(&mut c.body, visit);
            }
        }
        ExprKind::Array(elements) =>
        {
            for e in elements
            {
                walk_local_exprs(e, visit);
            }
        }
        ExprKind::StructLiteral { fields, .. } =>
        {
            for (_, expr) in fields
            {
                walk_local_exprs(expr, visit);
            }
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            walk_local_exprs(generator, visit);
            walk_local_exprs(size, visit);
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                walk_local_exprs(k, visit);
                walk_local_exprs(v, visit);
            }
        }
        ExprKind::Index { target, index } =>
        {
            walk_local_exprs(target, visit);
            walk_local_exprs(index, visit);
        }
        ExprKind::Slice { target, start, end } =>
        {
            walk_local_exprs(target, visit);
            walk_local_exprs(start, visit);
            walk_local_exprs(end, visit);
        }
        ExprKind::Not(expr) =>
        {
            walk_local_exprs(expr, visit);
        }
        ExprKind::And { left, right } | ExprKind::AndBool { left, right } =>
        {
            walk_local_exprs(left, visit);
            walk_local_exprs(right, visit);
        }
        ExprKind::Or { left, right } | ExprKind::OrBool { left, right } =>
        {
            walk_local_exprs(left, visit);
            walk_local_exprs(right, visit);
        }
        ExprKind::Clone(expr) =>
        {
            walk_local_exprs(expr, visit);
        }
        ExprKind::ErrorRaise(expr) =>
        {
            walk_local_exprs(expr, visit);
        }
        ExprKind::EnvFreeze(expr) =>
        {
            walk_local_exprs(expr, visit);
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            walk_local_exprs(target, visit);
            walk_local_exprs(index, visit);
            walk_local_exprs(value, visit);
        }
        ExprKind::FormatString(parts) =>
        {
            for part in parts
            {
                if let crate::ast::FormatPart::Expr { expr, .. } = part
                {
                    walk_local_exprs(expr, visit);
                }
            }
        }
        ExprKind::Use(_)
        | ExprKind::Load(_)
        | ExprKind::Import { .. }
        | ExprKind::Export { .. } =>
        {}
        ExprKind::Yield(args) =>
        {
            for a in args
            {
                walk_local_exprs(a, visit);
            }
        }
        _ =>
        {}
    }
}

