//! Opt-in, in-process performance baselines; absent from production builds.
use super::*;
use std::hint::black_box;
use std::time::{Duration, Instant};

fn measure(name: &str, mut operation: impl FnMut())
{
    for _ in 0..128 { operation(); }
    let mut iterations = 1u64;
    loop
    {
        let start = Instant::now();
        for _ in 0..iterations { operation(); }
        if start.elapsed() >= Duration::from_millis(20) { break; }
        iterations *= 2;
    }
    let mut samples = Vec::new();
    for _ in 0..9
    {
        let start = Instant::now();
        for _ in 0..iterations { operation(); }
        samples.push(start.elapsed().as_nanos() as f64 / iterations as f64);
    }
    let mut sorted = samples.clone();
    sorted.sort_by(f64::total_cmp);
    println!("KANSEI_BENCH {}", serde_json::json!({
        "name": name, "iterations_per_sample": iterations,
        "samples_ns": samples, "median_ns": sorted[4],
        "min_ns": sorted[0], "max_ns": sorted[8],
    }));
}

#[test]
#[ignore = "run through scripts/benchmark.py with a release build"]
fn performance_baselines()
{
    assert!(!cfg!(debug_assertions), "benchmarks require --release");
    let source: String = (0..100).map(|i| format!(
        "# function {i}\nfn sample_{i}(x)\n  (x * 1.25 + 2.0) / (x + 1.0)\nend\n"
    )).collect();
    measure("parse_100_functions", || {
        black_box(crate::parser::parse_source(black_box(&source)).unwrap());
    });
    let ast = crate::parser::parse_source(&source).unwrap();
    measure("resolve_100_cloned_functions", || {
        let mut ast = black_box(&ast).clone();
        resolve_slots(&mut ast);
        black_box(ast);
    });
    measure("format_100_functions", || {
        black_box(crate::formatter::format_source(black_box(&source)).unwrap());
    });

    for (label, mode) in [("off", BytecodeMode::Off), ("simple", BytecodeMode::Simple),
                          ("advanced", BytecodeMode::Advanced)]
    {
        let mut interpreter = Interpreter::new();
        interpreter.set_bytecode_mode(mode);
        for (name, source, args, expected) in [
            ("arithmetic", "fn arithmetic(x)\n (x * 1.25 + 2.0) / (x + 1.0)\nend",
             smallvec::smallvec![make_float(3.0, FloatKind::F64)], make_float(1.4375, FloatKind::F64)),
            ("index", "fn index(items, i)\n items[i]\nend",
             smallvec::smallvec![Value::F64Array(Rc::new(RefCell::new(vec![1.0, 2.0, 3.0]))),
                                 make_signed_int(1, IntKind::I64)], make_float(2.0, FloatKind::F64)),
        ]
        {
            let mut definition = crate::parser::parse_source(source).unwrap();
            resolve_slots(&mut definition);
            interpreter.eval(&definition, &mut []).unwrap();
            let function = interpreter.env.borrow().get(intern::intern_symbol(name)).unwrap();
            if name == "arithmetic" && mode != BytecodeMode::Off
            {
                let Value::Function(data) = &function else { panic!("expected function") };
                assert!(data.reg_code.is_some() || data.code.is_some(), "benchmark must execute compiled code");
            }
            assert_eq!(interpreter.call_value(function.clone(), args.clone(), 0, None).unwrap(), expected);
            let case = if name == "index" { "index_ast_fallback" } else { name };
            measure(&format!("execute_{case}_{label}"), || {
                black_box(interpreter.call_value(black_box(function.clone()), args.clone(), 0, None).unwrap());
            });
        }
    }

    let cache = Rc::new(RefCell::new(BinaryOpCache::default()));
    let left = make_float(3.0, FloatKind::F64);
    let right = make_float(4.0, FloatKind::F64);
    measure("arithmetic_cache_warm", || {
        black_box(eval_cached_binop(BinOpKind::Add, &cache, black_box(left.clone()), right.clone()).unwrap());
    });
    assert_eq!(cache.borrow().misses, 1);
    assert!(cache.borrow().hits > 0);
    measure("arithmetic_cache_cold", || {
        let cache = Rc::new(RefCell::new(BinaryOpCache::default()));
        black_box(eval_cached_binop(BinOpKind::Add, &cache, black_box(left.clone()), right.clone()).unwrap());
    });

    let array = Value::F64Array(Rc::new(RefCell::new(vec![1.0, 2.0, 3.0])));
    let index = make_signed_int(1, IntKind::I64);
    let cache = Rc::new(RefCell::new(IndexCache::default()));
    measure("index_cache_warm", || {
        black_box(eval_f64_index_cached_value(black_box(index.clone()), array.clone(), &cache).unwrap());
    });
    assert_eq!(cache.borrow().misses, 1);
    assert!(cache.borrow().hits > 0);
    measure("index_cache_cold", || {
        let cache = Rc::new(RefCell::new(IndexCache::default()));
        black_box(eval_f64_index_cached_value(black_box(index.clone()), array.clone(), &cache).unwrap());
    });
}
