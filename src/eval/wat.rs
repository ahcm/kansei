//! WAT generation from the GitHub compiler implementation.
use super::*;

#[derive(Clone, Copy)]
struct WatRuntimeStrings
{
    args: (i32, i32),
    float64: (i32, i32),
    int64: (i32, i32),
    sqrt: (i32, i32),
    parse: (i32, i32),
}

impl WatRuntimeStrings
{
    fn new(ctx: &mut WatContext) -> Self
    {
        let args = ctx.push_data(b"args");
        let float64 = ctx.push_data(b"Float64");
        let int64 = ctx.push_data(b"Int64");
        let sqrt = ctx.push_data(b"sqrt");
        let parse = ctx.push_data(b"parse");
        Self {
            args: (args, 4),
            float64: (float64, 7),
            int64: (int64, 5),
            sqrt: (sqrt, 4),
            parse: (parse, 5),
        }
    }
}

fn emit_wat_runtime(ctx: &mut WatContext, wasi: WasiTarget, _rt: &WatRuntimeStrings)
{
    let out = &mut ctx.out;
    if wasi == WasiTarget::Wasip1
    {
        out.push_str("  (import \"wasi_snapshot_preview1\" \"fd_write\" (func $fd_write (param i32 i32 i32 i32) (result i32)))\n");
        out.push_str("  (import \"wasi_snapshot_preview1\" \"args_sizes_get\" (func $args_sizes_get (param i32 i32) (result i32)))\n");
        out.push_str("  (import \"wasi_snapshot_preview1\" \"args_get\" (func $args_get (param i32 i32) (result i32)))\n");
    }
    out.push_str("  (memory (export \"memory\") 1)\n");
    out.push_str("  (global $heap_ptr (mut i32) (i32.const 1024))\n");
    out.push_str("  (global $TAG_PTR i64 (i64.const 0))\n");
    out.push_str("  (global $TAG_INT i64 (i64.const 1))\n");
    out.push_str("  (global $TAG_BOOL i64 (i64.const 2))\n");
    out.push_str("  (global $TAG_NIL i64 (i64.const 3))\n");
    out.push_str("  (global $MASK_TAG i64 (i64.const 7))\n");
    out.push_str("  (global $TYPE_F64 i32 (i32.const 1))\n");
    out.push_str("  (global $TYPE_STRING i32 (i32.const 2))\n");
    out.push_str("  (global $TYPE_MAP i32 (i32.const 3))\n");
    out.push_str("  (global $TYPE_ARRAY i32 (i32.const 4))\n");
    out.push_str("  (global $TYPE_F64ARRAY i32 (i32.const 5))\n");
    out.push_str("  (global $TYPE_FUNC i32 (i32.const 6))\n");
    out.push_str("  (global $TYPE_ENV i32 (i32.const 7))\n");
    out.push_str("  (global $TYPE_REF i32 (i32.const 8))\n");
    out.push_str("  (global $globals_ptr (mut i32) (i32.const 0))\n");
    out.push_str("  (global $args_sp (mut i32) (i32.const 0))\n");
    out.push_str("  (global $call_args_ptr (mut i32) (i32.const 0))\n");
    out.push_str("  (global $call_args_cap (mut i32) (i32.const 0))\n");
    out.push_str("  (type $fn (func (param i64 i32 i32) (result i64)))\n");

    out.push_str(
        "  (func $alloc (param $size i32) (result i32)\n\
            (local $old i32)\n\
            (local $aligned i32)\n\
            (local $needed i32)\n\
            (local $have i32)\n\
            local.get $size\n\
            i32.const 7\n\
            i32.add\n\
            i32.const -8\n\
            i32.and\n\
            local.set $aligned\n\
            global.get $heap_ptr\n\
            local.set $old\n\
            local.get $old\n\
            local.get $aligned\n\
            i32.add\n\
            local.set $needed\n\
            memory.size\n\
            i32.const 16\n\
            i32.shl\n\
            local.set $have\n\
            local.get $needed\n\
            local.get $have\n\
            i32.gt_u\n\
            if\n\
              local.get $needed\n\
              local.get $have\n\
              i32.sub\n\
              i32.const 65535\n\
              i32.add\n\
              i32.const 16\n\
              i32.shr_u\n\
              memory.grow\n\
              drop\n\
            end\n\
            local.get $old\n\
            local.get $aligned\n\
            i32.add\n\
            global.set $heap_ptr\n\
            local.get $old\n\
          )\n",
    );

    out.push_str(
        "  (func $call_args_alloc (param $size i32) (result i32)\n\
            global.get $call_args_cap\n\
            local.get $size\n\
            i32.lt_u\n\
            if\n\
              local.get $size\n\
              call $alloc\n\
              global.set $call_args_ptr\n\
              local.get $size\n\
              global.set $call_args_cap\n\
            end\n\
            global.get $call_args_ptr\n\
          )\n",
    );

    out.push_str(
        "  (func $globals_init (param $count i32)\n\
            (local $ptr i32)\n\
            (local $i i32)\n\
            local.get $count\n\
            i32.const 8\n\
            i32.mul\n\
            call $alloc\n\
            local.set $ptr\n\
            local.get $ptr\n\
            global.set $globals_ptr\n\
            i32.const 0\n\
            local.set $i\n\
            block $exit\n\
              loop $loop\n\
                local.get $i\n\
                local.get $count\n\
                i32.ge_u\n\
                br_if $exit\n\
                local.get $ptr\n\
                local.get $i\n\
                i32.const 8\n\
                i32.mul\n\
                i32.add\n\
                call $tag_nil\n\
                i64.store\n\
                local.get $i\n\
                i32.const 1\n\
                i32.add\n\
                local.set $i\n\
                br $loop\n\
              end\n\
            end\n\
          )\n",
    );

    out.push_str(
        "  (func $args_push (param $v i32)\n\
            global.get $args_sp\n\
            local.get $v\n\
            i32.store\n\
            global.get $args_sp\n\
            i32.const 4\n\
            i32.add\n\
            global.set $args_sp\n\
          )\n",
    );

    out.push_str(
        "  (func $args_pop (result i32)\n\
            global.get $args_sp\n\
            i32.const 4\n\
            i32.sub\n\
            global.set $args_sp\n\
            global.get $args_sp\n\
            i32.load\n\
          )\n",
    );

    out.push_str(
        "  (func $global_get (param $idx i32) (result i64)\n\
            global.get $globals_ptr\n\
            local.get $idx\n\
            i32.const 8\n\
            i32.mul\n\
            i32.add\n\
            i64.load\n\
          )\n",
    );
    out.push_str(
        "  (func $global_set (param $idx i32) (param $value i64) (result i64)\n\
            global.get $globals_ptr\n\
            local.get $idx\n\
            i32.const 8\n\
            i32.mul\n\
            i32.add\n\
            local.get $value\n\
            i64.store\n\
            local.get $value\n\
          )\n",
    );

    out.push_str(
        "  (func $env_new (param $count i32) (result i64)\n\
            (local $ptr i32)\n\
            (local $i i32)\n\
            local.get $count\n\
            i32.const 8\n\
            i32.mul\n\
            i32.const 8\n\
            i32.add\n\
            call $alloc\n\
            local.set $ptr\n\
            local.get $ptr\n\
            global.get $TYPE_ENV\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            local.get $count\n\
            i32.store\n\
            i32.const 0\n\
            local.set $i\n\
            block $exit\n\
              loop $loop\n\
                local.get $i\n\
                local.get $count\n\
                i32.ge_u\n\
                br_if $exit\n\
                local.get $ptr\n\
                i32.const 8\n\
                i32.add\n\
                local.get $i\n\
                i32.const 8\n\
                i32.mul\n\
                i32.add\n\
                call $tag_nil\n\
                i64.store\n\
                local.get $i\n\
                i32.const 1\n\
                i32.add\n\
                local.set $i\n\
                br $loop\n\
              end\n\
            end\n\
            local.get $ptr\n\
            i64.extend_i32_u\n\
            i64.const 3\n\
            i64.shl\n\
            global.get $TAG_PTR\n\
            i64.or\n\
          )\n",
    );

    out.push_str(
        "  (func $env_get (param $env i64) (param $idx i32) (result i64)\n\
            (local $ptr i32)\n\
            local.get $env\n\
            global.get $TAG_PTR\n\
            call $is_tag\n\
            i32.eqz\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $env\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.load\n\
            global.get $TYPE_ENV\n\
            i32.ne\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            local.get $idx\n\
            i32.const 8\n\
            i32.mul\n\
            i32.add\n\
            i64.load\n\
          )\n",
    );

    out.push_str(
        "  (func $env_set (param $env i64) (param $idx i32) (param $value i64) (result i64)\n\
            (local $ptr i32)\n\
            local.get $env\n\
            global.get $TAG_PTR\n\
            call $is_tag\n\
            i32.eqz\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $env\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.load\n\
            global.get $TYPE_ENV\n\
            i32.ne\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            local.get $idx\n\
            i32.const 8\n\
            i32.mul\n\
            i32.add\n\
            local.get $value\n\
            i64.store\n\
            local.get $value\n\
          )\n",
    );

    out.push_str(
        "  (func $make_ref (param $env i64) (param $idx i32) (result i64)\n\
            (local $ptr i32)\n\
            i32.const 12\n\
            call $alloc\n\
            local.set $ptr\n\
            local.get $ptr\n\
            global.get $TYPE_REF\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            local.get $idx\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            local.get $env\n\
            i64.store\n\
            local.get $ptr\n\
            i64.extend_i32_u\n\
            i64.const 3\n\
            i64.shl\n\
            global.get $TAG_PTR\n\
            i64.or\n\
          )\n",
    );

    out.push_str(
        "  (func $is_ref (param $v i64) (result i32)\n\
            (local $ptr i32)\n\
            local.get $v\n\
            global.get $TAG_PTR\n\
            call $is_tag\n\
            if (result i32)\n\
              local.get $v\n\
              call $ptr_of\n\
              local.set $ptr\n\
              local.get $ptr\n\
              i32.load\n\
              global.get $TYPE_REF\n\
              i32.eq\n\
            else\n\
              i32.const 0\n\
            end\n\
          )\n",
    );

    out.push_str(
        "  (func $ref_env (param $v i64) (result i64)\n\
            (local $ptr i32)\n\
            local.get $v\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            i64.load\n\
          )\n",
    );

    out.push_str(
        "  (func $ref_index (param $v i64) (result i32)\n\
            (local $ptr i32)\n\
            local.get $v\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
          )\n",
    );

    out.push_str(
        "  (func $ref_get (param $v i64) (result i64)\n\
            (local $env i64)\n\
            (local $idx i32)\n\
            local.get $v\n\
            call $ref_env\n\
            local.set $env\n\
            local.get $v\n\
            call $ref_index\n\
            local.set $idx\n\
            local.get $env\n\
            global.get $TAG_NIL\n\
            call $is_tag\n\
            if (result i64)\n\
              local.get $idx\n\
              call $global_get\n\
            else\n\
              local.get $env\n\
              local.get $idx\n\
              call $env_get\n\
            end\n\
          )\n",
    );

    out.push_str(
        "  (func $ref_set (param $v i64) (param $value i64) (result i64)\n\
            (local $env i64)\n\
            (local $idx i32)\n\
            local.get $v\n\
            call $ref_env\n\
            local.set $env\n\
            local.get $v\n\
            call $ref_index\n\
            local.set $idx\n\
            local.get $env\n\
            global.get $TAG_NIL\n\
            call $is_tag\n\
            if (result i64)\n\
              local.get $idx\n\
              local.get $value\n\
              call $global_set\n\
            else\n\
              local.get $env\n\
              local.get $idx\n\
              local.get $value\n\
              call $env_set\n\
            end\n\
          )\n",
    );

    out.push_str(
        "  (func $make_func (param $func_idx i32) (param $env i64) (result i64)\n\
            (local $ptr i32)\n\
            i32.const 16\n\
            call $alloc\n\
            local.set $ptr\n\
            local.get $ptr\n\
            global.get $TYPE_FUNC\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            local.get $func_idx\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            local.get $env\n\
            i64.store\n\
            local.get $ptr\n\
            i64.extend_i32_u\n\
            i64.const 3\n\
            i64.shl\n\
            global.get $TAG_PTR\n\
            i64.or\n\
          )\n",
    );

    out.push_str(
        "  (func $func_index_of (param $v i64) (result i32)\n\
            (local $ptr i32)\n\
            local.get $v\n\
            global.get $TAG_PTR\n\
            call $is_tag\n\
            i32.eqz\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $v\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.load\n\
            global.get $TYPE_FUNC\n\
            i32.ne\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
          )\n",
    );

    out.push_str(
        "  (func $func_env_of (param $v i64) (result i64)\n\
            (local $ptr i32)\n\
            local.get $v\n\
            global.get $TAG_PTR\n\
            call $is_tag\n\
            i32.eqz\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $v\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.load\n\
            global.get $TYPE_FUNC\n\
            i32.ne\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            i64.load\n\
          )\n",
    );

    out.push_str(
        "  (func $is_func (param $v i64) (result i32)\n\
            (local $ptr i32)\n\
            local.get $v\n\
            global.get $TAG_PTR\n\
            call $is_tag\n\
            if (result i32)\n\
              local.get $v\n\
              call $ptr_of\n\
              local.set $ptr\n\
              local.get $ptr\n\
              i32.load\n\
              global.get $TYPE_FUNC\n\
              i32.eq\n\
            else\n\
              i32.const 0\n\
            end\n\
          )\n",
    );

    out.push_str(
        "  (func $call_func (param $func i64) (param $args_ptr i32) (param $argc i32) (result i64)\n\
            (local $func_idx i32)\n\
            (local $env i64)\n\
            local.get $func\n\
            call $func_index_of\n\
            local.set $func_idx\n\
            local.get $func\n\
            call $func_env_of\n\
            local.set $env\n\
            local.get $env\n\
            local.get $args_ptr\n\
            local.get $argc\n\
            local.get $func_idx\n\
            call_indirect (type $fn)\n\
          )\n",
    );

    out.push_str(
        "  (func $tag_int (param $v i64) (result i64)\n\
            local.get $v\n\
            i64.const 3\n\
            i64.shl\n\
            global.get $TAG_INT\n\
            i64.or\n\
          )\n",
    );
    out.push_str(
        "  (func $untag_int (param $v i64) (result i64)\n\
            local.get $v\n\
            i64.const 3\n\
            i64.shr_s\n\
          )\n",
    );
    out.push_str(
        "  (func $tag_bool (param $v i32) (result i64)\n\
            local.get $v\n\
            i64.extend_i32_u\n\
            i64.const 3\n\
            i64.shl\n\
            global.get $TAG_BOOL\n\
            i64.or\n\
          )\n",
    );
    out.push_str(
        "  (func $tag_nil (result i64)\n\
            global.get $TAG_NIL\n\
          )\n",
    );
    out.push_str(
        "  (func $ptr_of (param $v i64) (result i32)\n\
            local.get $v\n\
            i64.const 3\n\
            i64.shr_u\n\
            i32.wrap_i64\n\
          )\n",
    );
    out.push_str(
        "  (func $is_tag (param $v i64) (param $tag i64) (result i32)\n\
            local.get $v\n\
            global.get $MASK_TAG\n\
            i64.and\n\
            local.get $tag\n\
            i64.eq\n\
          )\n",
    );

    out.push_str(
        "  (func $box_f64 (param $val f64) (result i64)\n\
            (local $ptr i32)\n\
            i32.const 16\n\
            call $alloc\n\
            local.set $ptr\n\
            local.get $ptr\n\
            global.get $TYPE_F64\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            local.get $val\n\
            f64.store\n\
            local.get $ptr\n\
            i64.extend_i32_u\n\
            i64.const 3\n\
            i64.shl\n\
            global.get $TAG_PTR\n\
            i64.or\n\
          )\n",
    );

    out.push_str(
        "  (func $string_new_from_data (param $src i32) (param $len i32) (result i64)\n\
            (local $ptr i32)\n\
            local.get $len\n\
            i32.const 8\n\
            i32.add\n\
            call $alloc\n\
            local.set $ptr\n\
            local.get $ptr\n\
            global.get $TYPE_STRING\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            local.get $len\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            local.get $src\n\
            local.get $len\n\
            memory.copy\n\
            local.get $ptr\n\
            i64.extend_i32_u\n\
            i64.const 3\n\
            i64.shl\n\
            global.get $TAG_PTR\n\
            i64.or\n\
          )\n",
    );

    if wasi == WasiTarget::Wasip1
    {
        out.push_str(
            "  (func $cstr_len (param $ptr i32) (result i32)\n\
            (local $len i32)\n\
            (loop $loop\n\
              local.get $ptr\n\
              local.get $len\n\
              i32.add\n\
              i32.load8_u\n\
              i32.eqz\n\
              if\n\
                local.get $len\n\
                return\n\
              end\n\
              local.get $len\n\
              i32.const 1\n\
              i32.add\n\
              local.set $len\n\
              br $loop\n\
            )\n\
            i32.const 0\n\
          )\n",
        );

        out.push_str(
            "  (func $args_to_array (result i64)\n\
            (local $tmp i32)\n\
            (local $argc i32)\n\
            (local $buf_size i32)\n\
            (local $argv_ptr i32)\n\
            (local $buf_ptr i32)\n\
            (local $out_count i32)\n\
            (local $arr i64)\n\
            (local $i i32)\n\
            (local $ptr i32)\n\
            (local $len i32)\n\
            (local $str i64)\n\
            i32.const 8\n\
            call $alloc\n\
            local.set $tmp\n\
            local.get $tmp\n\
            local.get $tmp\n\
            i32.const 4\n\
            i32.add\n\
            call $args_sizes_get\n\
            drop\n\
            local.get $tmp\n\
            i32.load\n\
            local.set $argc\n\
            local.get $tmp\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            local.set $buf_size\n\
            local.get $argc\n\
            i32.const 4\n\
            i32.mul\n\
            call $alloc\n\
            local.set $argv_ptr\n\
            local.get $buf_size\n\
            call $alloc\n\
            local.set $buf_ptr\n\
            local.get $argv_ptr\n\
            local.get $buf_ptr\n\
            call $args_get\n\
            drop\n\
            local.get $argc\n\
            i32.eqz\n\
            if\n\
              i32.const 0\n\
              local.set $out_count\n\
            else\n\
              local.get $argc\n\
              i32.const 1\n\
              i32.sub\n\
              local.set $out_count\n\
            end\n\
            local.get $out_count\n\
            call $array_new\n\
            local.set $arr\n\
            i32.const 0\n\
            local.set $i\n\
            (block $done (result i64)\n\
              (loop $loop\n\
                local.get $i\n\
                local.get $out_count\n\
                i32.ge_u\n\
                if\n\
                  local.get $arr\n\
                  br $done\n\
                end\n\
                local.get $argv_ptr\n\
                local.get $i\n\
                i32.const 1\n\
                i32.add\n\
                i32.const 4\n\
                i32.mul\n\
                i32.add\n\
                i32.load\n\
                local.set $ptr\n\
                local.get $ptr\n\
                call $cstr_len\n\
                local.set $len\n\
                local.get $ptr\n\
                local.get $len\n\
                call $string_new_from_data\n\
                local.set $str\n\
                local.get $arr\n\
                local.get $i\n\
                local.get $str\n\
                call $array_set\n\
                drop\n\
                local.get $i\n\
                i32.const 1\n\
                i32.add\n\
                local.set $i\n\
                br $loop\n\
              )\n\
              local.get $arr\n\
            )\n\
          )\n",
        );
    }

    out.push_str(
        "  (func $string_concat (param $a i64) (param $b i64) (result i64)\n\
            (local $pa i32)\n\
            (local $pb i32)\n\
            (local $la i32)\n\
            (local $lb i32)\n\
            (local $dst i32)\n\
            local.get $a\n\
            call $ptr_of\n\
            local.set $pa\n\
            local.get $b\n\
            call $ptr_of\n\
            local.set $pb\n\
            local.get $pa\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            local.set $la\n\
            local.get $pb\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            local.set $lb\n\
            local.get $la\n\
            local.get $lb\n\
            i32.add\n\
            call $alloc\n\
            local.set $dst\n\
            local.get $dst\n\
            local.get $pa\n\
            i32.const 8\n\
            i32.add\n\
            local.get $la\n\
            memory.copy\n\
            local.get $dst\n\
            local.get $la\n\
            i32.add\n\
            local.get $pb\n\
            i32.const 8\n\
            i32.add\n\
            local.get $lb\n\
            memory.copy\n\
            local.get $dst\n\
            local.get $la\n\
            local.get $lb\n\
            i32.add\n\
            call $string_new_from_data\n\
          )\n",
    );

    out.push_str(
        "  (func $value_to_string (param $v i64) (result i32 i32)\n\
            (local $ptr i32)\n\
            (local $len i32)\n\
            (local $tmp i32)\n\
            local.get $v\n\
            call $is_ref\n\
            if\n\
              local.get $v\n\
              call $ref_get\n\
              call $value_to_string\n\
              return\n\
            end\n\
            local.get $v\n\
            global.get $TAG_INT\n\
            call $is_tag\n\
            if\n\
              local.get $v\n\
              call $untag_int\n\
              call $int_to_string\n\
              local.set $len\n\
              local.set $ptr\n\
            else\n\
              local.get $v\n\
              global.get $TAG_BOOL\n\
              call $is_tag\n\
              if\n\
                local.get $v\n\
                call $untag_int\n\
                i64.eqz\n\
                if\n\
                  i32.const 5\n\
                  call $alloc\n\
                  local.set $ptr\n\
                  local.get $ptr\n\
                  i32.const 102\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 1\n\
                  i32.add\n\
                  i32.const 97\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 2\n\
                  i32.add\n\
                  i32.const 108\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 3\n\
                  i32.add\n\
                  i32.const 115\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 4\n\
                  i32.add\n\
                  i32.const 101\n\
                  i32.store8\n\
                  i32.const 5\n\
                  local.set $len\n\
                else\n\
                  i32.const 4\n\
                  call $alloc\n\
                  local.set $ptr\n\
                  local.get $ptr\n\
                  i32.const 116\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 1\n\
                  i32.add\n\
                  i32.const 114\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 2\n\
                  i32.add\n\
                  i32.const 117\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 3\n\
                  i32.add\n\
                  i32.const 101\n\
                  i32.store8\n\
                  i32.const 4\n\
                  local.set $len\n\
                end\n\
              else\n\
                local.get $v\n\
                global.get $TAG_NIL\n\
                call $is_tag\n\
                if\n\
                  i32.const 3\n\
                  call $alloc\n\
                  local.set $ptr\n\
                  local.get $ptr\n\
                  i32.const 110\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 1\n\
                  i32.add\n\
                  i32.const 105\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 2\n\
                  i32.add\n\
                  i32.const 108\n\
                  i32.store8\n\
                  i32.const 3\n\
                  local.set $len\n\
                else\n\
                  local.get $v\n\
                  global.get $TAG_PTR\n\
                  call $is_tag\n\
                  if\n\
                    local.get $v\n\
                    call $ptr_of\n\
                    local.set $tmp\n\
                    local.get $tmp\n\
                    i32.load\n\
                    global.get $TYPE_STRING\n\
                    i32.eq\n\
                    if\n\
                      local.get $tmp\n\
                      i32.const 4\n\
                      i32.add\n\
                      i32.load\n\
                      local.set $len\n\
                      local.get $tmp\n\
                      i32.const 8\n\
                      i32.add\n\
                      local.set $ptr\n\
                    else\n\
                      local.get $tmp\n\
                      i32.load\n\
                      global.get $TYPE_ARRAY\n\
                      i32.eq\n\
                      if\n\
                        i32.const 5\n\
                        call $alloc\n\
                        local.set $ptr\n\
                        local.get $ptr\n\
                        i32.const 97\n\
                        i32.store8\n\
                        local.get $ptr\n\
                        i32.const 1\n\
                        i32.add\n\
                        i32.const 114\n\
                        i32.store8\n\
                        local.get $ptr\n\
                        i32.const 2\n\
                        i32.add\n\
                        i32.const 114\n\
                        i32.store8\n\
                        local.get $ptr\n\
                        i32.const 3\n\
                        i32.add\n\
                        i32.const 97\n\
                        i32.store8\n\
                        local.get $ptr\n\
                        i32.const 4\n\
                        i32.add\n\
                        i32.const 121\n\
                        i32.store8\n\
                        i32.const 5\n\
                        local.set $len\n\
                      else\n\
                        local.get $tmp\n\
                        i32.load\n\
                        global.get $TYPE_MAP\n\
                        i32.eq\n\
                        if\n\
                          i32.const 3\n\
                          call $alloc\n\
                          local.set $ptr\n\
                          local.get $ptr\n\
                          i32.const 109\n\
                          i32.store8\n\
                          local.get $ptr\n\
                          i32.const 1\n\
                          i32.add\n\
                          i32.const 97\n\
                          i32.store8\n\
                          local.get $ptr\n\
                          i32.const 2\n\
                          i32.add\n\
                          i32.const 112\n\
                          i32.store8\n\
                          i32.const 3\n\
                          local.set $len\n\
                        else\n\
                      local.get $tmp\n\
                      i32.load\n\
                      global.get $TYPE_F64\n\
                      i32.eq\n\
                      if\n\
                        local.get $tmp\n\
                        i32.const 8\n\
                        i32.add\n\
                        f64.load\n\
                        call $f64_to_string\n\
                        local.set $len\n\
                        local.set $ptr\n\
                      else\n\
                            i32.const 3\n\
                            call $alloc\n\
                            local.set $ptr\n\
                            local.get $ptr\n\
                            i32.const 110\n\
                            i32.store8\n\
                            local.get $ptr\n\
                            i32.const 1\n\
                            i32.add\n\
                            i32.const 105\n\
                            i32.store8\n\
                            local.get $ptr\n\
                            i32.const 2\n\
                            i32.add\n\
                            i32.const 108\n\
                            i32.store8\n\
                            i32.const 3\n\
                            local.set $len\n\
                          end\n\
                        end\n\
                      end\n\
                    end\n\
                  else\n\
                    i32.const 3\n\
                    call $alloc\n\
                    local.set $ptr\n\
                    local.get $ptr\n\
                    i32.const 110\n\
                    i32.store8\n\
                    local.get $ptr\n\
                    i32.const 1\n\
                    i32.add\n\
                    i32.const 105\n\
                    i32.store8\n\
                    local.get $ptr\n\
                    i32.const 2\n\
                    i32.add\n\
                    i32.const 108\n\
                    i32.store8\n\
                    i32.const 3\n\
                    local.set $len\n\
                  end\n\
                end\n\
              end\n\
            end\n\
            local.get $ptr\n\
            local.get $len\n\
          )\n",
    );

    out.push_str(
        "  (func $coerce_to_string (param $v i64) (result i64)\n\
            (local $ptr i32)\n\
            (local $len i32)\n\
            local.get $v\n\
            global.get $TAG_PTR\n\
            call $is_tag\n\
            if\n\
              local.get $v\n\
              call $ptr_of\n\
              i32.load\n\
              global.get $TYPE_STRING\n\
              i32.eq\n\
              if\n\
                local.get $v\n\
                return\n\
              end\n\
            end\n\
            local.get $v\n\
            call $value_to_string\n\
            local.set $len\n\
            local.set $ptr\n\
            local.get $ptr\n\
            local.get $len\n\
            call $string_new_from_data\n\
          )\n",
    );

    out.push_str(
        "  (func $write_bytes (param $fd i32) (param $ptr i32) (param $len i32)\n\
            (local $iovec i32)\n\
            (local $nwritten i32)\n\
            i32.const 8\n\
            call $alloc\n\
            local.set $iovec\n\
            local.get $iovec\n\
            local.get $ptr\n\
            i32.store\n\
            local.get $iovec\n\
            i32.const 4\n\
            i32.add\n\
            local.get $len\n\
            i32.store\n\
            i32.const 4\n\
            call $alloc\n\
            local.set $nwritten\n\
            local.get $fd\n\
            local.get $iovec\n\
            i32.const 1\n\
            local.get $nwritten\n\
            call $fd_write\n\
            drop\n\
          )\n",
    );

    out.push_str(
        "  (func $int_to_string (param $v i64) (result i32 i32)\n\
            (local $ptr i32)\n\
            (local $start i32)\n\
            (local $n i64)\n\
            (local $neg i32)\n\
            (local $digit i32)\n\
            i32.const 32\n\
            call $alloc\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 32\n\
            i32.add\n\
            local.set $start\n\
            local.get $v\n\
            local.set $n\n\
            i32.const 0\n\
            local.set $neg\n\
            local.get $n\n\
            i64.const 0\n\
            i64.lt_s\n\
            if\n\
              i32.const 1\n\
              local.set $neg\n\
              i64.const 0\n\
              local.get $n\n\
              i64.sub\n\
              local.set $n\n\
            end\n\
            local.get $n\n\
            i64.eqz\n\
            if\n\
              local.get $start\n\
              i32.const 1\n\
              i32.sub\n\
              local.set $start\n\
              local.get $start\n\
              i32.const 48\n\
              i32.store8\n\
            else\n\
              block $done\n\
                loop $loop\n\
                  local.get $n\n\
                  i64.const 0\n\
                  i64.eq\n\
                  br_if $done\n\
                  local.get $n\n\
                  i64.const 10\n\
                  i64.rem_u\n\
                  i32.wrap_i64\n\
                  local.set $digit\n\
                  local.get $n\n\
                  i64.const 10\n\
                  i64.div_u\n\
                  local.set $n\n\
                  local.get $start\n\
                  i32.const 1\n\
                  i32.sub\n\
                  local.set $start\n\
                  local.get $start\n\
                  local.get $digit\n\
                  i32.const 48\n\
                  i32.add\n\
                  i32.store8\n\
                  br $loop\n\
                end\n\
              end\n\
            end\n\
            local.get $neg\n\
            if\n\
              local.get $start\n\
              i32.const 1\n\
              i32.sub\n\
              local.set $start\n\
              local.get $start\n\
              i32.const 45\n\
              i32.store8\n\
            end\n\
            local.get $start\n\
            local.get $ptr\n\
            i32.const 32\n\
            i32.add\n\
            local.get $start\n\
            i32.sub\n\
          )\n",
    );

    out.push_str(
        "  (func $f64_to_string (param $v f64) (result i32 i32)\n\
            (local $neg i32)\n\
            (local $abs f64)\n\
            (local $int i64)\n\
            (local $frac f64)\n\
            (local $frac_i i64)\n\
            (local $int_ptr i32)\n\
            (local $int_len i32)\n\
            (local $out_ptr i32)\n\
            (local $out_len i32)\n\
            (local $i i32)\n\
            (local $digit i32)\n\
            local.get $v\n\
            f64.const 0\n\
            f64.lt\n\
            if\n\
              i32.const 1\n\
              local.set $neg\n\
              local.get $v\n\
              f64.neg\n\
              local.set $abs\n\
            else\n\
              i32.const 0\n\
              local.set $neg\n\
              local.get $v\n\
              local.set $abs\n\
            end\n\
            local.get $abs\n\
            i64.trunc_sat_f64_s\n\
            local.set $int\n\
            local.get $abs\n\
            local.get $int\n\
            f64.convert_i64_s\n\
            f64.sub\n\
            local.set $frac\n\
            local.get $frac\n\
            f64.const 1000000000\n\
            f64.mul\n\
            f64.const 0.5\n\
            f64.add\n\
            i64.trunc_sat_f64_s\n\
            local.set $frac_i\n\
            local.get $int\n\
            call $int_to_string\n\
            local.set $int_len\n\
            local.set $int_ptr\n\
            local.get $int_len\n\
            i32.const 10\n\
            i32.add\n\
            local.get $neg\n\
            i32.add\n\
            local.set $out_len\n\
            local.get $out_len\n\
            call $alloc\n\
            local.set $out_ptr\n\
            local.get $neg\n\
            if\n\
              local.get $out_ptr\n\
              i32.const 45\n\
              i32.store8\n\
            end\n\
            local.get $out_ptr\n\
            local.get $neg\n\
            i32.add\n\
            local.get $int_ptr\n\
            local.get $int_len\n\
            memory.copy\n\
            local.get $out_ptr\n\
            local.get $neg\n\
            i32.add\n\
            local.get $int_len\n\
            i32.add\n\
            i32.const 46\n\
            i32.store8\n\
            i32.const 0\n\
            local.set $i\n\
            (block $done\n\
              (loop $loop\n\
                local.get $i\n\
                i32.const 9\n\
                i32.ge_u\n\
                br_if $done\n\
                local.get $frac_i\n\
                i64.const 10\n\
                i64.rem_u\n\
                i32.wrap_i64\n\
                local.set $digit\n\
                local.get $frac_i\n\
                i64.const 10\n\
                i64.div_u\n\
                local.set $frac_i\n\
                local.get $out_ptr\n\
                local.get $neg\n\
                i32.add\n\
                local.get $int_len\n\
                i32.add\n\
                i32.const 1\n\
                i32.add\n\
                i32.const 8\n\
                local.get $i\n\
                i32.sub\n\
                i32.add\n\
                local.get $digit\n\
                i32.const 48\n\
                i32.add\n\
                i32.store8\n\
                local.get $i\n\
                i32.const 1\n\
                i32.add\n\
                local.set $i\n\
                br $loop\n\
              )\n\
            )\n\
            local.get $out_ptr\n\
            local.get $out_len\n\
          )\n",
    );

    out.push_str(
        "  (func $write_value (param $fd i32) (param $v i64) (param $newline i32) (result i64)\n\
            (local $ptr i32)\n\
            (local $len i32)\n\
            (local $tmp i32)\n\
            local.get $v\n\
            global.get $TAG_INT\n\
            call $is_tag\n\
            if\n\
              local.get $v\n\
              call $untag_int\n\
              call $int_to_string\n\
              local.set $len\n\
              local.set $ptr\n\
            else\n\
              local.get $v\n\
              global.get $TAG_BOOL\n\
              call $is_tag\n\
              if\n\
                local.get $v\n\
                call $untag_int\n\
                i64.eqz\n\
                if\n\
                  i32.const 5\n\
                  call $alloc\n\
                  local.set $ptr\n\
                  local.get $ptr\n\
                  i32.const 102\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 1\n\
                  i32.add\n\
                  i32.const 97\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 2\n\
                  i32.add\n\
                  i32.const 108\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 3\n\
                  i32.add\n\
                  i32.const 115\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 4\n\
                  i32.add\n\
                  i32.const 101\n\
                  i32.store8\n\
                  i32.const 5\n\
                  local.set $len\n\
                else\n\
                  i32.const 4\n\
                  call $alloc\n\
                  local.set $ptr\n\
                  local.get $ptr\n\
                  i32.const 116\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 1\n\
                  i32.add\n\
                  i32.const 114\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 2\n\
                  i32.add\n\
                  i32.const 117\n\
                  i32.store8\n\
                  local.get $ptr\n\
                  i32.const 3\n\
                  i32.add\n\
                  i32.const 101\n\
                  i32.store8\n\
                  i32.const 4\n\
                  local.set $len\n\
                end\n\
              else\n\
              local.get $v\n\
              global.get $TAG_NIL\n\
              call $is_tag\n\
              if\n\
                i32.const 3\n\
                call $alloc\n\
                local.set $ptr\n\
                local.get $ptr\n\
                i32.const 110\n\
                i32.store8\n\
                local.get $ptr\n\
                i32.const 1\n\
                i32.add\n\
                i32.const 105\n\
                i32.store8\n\
                local.get $ptr\n\
                i32.const 2\n\
                i32.add\n\
                i32.const 108\n\
                i32.store8\n\
                i32.const 3\n\
                local.set $len\n\
              else\n\
                local.get $v\n\
                call $value_to_string\n\
                local.set $len\n\
                local.set $ptr\n\
              end\n\
            end\n\
          end\n\
            local.get $fd\n\
            local.get $ptr\n\
            local.get $len\n\
            call $write_bytes\n\
            local.get $newline\n\
            i32.eqz\n\
            if\n\
            else\n\
              i32.const 1\n\
              call $alloc\n\
              local.set $ptr\n\
              local.get $ptr\n\
              i32.const 10\n\
              i32.store8\n\
              local.get $fd\n\
              local.get $ptr\n\
              i32.const 1\n\
              call $write_bytes\n\
            end\n\
            call $tag_nil\n\
          )\n",
    );

    out.push_str(
        "  (func $to_f64 (param $v i64) (result f64)\n\
            (local $ptr i32)\n\
            local.get $v\n\
            global.get $TAG_INT\n\
            call $is_tag\n\
            if (result f64)\n\
              local.get $v\n\
              call $untag_int\n\
              f64.convert_i64_s\n\
            else\n\
              local.get $v\n\
              global.get $TAG_PTR\n\
              call $is_tag\n\
              if (result f64)\n\
                local.get $v\n\
                call $ptr_of\n\
                local.set $ptr\n\
                local.get $ptr\n\
                i32.load\n\
                global.get $TYPE_F64\n\
                i32.eq\n\
                if (result f64)\n\
                  local.get $ptr\n\
                  i32.const 8\n\
                  i32.add\n\
                  f64.load\n\
                else\n\
                  unreachable\n\
                end\n\
              else\n\
                unreachable\n\
              end\n\
            end\n\
          )\n",
    );

    out.push_str(
        "  (func $string_eq (param $a i64) (param $b i64) (result i32)\n\
            (local $pa i32)\n\
            (local $pb i32)\n\
            (local $la i32)\n\
            (local $lb i32)\n\
            (local $i i32)\n\
            local.get $a\n\
            call $ptr_of\n\
            local.set $pa\n\
            local.get $b\n\
            call $ptr_of\n\
            local.set $pb\n\
            local.get $pa\n\
            i32.load\n\
            global.get $TYPE_STRING\n\
            i32.ne\n\
            if\n\
              i32.const 0\n\
              return\n\
            end\n\
            local.get $pb\n\
            i32.load\n\
            global.get $TYPE_STRING\n\
            i32.ne\n\
            if\n\
              i32.const 0\n\
              return\n\
            end\n\
            local.get $pa\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            local.set $la\n\
            local.get $pb\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            local.set $lb\n\
            local.get $la\n\
            local.get $lb\n\
            i32.ne\n\
            if\n\
              i32.const 0\n\
              return\n\
            end\n\
            i32.const 0\n\
            local.set $i\n\
            block $exit\n\
              loop $loop\n\
                local.get $i\n\
                local.get $la\n\
                i32.ge_u\n\
                br_if $exit\n\
                local.get $pa\n\
                i32.const 8\n\
                i32.add\n\
                local.get $i\n\
                i32.add\n\
                i32.load8_u\n\
                local.get $pb\n\
                i32.const 8\n\
                i32.add\n\
                local.get $i\n\
                i32.add\n\
                i32.load8_u\n\
                i32.ne\n\
                if\n\
                  i32.const 0\n\
                  return\n\
                end\n\
                local.get $i\n\
                i32.const 1\n\
                i32.add\n\
                local.set $i\n\
                br $loop\n\
              end\n\
            end\n\
            i32.const 1\n\
          )\n",
    );

    out.push_str(
        "  (func $eq_value (param $a i64) (param $b i64) (result i32)\n\
            local.get $a\n\
            local.get $b\n\
            i64.eq\n\
            if (result i32)\n\
              i32.const 1\n\
            else\n\
              local.get $a\n\
              global.get $TAG_PTR\n\
              call $is_tag\n\
              local.get $b\n\
              global.get $TAG_PTR\n\
              call $is_tag\n\
              i32.and\n\
              if (result i32)\n\
                local.get $a\n\
                local.get $b\n\
                call $string_eq\n\
              else\n\
                i32.const 0\n\
              end\n\
            end\n\
          )\n",
    );

    out.push_str(
        "  (func $is_truthy (param $v i64) (result i32)\n\
            (local $ptr i64)\n\
            (local $tmp i32)\n\
            local.get $v\n\
            global.get $TAG_NIL\n\
            call $is_tag\n\
            if (result i32)\n\
              i32.const 0\n\
            else\n\
              local.get $v\n\
              global.get $TAG_BOOL\n\
              call $is_tag\n\
              if (result i32)\n\
                local.get $v\n\
                call $untag_int\n\
                i64.eqz\n\
                i32.eqz\n\
              else\n\
                local.get $v\n\
                global.get $TAG_INT\n\
                call $is_tag\n\
                if (result i32)\n\
                  local.get $v\n\
                  call $untag_int\n\
                  i64.eqz\n\
                  i32.eqz\n\
                else\n\
                  local.get $v\n\
                  call $is_ref\n\
                  if (result i32)\n\
                  local.get $v\n\
                  call $ref_get\n\
                  call $is_truthy\n\
                  else\n\
                    i32.const 1\n\
                  end\n\
                end\n\
              end\n\
            end\n\
          )\n",
    );

    out.push_str(
        "  (func $add (param $a i64) (param $b i64) (result i64)\n\
            (local $sa i64)\n\
            (local $sb i64)\n\
            local.get $a\n\
            global.get $TAG_PTR\n\
            call $is_tag\n\
            if (result i32)\n\
              local.get $a\n\
              call $ptr_of\n\
              i32.load\n\
              global.get $TYPE_STRING\n\
              i32.eq\n\
            else\n\
              i32.const 0\n\
            end\n\
            local.get $b\n\
            global.get $TAG_PTR\n\
            call $is_tag\n\
            if (result i32)\n\
              local.get $b\n\
              call $ptr_of\n\
              i32.load\n\
              global.get $TYPE_STRING\n\
              i32.eq\n\
            else\n\
              i32.const 0\n\
            end\n\
            i32.or\n\
            if (result i64)\n\
              local.get $a\n\
              call $coerce_to_string\n\
              local.set $sa\n\
              local.get $b\n\
              call $coerce_to_string\n\
              local.set $sb\n\
              local.get $sa\n\
              local.get $sb\n\
              call $string_concat\n\
            else\n\
            local.get $a\n\
            global.get $TAG_INT\n\
            call $is_tag\n\
            local.get $b\n\
            global.get $TAG_INT\n\
            call $is_tag\n\
            i32.and\n\
            if (result i64)\n\
              local.get $a\n\
              call $untag_int\n\
              local.get $b\n\
              call $untag_int\n\
              i64.add\n\
              call $tag_int\n\
            else\n\
              local.get $a\n\
              call $to_f64\n\
              local.get $b\n\
              call $to_f64\n\
              f64.add\n\
              call $box_f64\n\
            end\n\
            end\n\
          )\n",
    );
    out.push_str(
        "  (func $sub (param $a i64) (param $b i64) (result i64)\n\
            local.get $a\n\
            global.get $TAG_INT\n\
            call $is_tag\n\
            local.get $b\n\
            global.get $TAG_INT\n\
            call $is_tag\n\
            i32.and\n\
            if (result i64)\n\
              local.get $a\n\
              call $untag_int\n\
              local.get $b\n\
              call $untag_int\n\
              i64.sub\n\
              call $tag_int\n\
            else\n\
              local.get $a\n\
              call $to_f64\n\
              local.get $b\n\
              call $to_f64\n\
              f64.sub\n\
              call $box_f64\n\
            end\n\
          )\n",
    );
    out.push_str(
        "  (func $mul (param $a i64) (param $b i64) (result i64)\n\
            local.get $a\n\
            global.get $TAG_INT\n\
            call $is_tag\n\
            local.get $b\n\
            global.get $TAG_INT\n\
            call $is_tag\n\
            i32.and\n\
            if (result i64)\n\
              local.get $a\n\
              call $untag_int\n\
              local.get $b\n\
              call $untag_int\n\
              i64.mul\n\
              call $tag_int\n\
            else\n\
              local.get $a\n\
              call $to_f64\n\
              local.get $b\n\
              call $to_f64\n\
              f64.mul\n\
              call $box_f64\n\
            end\n\
          )\n",
    );
    out.push_str(
        "  (func $div (param $a i64) (param $b i64) (result i64)\n\
            local.get $a\n\
            call $to_f64\n\
            local.get $b\n\
            call $to_f64\n\
            f64.div\n\
            call $box_f64\n\
          )\n",
    );

    out.push_str(
        "  (func $eq (param $a i64) (param $b i64) (result i64)\n\
            local.get $a\n\
            local.get $b\n\
            call $eq_value\n\
            call $tag_bool\n\
          )\n",
    );
    out.push_str(
        "  (func $lt (param $a i64) (param $b i64) (result i64)\n\
            local.get $a\n\
            call $to_f64\n\
            local.get $b\n\
            call $to_f64\n\
            f64.lt\n\
            call $tag_bool\n\
          )\n",
    );
    out.push_str(
        "  (func $gt (param $a i64) (param $b i64) (result i64)\n\
            local.get $a\n\
            call $to_f64\n\
            local.get $b\n\
            call $to_f64\n\
            f64.gt\n\
            call $tag_bool\n\
          )\n",
    );

    out.push_str(
        "  (func $map_new (result i64)\n\
            (local $ptr i32)\n\
            i32.const 16\n\
            call $alloc\n\
            local.set $ptr\n\
            local.get $ptr\n\
            global.get $TYPE_MAP\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            i32.const 0\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            i32.const 0\n\
            i32.store\n\
            local.get $ptr\n\
            i64.extend_i32_u\n\
            i64.const 3\n\
            i64.shl\n\
            global.get $TAG_PTR\n\
            i64.or\n\
          )\n",
    );

    out.push_str(
        "  (func $map_get (param $map i64) (param $key i64) (result i64)\n\
            (local $ptr i32)\n\
            (local $node i32)\n\
            local.get $map\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            i32.load\n\
            local.set $node\n\
            block $exit\n\
              loop $loop\n\
                local.get $node\n\
                i32.eqz\n\
                br_if $exit\n\
                local.get $node\n\
                i64.load\n\
                local.get $key\n\
                call $eq_value\n\
                if\n\
                  local.get $node\n\
                  i32.const 8\n\
                  i32.add\n\
                  i64.load\n\
                  return\n\
                end\n\
                local.get $node\n\
                i32.const 16\n\
                i32.add\n\
                i32.load\n\
                local.set $node\n\
                br $loop\n\
              end\n\
            end\n\
            call $tag_nil\n\
          )\n",
    );

    out.push_str(
        "  (func $map_set (param $map i64) (param $key i64) (param $value i64) (result i64)\n\
            (local $ptr i32)\n\
            (local $node i32)\n\
            local.get $map\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            i32.load\n\
            local.set $node\n\
            block $exit\n\
              loop $loop\n\
                local.get $node\n\
                i32.eqz\n\
                br_if $exit\n\
                local.get $node\n\
                i64.load\n\
                local.get $key\n\
                call $eq_value\n\
                if\n\
                  local.get $node\n\
                  i32.const 8\n\
                  i32.add\n\
                  local.get $value\n\
                  i64.store\n\
                  local.get $value\n\
                  return\n\
                end\n\
                local.get $node\n\
                i32.const 16\n\
                i32.add\n\
                i32.load\n\
                local.set $node\n\
                br $loop\n\
              end\n\
            end\n\
            i32.const 24\n\
            call $alloc\n\
            local.set $node\n\
            local.get $node\n\
            local.get $key\n\
            i64.store\n\
            local.get $node\n\
            i32.const 8\n\
            i32.add\n\
            local.get $value\n\
            i64.store\n\
            local.get $node\n\
            i32.const 16\n\
            i32.add\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            i32.load\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            local.get $node\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            i32.const 1\n\
            i32.add\n\
            i32.store\n\
            local.get $value\n\
          )\n",
    );

    out.push_str(
        "  (func $map_keys (param $map i64) (result i64)\n\
            (local $ptr i32)\n\
            (local $node i32)\n\
            (local $count i32)\n\
            (local $out i64)\n\
            (local $i i32)\n\
            local.get $map\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            local.set $count\n\
            local.get $count\n\
            call $array_new\n\
            local.set $out\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            i32.load\n\
            local.set $node\n\
            i32.const 0\n\
            local.set $i\n\
            block $exit\n\
              loop $loop\n\
                local.get $node\n\
                i32.eqz\n\
                br_if $exit\n\
                local.get $out\n\
                local.get $i\n\
                local.get $node\n\
                i64.load\n\
                call $array_set\n\
                drop\n\
                local.get $i\n\
                i32.const 1\n\
                i32.add\n\
                local.set $i\n\
                local.get $node\n\
                i32.const 16\n\
                i32.add\n\
                i32.load\n\
                local.set $node\n\
                br $loop\n\
              end\n\
            end\n\
            local.get $out\n\
          )\n",
    );

    out.push_str(
        "  (func $array_new (param $len i32) (result i64)\n\
            (local $ptr i32)\n\
            (local $data i32)\n\
            local.get $len\n\
            i32.const 8\n\
            i32.mul\n\
            call $alloc\n\
            local.set $data\n\
            i32.const 16\n\
            call $alloc\n\
            local.set $ptr\n\
            local.get $ptr\n\
            global.get $TYPE_ARRAY\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            local.get $len\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            local.get $data\n\
            i32.store\n\
            local.get $ptr\n\
            i64.extend_i32_u\n\
            i64.const 3\n\
            i64.shl\n\
            global.get $TAG_PTR\n\
            i64.or\n\
          )\n",
    );

    out.push_str(
        "  (func $f64array_new (param $len i32) (result i64)\n\
            (local $ptr i32)\n\
            (local $data i32)\n\
            local.get $len\n\
            i32.const 8\n\
            i32.mul\n\
            call $alloc\n\
            local.set $data\n\
            i32.const 16\n\
            call $alloc\n\
            local.set $ptr\n\
            local.get $ptr\n\
            global.get $TYPE_F64ARRAY\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            local.get $len\n\
            i32.store\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            local.get $data\n\
            i32.store\n\
            local.get $ptr\n\
            i64.extend_i32_u\n\
            i64.const 3\n\
            i64.shl\n\
            global.get $TAG_PTR\n\
            i64.or\n\
          )\n",
    );

    out.push_str(
        "  (func $array_len (param $arr i64) (result i32)\n\
            (local $ptr i32)\n\
            local.get $arr\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
          )\n",
    );

    out.push_str(
        "  (func $array_get (param $arr i64) (param $idx i32) (result i64)\n\
            (local $ptr i32)\n\
            (local $len i32)\n\
            (local $data i32)\n\
            local.get $arr\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            local.set $len\n\
            local.get $idx\n\
            local.get $len\n\
            i32.ge_u\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            i32.load\n\
            local.set $data\n\
            local.get $data\n\
            local.get $idx\n\
            i32.const 8\n\
            i32.mul\n\
            i32.add\n\
            i64.load\n\
          )\n",
    );

    out.push_str(
        "  (func $f64array_get (param $arr i64) (param $idx i32) (result f64)\n\
            (local $ptr i32)\n\
            (local $len i32)\n\
            (local $data i32)\n\
            local.get $arr\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            local.set $len\n\
            local.get $idx\n\
            local.get $len\n\
            i32.ge_u\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            i32.load\n\
            local.set $data\n\
            local.get $data\n\
            local.get $idx\n\
            i32.const 8\n\
            i32.mul\n\
            i32.add\n\
            f64.load\n\
          )\n",
    );

    out.push_str(
        "  (func $array_set (param $arr i64) (param $idx i32) (param $value i64) (result i64)\n\
            (local $ptr i32)\n\
            (local $len i32)\n\
            (local $data i32)\n\
            local.get $arr\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            local.set $len\n\
            local.get $idx\n\
            local.get $len\n\
            i32.ge_u\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            i32.load\n\
            local.set $data\n\
            local.get $data\n\
            local.get $idx\n\
            i32.const 8\n\
            i32.mul\n\
            i32.add\n\
            local.get $value\n\
            i64.store\n\
            local.get $value\n\
          )\n",
    );

    out.push_str(
        "  (func $f64array_set (param $arr i64) (param $idx i32) (param $value f64) (result f64)\n\
            (local $ptr i32)\n\
            (local $len i32)\n\
            (local $data i32)\n\
            local.get $arr\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            local.set $len\n\
            local.get $idx\n\
            local.get $len\n\
            i32.ge_u\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            i32.load\n\
            local.set $data\n\
            local.get $data\n\
            local.get $idx\n\
            i32.const 8\n\
            i32.mul\n\
            i32.add\n\
            local.get $value\n\
            f64.store\n\
            local.get $value\n\
          )\n",
    );

    out.push_str(
        "  (func $array_slice (param $arr i64) (param $start i32) (param $end i32) (result i64)\n\
            (local $len i32)\n\
            (local $s i32)\n\
            (local $e i32)\n\
            (local $out i64)\n\
            (local $i i32)\n\
            local.get $arr\n\
            call $array_len\n\
            local.set $len\n\
            local.get $start\n\
            local.get $len\n\
            i32.gt_u\n\
            if (result i32)\n\
              local.get $len\n\
            else\n\
              local.get $start\n\
            end\n\
            local.set $s\n\
            local.get $end\n\
            local.get $len\n\
            i32.gt_u\n\
            if (result i32)\n\
              local.get $len\n\
            else\n\
              local.get $end\n\
            end\n\
            local.set $e\n\
            local.get $s\n\
            local.get $e\n\
            i32.ge_u\n\
            if\n\
              i32.const 0\n\
              call $array_new\n\
              return\n\
            end\n\
            local.get $e\n\
            local.get $s\n\
            i32.sub\n\
            call $array_new\n\
            local.set $out\n\
            i32.const 0\n\
            local.set $i\n\
            block $exit\n\
              loop $loop\n\
                local.get $i\n\
                local.get $e\n\
                local.get $s\n\
                i32.sub\n\
                i32.ge_u\n\
                br_if $exit\n\
                local.get $out\n\
                local.get $i\n\
                local.get $arr\n\
                local.get $s\n\
                local.get $i\n\
                i32.add\n\
                call $array_get\n\
                call $array_set\n\
                drop\n\
                local.get $i\n\
                i32.const 1\n\
                i32.add\n\
                local.set $i\n\
                br $loop\n\
              end\n\
            end\n\
            local.get $out\n\
          )\n",
    );

    out.push_str(
        "  (func $string_slice (param $sval i64) (param $start i32) (param $end i32) (result i64)\n\
            (local $ptr i32)\n\
            (local $len i32)\n\
            (local $s i32)\n\
            (local $e i32)\n\
            (local $src i32)\n\
            local.get $sval\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            local.set $len\n\
            local.get $start\n\
            local.get $len\n\
            i32.gt_u\n\
            if (result i32)\n\
              local.get $len\n\
            else\n\
              local.get $start\n\
            end\n\
            local.set $s\n\
            local.get $end\n\
            local.get $len\n\
            i32.gt_u\n\
            if (result i32)\n\
              local.get $len\n\
            else\n\
              local.get $end\n\
            end\n\
            local.set $e\n\
            local.get $s\n\
            local.get $e\n\
            i32.ge_u\n\
            if\n\
              i32.const 0\n\
              i32.const 0\n\
              call $string_new_from_data\n\
              return\n\
            end\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            local.set $src\n\
            local.get $src\n\
            local.get $s\n\
            i32.add\n\
            local.get $e\n\
            local.get $s\n\
            i32.sub\n\
            call $string_new_from_data\n\
          )\n",
    );

    out.push_str(
        "  (func $index_get (param $target i64) (param $index i64) (result i64)\n\
            (local $ptr i32)\n\
            local.get $target\n\
            call $is_ref\n\
            if\n\
              local.get $target\n\
              call $ref_get\n\
              local.set $target\n\
            end\n\
            local.get $target\n\
            global.get $TAG_PTR\n\
            call $is_tag\n\
            i32.eqz\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $target\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.load\n\
            global.get $TYPE_MAP\n\
            i32.eq\n\
            if (result i64)\n\
              local.get $target\n\
              local.get $index\n\
              call $map_get\n\
            else\n\
              local.get $ptr\n\
              i32.load\n\
              global.get $TYPE_ARRAY\n\
              i32.eq\n\
              if (result i64)\n\
                local.get $target\n\
                local.get $index\n\
                call $untag_int\n\
                i32.wrap_i64\n\
                call $array_get\n\
              else\n\
                local.get $ptr\n\
                i32.load\n\
                global.get $TYPE_F64ARRAY\n\
                i32.eq\n\
                if (result i64)\n\
                  local.get $target\n\
                  local.get $index\n\
                  call $untag_int\n\
                  i32.wrap_i64\n\
                  call $f64array_get\n\
                  call $box_f64\n\
                else\n\
                  unreachable\n\
                end\n\
              end\n\
            end\n\
          )\n",
    );

    out.push_str(
        "  (func $index_set (param $target i64) (param $index i64) (param $value i64) (result i64)\n\
            (local $ptr i32)\n\
            local.get $target\n\
            call $is_ref\n\
            if\n\
              local.get $target\n\
              call $ref_get\n\
              local.set $target\n\
            end\n\
            local.get $target\n\
            global.get $TAG_PTR\n\
            call $is_tag\n\
            i32.eqz\n\
            if\n\
              unreachable\n\
            end\n\
            local.get $target\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.load\n\
            global.get $TYPE_MAP\n\
            i32.eq\n\
            if (result i64)\n\
              local.get $target\n\
              local.get $index\n\
              local.get $value\n\
              call $map_set\n\
            else\n\
              local.get $ptr\n\
              i32.load\n\
              global.get $TYPE_ARRAY\n\
              i32.eq\n\
              if (result i64)\n\
                local.get $target\n\
                local.get $index\n\
                call $untag_int\n\
                i32.wrap_i64\n\
                local.get $value\n\
                call $array_set\n\
              else\n\
                local.get $ptr\n\
                i32.load\n\
                global.get $TYPE_F64ARRAY\n\
                i32.eq\n\
                if (result i64)\n\
                  local.get $target\n\
                  local.get $index\n\
                  call $untag_int\n\
                  i32.wrap_i64\n\
                  local.get $value\n\
                  call $to_f64\n\
                  call $f64array_set\n\
                  call $box_f64\n\
                else\n\
                  unreachable\n\
                end\n\
              end\n\
            end\n\
          )\n",
    );
}

fn emit_wat_builtins(ctx: &mut WatContext)
{
    fn emit_builtin(ctx: &mut WatContext, name: &str, fd: i32, newline: i32)
    {
        ctx.out.push_str(&format!(
            "  (func ${name} (param $env i64) (param $args_ptr i32) (param $argc i32) (result i64)\n"
        ));
        ctx.out.push_str("    (local $val i64)\n");
        ctx.out.push_str("    local.get $argc\n");
        ctx.out.push_str("    i32.eqz\n");
        ctx.out.push_str("    if\n");
        ctx.out.push_str("      call $tag_nil\n");
        ctx.out.push_str("      local.set $val\n");
        ctx.out.push_str("    else\n");
        ctx.out.push_str("      local.get $args_ptr\n");
        ctx.out.push_str("      i64.load\n");
        ctx.out.push_str("      local.set $val\n");
        ctx.out.push_str("    end\n");
        ctx.out.push_str("    local.get $val\n");
        ctx.out.push_str("    call $is_ref\n");
        ctx.out.push_str("    if\n");
        ctx.out.push_str("      local.get $val\n");
        ctx.out.push_str("      call $ref_get\n");
        ctx.out.push_str("      local.set $val\n");
        ctx.out.push_str("    end\n");
        ctx.out
            .push_str(&format!("    i32.const {fd}\n"));
        ctx.out.push_str("    local.get $val\n");
        ctx.out
            .push_str(&format!("    i32.const {newline}\n"));
        ctx.out.push_str("    call $write_value\n");
        ctx.out.push_str("    drop\n");
        ctx.out.push_str("    call $tag_nil\n");
        ctx.out.push_str("  )\n");
    }

    emit_builtin(ctx, "builtin_print", 1, 0);
    emit_builtin(ctx, "builtin_puts", 1, 1);
    emit_builtin(ctx, "builtin_eprint", 2, 0);
    emit_builtin(ctx, "builtin_eputs", 2, 1);
    emit_builtin(ctx, "builtin_log", 2, 1);

    ctx.out.push_str(
        "  (func $builtin_float64_sqrt (param $env i64) (param $args_ptr i32) (param $argc i32) (result i64)\n\
            (local $arg i64)\n\
            local.get $argc\n\
            i32.eqz\n\
            if\n\
              call $tag_nil\n\
              return\n\
            end\n\
            local.get $args_ptr\n\
            i64.load\n\
            local.set $arg\n\
            local.get $arg\n\
            call $is_ref\n\
            if\n\
              local.get $arg\n\
              call $ref_get\n\
              local.set $arg\n\
            end\n\
            local.get $arg\n\
            call $to_f64\n\
            f64.sqrt\n\
            call $box_f64\n\
          )\n",
    );

    ctx.out.push_str(
        "  (func $builtin_int64_parse (param $env i64) (param $args_ptr i32) (param $argc i32) (result i64)\n\
            (local $arg i64)\n\
            (local $s i64)\n\
            (local $ptr i32)\n\
            (local $len i32)\n\
            (local $i i32)\n\
            (local $ch i32)\n\
            (local $sign i64)\n\
            (local $acc i64)\n\
            local.get $argc\n\
            i32.eqz\n\
            if\n\
              call $tag_nil\n\
              return\n\
            end\n\
            local.get $args_ptr\n\
            i64.load\n\
            local.set $arg\n\
            local.get $arg\n\
            call $is_ref\n\
            if\n\
              local.get $arg\n\
              call $ref_get\n\
              local.set $arg\n\
            end\n\
            local.get $arg\n\
            call $coerce_to_string\n\
            local.set $s\n\
            local.get $s\n\
            call $ptr_of\n\
            local.set $ptr\n\
            local.get $ptr\n\
            i32.const 4\n\
            i32.add\n\
            i32.load\n\
            local.set $len\n\
            local.get $ptr\n\
            i32.const 8\n\
            i32.add\n\
            local.set $ptr\n\
            i64.const 1\n\
            local.set $sign\n\
            i64.const 0\n\
            local.set $acc\n\
            i32.const 0\n\
            local.set $i\n\
            local.get $len\n\
            i32.eqz\n\
            if\n\
              local.get $acc\n\
              call $tag_int\n\
              return\n\
            end\n\
            local.get $ptr\n\
            i32.load8_u\n\
            local.set $ch\n\
            local.get $ch\n\
            i32.const 45\n\
            i32.eq\n\
            if\n\
              i64.const -1\n\
              local.set $sign\n\
              i32.const 1\n\
              local.set $i\n\
            else\n\
              local.get $ch\n\
              i32.const 43\n\
              i32.eq\n\
              if\n\
                i32.const 1\n\
                local.set $i\n\
              end\n\
            end\n\
            (loop $loop\n\
              local.get $i\n\
              local.get $len\n\
              i32.ge_u\n\
              if\n\
                local.get $sign\n\
                i64.const -1\n\
                i64.eq\n\
                if\n\
                  local.get $acc\n\
                  i64.const 0\n\
                  i64.sub\n\
                  local.set $acc\n\
                end\n\
                local.get $acc\n\
                call $tag_int\n\
                return\n\
              end\n\
              local.get $ptr\n\
              local.get $i\n\
              i32.add\n\
              i32.load8_u\n\
              local.set $ch\n\
              local.get $ch\n\
              i32.const 48\n\
              i32.lt_u\n\
              if\n\
                local.get $acc\n\
                call $tag_int\n\
                return\n\
              end\n\
              local.get $ch\n\
              i32.const 57\n\
              i32.gt_u\n\
              if\n\
                local.get $acc\n\
                call $tag_int\n\
                return\n\
              end\n\
              local.get $acc\n\
              i64.const 10\n\
              i64.mul\n\
              local.get $ch\n\
              i32.const 48\n\
              i32.sub\n\
              i64.extend_i32_u\n\
              i64.add\n\
              local.set $acc\n\
              local.get $i\n\
              i32.const 1\n\
              i32.add\n\
              local.set $i\n\
              br $loop\n\
            )\n\
            call $tag_nil\n\
          )\n",
    );
}

struct WatContext
{
    out: String,
    data_segments: Vec<(i32, Vec<u8>)>,
    data_offset: i32,
    func_names: FxHashMap<SymbolId, String>,
    global_names: FxHashMap<SymbolId, usize>,
    anon_names: FxHashMap<usize, String>,
    func_indices: FxHashMap<String, i32>,
    anon_captures: FxHashMap<usize, Vec<SymbolId>>,
    current_captures: FxHashMap<SymbolId, usize>,
    current_locals: FxHashMap<SymbolId, usize>,
    global_set: FxHashSet<SymbolId>,
    func_def_names: FxHashMap<usize, String>,
    func_def_captures: FxHashMap<usize, Vec<SymbolId>>,
    builtin_names: FxHashMap<SymbolId, String>,
    runtime_strings: Option<WatRuntimeStrings>,
    f64_arrays: FxHashSet<SymbolId>,
}

impl WatContext
{
    fn new() -> Self
    {
        Self {
            out: String::new(),
            data_segments: Vec::new(),
            data_offset: 4096,
            func_names: FxHashMap::default(),
            global_names: FxHashMap::default(),
            anon_names: FxHashMap::default(),
            func_indices: FxHashMap::default(),
            anon_captures: FxHashMap::default(),
            current_captures: FxHashMap::default(),
            current_locals: FxHashMap::default(),
            global_set: FxHashSet::default(),
            func_def_names: FxHashMap::default(),
            func_def_captures: FxHashMap::default(),
            builtin_names: FxHashMap::default(),
            runtime_strings: None,
            f64_arrays: FxHashSet::default(),
        }
    }

    fn push_data(&mut self, bytes: &[u8]) -> i32
    {
        let offset = self.data_offset;
        let len = bytes.len() as i32;
        self.data_segments.push((offset, bytes.to_vec()));
        let aligned = (len + 3) & !3;
        self.data_offset += aligned;
        offset
    }
}

fn emit_wat_std_map(ctx: &mut WatContext) -> Result<(), String>
{
    let rt = ctx
        .runtime_strings
        .ok_or_else(|| "WAT dump missing runtime strings for std".to_string())?;
    let sqrt_idx = ctx
        .func_indices
        .get("builtin_float64_sqrt")
        .cloned()
        .unwrap_or(0);
    let parse_idx = ctx
        .func_indices
        .get("builtin_int64_parse")
        .cloned()
        .unwrap_or(0);
    ctx.out.push_str("    call $map_new\n");
    ctx.out.push_str("    local.set $tmp\n");
    ctx.out.push_str("    call $map_new\n");
    ctx.out.push_str("    local.set $tmp2\n");
    ctx.out.push_str("    call $map_new\n");
    ctx.out.push_str("    local.set $tmp3\n");
    ctx.out.push_str(&format!("    i32.const {sqrt_idx}\n"));
    ctx.out.push_str("    call $tag_nil\n");
    ctx.out.push_str("    call $make_func\n");
    ctx.out.push_str("    local.set $tmp4\n");
    ctx.out.push_str("    local.get $tmp2\n");
    ctx.out.push_str(&format!("    i32.const {}\n", rt.sqrt.0));
    ctx.out.push_str(&format!("    i32.const {}\n", rt.sqrt.1));
    ctx.out.push_str("    call $string_new_from_data\n");
    ctx.out.push_str("    local.get $tmp4\n");
    ctx.out.push_str("    call $map_set\n");
    ctx.out.push_str("    drop\n");
    ctx.out.push_str(&format!("    i32.const {parse_idx}\n"));
    ctx.out.push_str("    call $tag_nil\n");
    ctx.out.push_str("    call $make_func\n");
    ctx.out.push_str("    local.set $tmp4\n");
    ctx.out.push_str("    local.get $tmp3\n");
    ctx.out.push_str(&format!("    i32.const {}\n", rt.parse.0));
    ctx.out.push_str(&format!("    i32.const {}\n", rt.parse.1));
    ctx.out.push_str("    call $string_new_from_data\n");
    ctx.out.push_str("    local.get $tmp4\n");
    ctx.out.push_str("    call $map_set\n");
    ctx.out.push_str("    drop\n");
    ctx.out.push_str("    local.get $tmp\n");
    ctx.out
        .push_str(&format!("    i32.const {}\n", rt.float64.0));
    ctx.out
        .push_str(&format!("    i32.const {}\n", rt.float64.1));
    ctx.out.push_str("    call $string_new_from_data\n");
    ctx.out.push_str("    local.get $tmp2\n");
    ctx.out.push_str("    call $map_set\n");
    ctx.out.push_str("    drop\n");
    ctx.out.push_str("    local.get $tmp\n");
    ctx.out.push_str(&format!("    i32.const {}\n", rt.int64.0));
    ctx.out.push_str(&format!("    i32.const {}\n", rt.int64.1));
    ctx.out.push_str("    call $string_new_from_data\n");
    ctx.out.push_str("    local.get $tmp3\n");
    ctx.out.push_str("    call $map_set\n");
    ctx.out.push_str("    drop\n");
    Ok(())
}

fn emit_wat_program_map(ctx: &mut WatContext) -> Result<(), String>
{
    let rt = ctx
        .runtime_strings
        .ok_or_else(|| "WAT dump missing runtime strings for program".to_string())?;
    ctx.out.push_str("    call $map_new\n");
    ctx.out.push_str("    local.set $tmp\n");
    ctx.out.push_str("    call $args_to_array\n");
    ctx.out.push_str("    local.set $tmp2\n");
    ctx.out.push_str("    local.get $tmp\n");
    ctx.out.push_str(&format!("    i32.const {}\n", rt.args.0));
    ctx.out.push_str(&format!("    i32.const {}\n", rt.args.1));
    ctx.out.push_str("    call $string_new_from_data\n");
    ctx.out.push_str("    local.get $tmp2\n");
    ctx.out.push_str("    call $map_set\n");
    ctx.out.push_str("    drop\n");
    Ok(())
}

fn emit_wat_float64_map(ctx: &mut WatContext) -> Result<(), String>
{
    let rt = ctx
        .runtime_strings
        .ok_or_else(|| "WAT dump missing runtime strings for Float64".to_string())?;
    let sqrt_idx = ctx
        .func_indices
        .get("builtin_float64_sqrt")
        .cloned()
        .unwrap_or(0);
    ctx.out.push_str("    call $map_new\n");
    ctx.out.push_str("    local.set $tmp\n");
    ctx.out.push_str(&format!("    i32.const {sqrt_idx}\n"));
    ctx.out.push_str("    call $tag_nil\n");
    ctx.out.push_str("    call $make_func\n");
    ctx.out.push_str("    local.set $tmp2\n");
    ctx.out.push_str("    local.get $tmp\n");
    ctx.out.push_str(&format!("    i32.const {}\n", rt.sqrt.0));
    ctx.out.push_str(&format!("    i32.const {}\n", rt.sqrt.1));
    ctx.out.push_str("    call $string_new_from_data\n");
    ctx.out.push_str("    local.get $tmp2\n");
    ctx.out.push_str("    call $map_set\n");
    ctx.out.push_str("    drop\n");
    ctx.out.push_str("    local.get $tmp\n");
    Ok(())
}

fn emit_wat_int64_map(ctx: &mut WatContext) -> Result<(), String>
{
    let rt = ctx
        .runtime_strings
        .ok_or_else(|| "WAT dump missing runtime strings for Int64".to_string())?;
    let parse_idx = ctx
        .func_indices
        .get("builtin_int64_parse")
        .cloned()
        .unwrap_or(0);
    ctx.out.push_str("    call $map_new\n");
    ctx.out.push_str("    local.set $tmp\n");
    ctx.out.push_str(&format!("    i32.const {parse_idx}\n"));
    ctx.out.push_str("    call $tag_nil\n");
    ctx.out.push_str("    call $make_func\n");
    ctx.out.push_str("    local.set $tmp2\n");
    ctx.out.push_str("    local.get $tmp\n");
    ctx.out.push_str(&format!("    i32.const {}\n", rt.parse.0));
    ctx.out.push_str(&format!("    i32.const {}\n", rt.parse.1));
    ctx.out.push_str("    call $string_new_from_data\n");
    ctx.out.push_str("    local.get $tmp2\n");
    ctx.out.push_str("    call $map_set\n");
    ctx.out.push_str("    drop\n");
    ctx.out.push_str("    local.get $tmp\n");
    Ok(())
}

fn expr_has_float(expr: &Expr, f64_arrays: &FxHashSet<SymbolId>) -> bool
{
    match &expr.kind
    {
        ExprKind::Float { .. } => true,
        ExprKind::Integer { .. } | ExprKind::Unsigned { .. } => false,
        ExprKind::BinaryOp { left, right, .. } =>
        {
            expr_has_float(left, f64_arrays) || expr_has_float(right, f64_arrays)
        }
        ExprKind::Index { target, .. } =>
        {
            if let ExprKind::Identifier { name, .. } = &target.kind
            {
                f64_arrays.contains(name)
            }
            else
            {
                false
            }
        }
        ExprKind::Call { .. } => true,
        _ => false,
    }
}

fn collect_f64_arrays(expr: &Expr) -> FxHashSet<SymbolId>
{
    fn array_is_numeric(expr: &Expr, set: &FxHashSet<SymbolId>) -> bool
    {
        match &expr.kind
        {
            ExprKind::Float { .. } => true,
            ExprKind::Integer { .. } | ExprKind::Unsigned { .. } => true,
            ExprKind::BinaryOp { left, right, .. } =>
            {
                array_is_numeric(left, set) || array_is_numeric(right, set)
            }
            ExprKind::Index { target, .. } =>
            {
                if let ExprKind::Identifier { name, .. } = &target.kind
                {
                    set.contains(name)
                }
                else
                {
                    false
                }
            }
            ExprKind::Call { .. } => true,
            _ => false,
        }
    }

    fn visit(expr: &Expr, set: &mut FxHashSet<SymbolId>, changed: &mut bool)
    {
        match &expr.kind
        {
            ExprKind::Assignment { name, value, .. } =>
            {
                match &value.kind
                {
                    ExprKind::ArrayGenerator { generator, .. } =>
                    {
                        if array_is_numeric(generator, set)
                            && set.insert(*name)
                        {
                            *changed = true;
                        }
                    }
                    ExprKind::Array(items) =>
                    {
                        if !items.is_empty()
                            && items.iter().all(|e| array_is_numeric(e, set))
                            && set.insert(*name)
                        {
                            *changed = true;
                        }
                    }
                    _ => {}
                }
            }
            ExprKind::IndexAssignment { target, value, .. } =>
            {
                if let ExprKind::Identifier { name, .. } = &target.kind
                {
                    if expr_has_float(value, set) && set.insert(*name)
                    {
                        *changed = true;
                    }
                }
            }
            ExprKind::BinaryOp { left, right, .. } =>
            {
                if let ExprKind::Index { target, .. } = &left.kind
                {
                    if let ExprKind::Identifier { name, .. } = &target.kind
                    {
                        if expr_has_float(right, set) && set.insert(*name)
                        {
                            *changed = true;
                        }
                    }
                }
                if let ExprKind::Index { target, .. } = &right.kind
                {
                    if let ExprKind::Identifier { name, .. } = &target.kind
                    {
                        if expr_has_float(left, set) && set.insert(*name)
                        {
                            *changed = true;
                        }
                    }
                }
            }
            _ => {}
        }

        match &expr.kind
        {
            ExprKind::BinaryOp { left, right, .. } =>
            {
                visit(left, set, changed);
                visit(right, set, changed);
            }
            ExprKind::Not(expr)
            | ExprKind::FilePublic(expr)
            | ExprKind::FunctionPublic(expr) =>
            {
                visit(expr, set, changed);
            }
            ExprKind::And { left, right }
            | ExprKind::AndBool { left, right }
            | ExprKind::Or { left, right }
            | ExprKind::OrBool { left, right } =>
            {
                visit(left, set, changed);
                visit(right, set, changed);
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } =>
            {
                visit(condition, set, changed);
                visit(then_branch, set, changed);
                if let Some(expr) = else_branch
                {
                    visit(expr, set, changed);
                }
            }
            ExprKind::While { condition, body } =>
            {
                visit(condition, set, changed);
                visit(body, set, changed);
            }
            ExprKind::Loop { count, body, .. } =>
            {
                visit(count, set, changed);
                visit(body, set, changed);
            }
            ExprKind::For { iterable, body, .. } =>
            {
                visit(iterable, set, changed);
                visit(body, set, changed);
            }
            ExprKind::Collect { count, into, body, .. } =>
            {
                visit(count, set, changed);
                if let Some(expr) = into
                {
                    visit(expr, set, changed);
                }
                visit(body, set, changed);
            }
            ExprKind::ArrayGenerator { generator, size } =>
            {
                visit(generator, set, changed);
                visit(size, set, changed);
            }
            ExprKind::Array(items) =>
            {
                for item in items
                {
                    visit(item, set, changed);
                }
            }
            ExprKind::Map(entries) =>
            {
                for (k, v) in entries
                {
                    visit(k, set, changed);
                    visit(v, set, changed);
                }
            }
            ExprKind::Block(items) =>
            {
                for item in items
                {
                    visit(item, set, changed);
                }
            }
            ExprKind::Call {
                function, args, ..
            } =>
            {
                visit(function, set, changed);
                for arg in args
                {
                    visit(arg, set, changed);
                }
            }
            ExprKind::Index { target, index } =>
            {
                visit(target, set, changed);
                visit(index, set, changed);
            }
            ExprKind::IndexAssignment { target, index, value } =>
            {
                visit(target, set, changed);
                visit(index, set, changed);
                visit(value, set, changed);
            }
            ExprKind::Assignment { value, .. } =>
            {
                visit(value, set, changed);
            }
            ExprKind::Slice { target, start, end } =>
            {
                visit(target, set, changed);
                visit(start, set, changed);
                visit(end, set, changed);
            }
            ExprKind::FormatString(parts) =>
            {
                for part in parts
                {
                    if let crate::ast::FormatPart::Expr { expr, .. } = part
                    {
                        visit(expr, set, changed);
                    }
                }
            }
            _ => {}
        }
    }

    let mut set = FxHashSet::default();
    loop
    {
        let mut changed = false;
        visit(expr, &mut set, &mut changed);
        if !changed
        {
            break;
        }
    }
    set
}

fn emit_expr_f64(ctx: &mut WatContext, expr: &Expr) -> Result<(), String>
{
    match &expr.kind
    {
        ExprKind::Float { value, .. } =>
        {
            ctx.out.push_str(&format!("    f64.const {value}\n"));
        }
        ExprKind::Integer { value, .. } =>
        {
            ctx.out.push_str(&format!("    i64.const {value}\n"));
            ctx.out.push_str("    f64.convert_i64_s\n");
        }
        ExprKind::Unsigned { value, .. } =>
        {
            ctx.out.push_str(&format!("    i64.const {value}\n"));
            ctx.out.push_str("    f64.convert_i64_s\n");
        }
        ExprKind::Identifier { slot: Some(slot), .. } =>
        {
            ctx.out.push_str(&format!("    local.get $r{slot}\n"));
            ctx.out.push_str("    local.set $tmp\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $is_ref\n");
            ctx.out.push_str("    if\n");
            ctx.out.push_str("      local.get $tmp\n");
            ctx.out.push_str("      call $ref_get\n");
            ctx.out.push_str("      local.set $tmp\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $to_f64\n");
        }
        ExprKind::Identifier { slot: None, name } =>
        {
            let idx = ctx
                .global_names
                .get(name)
                .cloned()
                .ok_or_else(|| format!("Unknown global: {}", symbol_name(*name).as_str()))?;
            ctx.out.push_str(&format!("    i32.const {idx}\n"));
            ctx.out.push_str("    call $global_get\n");
            ctx.out.push_str("    local.set $tmp\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $is_ref\n");
            ctx.out.push_str("    if\n");
            ctx.out.push_str("      local.get $tmp\n");
            ctx.out.push_str("      call $ref_get\n");
            ctx.out.push_str("      local.set $tmp\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $to_f64\n");
        }
        ExprKind::Index { target, index } =>
        {
            if let ExprKind::Identifier { name, .. } = &target.kind
            {
                if ctx.f64_arrays.contains(name)
                {
                    emit_expr_value(ctx, index)?;
                    ctx.out.push_str("    call $untag_int\n");
                    ctx.out.push_str("    i32.wrap_i64\n");
                    ctx.out.push_str("    local.set $tmp_i32\n");
                    emit_expr_value(ctx, target)?;
                    ctx.out.push_str("    local.set $tmp\n");
                    ctx.out.push_str("    local.get $tmp\n");
                    ctx.out.push_str("    local.get $tmp_i32\n");
                    ctx.out.push_str("    call $f64array_get\n");
                    return Ok(());
                }
            }
            emit_expr_value(ctx, expr)?;
            ctx.out.push_str("    call $to_f64\n");
        }
        ExprKind::BinaryOp { left, op, right } =>
        {
            emit_expr_f64(ctx, left)?;
            emit_expr_f64(ctx, right)?;
            match op
            {
                Op::Add => ctx.out.push_str("    f64.add\n"),
                Op::Subtract => ctx.out.push_str("    f64.sub\n"),
                Op::Multiply => ctx.out.push_str("    f64.mul\n"),
                Op::Divide => ctx.out.push_str("    f64.div\n"),
                _ => return Err("WAT dump does not support this f64 op".to_string()),
            }
        }
        _ =>
        {
            emit_expr_value(ctx, expr)?;
            ctx.out.push_str("    call $to_f64\n");
        }
    }
    Ok(())
}

fn emit_expr_value(ctx: &mut WatContext, expr: &Expr) -> Result<(), String>
{
    match &expr.kind
    {
        ExprKind::Integer { value, .. } =>
        {
            ctx.out.push_str(&format!("    i64.const {value}\n"));
            ctx.out.push_str("    call $tag_int\n");
        }
        ExprKind::Unsigned { value, .. } =>
        {
            ctx.out.push_str(&format!("    i64.const {value}\n"));
            ctx.out.push_str("    call $tag_int\n");
        }
        ExprKind::Float { value, .. } =>
        {
            ctx.out.push_str(&format!("    f64.const {value}\n"));
            ctx.out.push_str("    call $box_f64\n");
        }
        ExprKind::Boolean(value) =>
        {
            ctx.out.push_str(&format!(
                "    i32.const {}\n",
                if *value { 1 } else { 0 }
            ));
            ctx.out.push_str("    call $tag_bool\n");
        }
        ExprKind::Nil =>
        {
            ctx.out.push_str("    call $tag_nil\n");
        }
        ExprKind::String(text) =>
        {
            let bytes = text.as_bytes();
            let offset = ctx.push_data(bytes);
            ctx.out.push_str(&format!("    i32.const {offset}\n"));
            ctx.out
                .push_str(&format!("    i32.const {}\n", bytes.len()));
            ctx.out.push_str("    call $string_new_from_data\n");
        }
        ExprKind::Identifier { slot: Some(slot), .. } =>
        {
            ctx.out.push_str(&format!("    local.get $r{slot}\n"));
            ctx.out.push_str("    local.set $tmp\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $is_ref\n");
            ctx.out.push_str("    if (result i64)\n");
            ctx.out.push_str("      local.get $tmp\n");
            ctx.out.push_str("      call $ref_get\n");
            ctx.out.push_str("    else\n");
            ctx.out.push_str("      local.get $tmp\n");
            ctx.out.push_str("    end\n");
        }
        ExprKind::Identifier { slot: None, .. } =>
        {
            let name = match &expr.kind
            {
                ExprKind::Identifier { name, .. } => *name,
                _ => unreachable!(),
            };
            if let Some(idx) = ctx.current_captures.get(&name)
            {
                ctx.out.push_str("    local.get $env\n");
                ctx.out.push_str(&format!("    i32.const {idx}\n"));
                ctx.out.push_str("    call $env_get\n");
                ctx.out.push_str("    local.set $tmp\n");
                ctx.out.push_str("    local.get $tmp\n");
                ctx.out.push_str("    call $is_ref\n");
                ctx.out.push_str("    if (result i64)\n");
                ctx.out.push_str("      local.get $tmp\n");
                ctx.out.push_str("      call $ref_get\n");
                ctx.out.push_str("    else\n");
                ctx.out.push_str("      local.get $tmp\n");
                ctx.out.push_str("    end\n");
            }
            else if let Some(idx) = ctx.global_names.get(&name)
            {
                ctx.out.push_str(&format!("    i32.const {idx}\n"));
                ctx.out.push_str("    call $global_get\n");
                ctx.out.push_str("    local.set $tmp\n");
                ctx.out.push_str("    local.get $tmp\n");
                ctx.out.push_str("    call $is_ref\n");
                ctx.out.push_str("    if (result i64)\n");
                ctx.out.push_str("      local.get $tmp\n");
                ctx.out.push_str("      call $ref_get\n");
                ctx.out.push_str("    else\n");
                ctx.out.push_str("      local.get $tmp\n");
                ctx.out.push_str("    end\n");
            }
            else
            {
                let label = symbol_name(name);
                return Err(format!("Unknown global: {}", label.as_str()));
            }
        }
        ExprKind::Reference(name) =>
        {
            if let Some(_slot) = ctx.current_locals.get(name)
            {
                return Err(format!(
                    "WAT dump does not support references to locals yet: {}",
                    symbol_name(*name).as_str()
                ));
            }
            else if let Some(idx) = ctx.current_captures.get(name)
            {
                ctx.out.push_str("    local.get $env\n");
                ctx.out.push_str(&format!("    i32.const {idx}\n"));
                ctx.out.push_str("    call $make_ref\n");
            }
            else if let Some(global_idx) = ctx.global_names.get(name)
            {
                ctx.out.push_str("    call $tag_nil\n");
                ctx.out.push_str(&format!("    i32.const {global_idx}\n"));
                ctx.out.push_str("    call $make_ref\n");
            }
            else
            {
                return Err(format!(
                    "Unknown global: {}",
                    symbol_name(*name).as_str()
                ));
            }
        }
        ExprKind::Assignment {
            slot: Some(slot),
            value,
            ..
        } =>
        {
            emit_expr_value(ctx, value)?;
            ctx.out.push_str("    local.set $tmp2\n");
            ctx.out.push_str(&format!("    local.get $r{slot}\n"));
            ctx.out.push_str("    call $is_ref\n");
            ctx.out.push_str("    if (result i64)\n");
            ctx.out.push_str(&format!("      local.get $r{slot}\n"));
            ctx.out.push_str("      local.get $tmp2\n");
            ctx.out.push_str("      call $ref_set\n");
            ctx.out.push_str("      drop\n");
            ctx.out.push_str("      local.get $tmp2\n");
            ctx.out.push_str("    else\n");
            ctx.out.push_str("      local.get $tmp2\n");
            ctx.out.push_str(&format!("      local.tee $r{slot}\n"));
            ctx.out.push_str("    end\n");
        }
        ExprKind::Assignment { slot: None, .. } =>
        {
            let (name, value) = match &expr.kind
            {
                ExprKind::Assignment { name, value, .. } => (*name, value),
                _ => unreachable!(),
            };
            let idx = ctx
                .global_names
                .get(&name)
                .cloned()
                .ok_or_else(|| format!("Unknown global: {}", symbol_name(name).as_str()))?;
            emit_expr_value(ctx, value)?;
            ctx.out.push_str("    local.set $tmp2\n");
            ctx.out.push_str(&format!("    i32.const {idx}\n"));
            ctx.out.push_str("    call $global_get\n");
            ctx.out.push_str("    call $is_ref\n");
            ctx.out.push_str("    if (result i64)\n");
            ctx.out.push_str(&format!("      i32.const {idx}\n"));
            ctx.out.push_str("      call $global_get\n");
            ctx.out.push_str("      local.get $tmp2\n");
            ctx.out.push_str("      call $ref_set\n");
            ctx.out.push_str("      drop\n");
            ctx.out.push_str("      local.get $tmp2\n");
            ctx.out.push_str("    else\n");
            ctx.out.push_str(&format!("      i32.const {idx}\n"));
            ctx.out.push_str("      local.get $tmp2\n");
            ctx.out.push_str("      call $global_set\n");
            ctx.out.push_str("    end\n");
        }
        ExprKind::BinaryOp { left, op, right } =>
        {
            emit_expr_value(ctx, left)?;
            emit_expr_value(ctx, right)?;
            match op
            {
                Op::Add => ctx.out.push_str("    call $add\n"),
                Op::Subtract => ctx.out.push_str("    call $sub\n"),
                Op::Multiply => ctx.out.push_str("    call $mul\n"),
                Op::Divide => ctx.out.push_str("    call $div\n"),
                Op::Power =>
                {
                    return Err("WAT dump does not support pow yet".to_string());
                }
                Op::Equal => ctx.out.push_str("    call $eq\n"),
                Op::GreaterThan => ctx.out.push_str("    call $gt\n"),
                Op::LessThan => ctx.out.push_str("    call $lt\n"),
                _ => return Err("WAT dump only supports basic comparisons".to_string()),
            }
        }
        ExprKind::Not(expr) =>
        {
            emit_expr_value(ctx, expr)?;
            ctx.out.push_str("    call $is_truthy\n");
            ctx.out.push_str("    i32.eqz\n");
            ctx.out.push_str("    call $tag_bool\n");
        }
        ExprKind::And { left, right } | ExprKind::AndBool { left, right } =>
        {
            emit_expr_value(ctx, left)?;
            ctx.out.push_str("    call $is_truthy\n");
            ctx.out.push_str("    if (result i64)\n");
            emit_expr_value(ctx, right)?;
            ctx.out.push_str("      call $is_truthy\n");
            ctx.out.push_str("      call $tag_bool\n");
            ctx.out.push_str("    else\n");
            ctx.out.push_str("      i32.const 0\n");
            ctx.out.push_str("      call $tag_bool\n");
            ctx.out.push_str("    end\n");
        }
        ExprKind::Or { left, right } | ExprKind::OrBool { left, right } =>
        {
            emit_expr_value(ctx, left)?;
            ctx.out.push_str("    call $is_truthy\n");
            ctx.out.push_str("    if (result i64)\n");
            ctx.out.push_str("      i32.const 1\n");
            ctx.out.push_str("      call $tag_bool\n");
            ctx.out.push_str("    else\n");
            emit_expr_value(ctx, right)?;
            ctx.out.push_str("      call $is_truthy\n");
            ctx.out.push_str("      call $tag_bool\n");
            ctx.out.push_str("    end\n");
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            emit_expr_value(ctx, condition)?;
            ctx.out.push_str("    call $is_truthy\n");
            ctx.out.push_str("    if (result i64)\n");
            emit_expr_value(ctx, then_branch)?;
            ctx.out.push_str("    else\n");
            if let Some(else_expr) = else_branch
            {
                emit_expr_value(ctx, else_expr)?;
            }
            else
            {
                ctx.out.push_str("      call $tag_nil\n");
            }
            ctx.out.push_str("    end\n");
        }
        ExprKind::While { condition, body } =>
        {
            ctx.out.push_str("    block $while_exit\n");
            ctx.out.push_str("      loop $while_loop\n");
            emit_expr_value(ctx, condition)?;
            ctx.out.push_str("        call $is_truthy\n");
            ctx.out.push_str("        i32.eqz\n");
            ctx.out.push_str("        br_if $while_exit\n");
            emit_expr_value(ctx, body)?;
            ctx.out.push_str("        drop\n");
            ctx.out.push_str("        br $while_loop\n");
            ctx.out.push_str("      end\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    call $tag_nil\n");
        }
        ExprKind::For {
            var_slot: Some(var_slot),
            iterable,
            body,
            ..
        } =>
        {
            ctx.out.push_str("    local.get $loop_index\n");
            ctx.out.push_str("    call $args_push\n");
            ctx.out.push_str("    local.get $loop_limit\n");
            ctx.out.push_str("    call $args_push\n");
            emit_expr_value(ctx, iterable)?;
            ctx.out.push_str("    local.set $tmp\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    global.get $TAG_PTR\n");
            ctx.out.push_str("    call $is_tag\n");
            ctx.out.push_str("    i32.eqz\n");
            ctx.out.push_str("    if\n");
            ctx.out.push_str("      unreachable\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $ptr_of\n");
            ctx.out.push_str("    local.set $tmp_ptr\n");
            ctx.out.push_str("    local.get $tmp_ptr\n");
            ctx.out.push_str("    i32.load\n");
            ctx.out.push_str("    local.set $tmp_i32\n");
            ctx.out.push_str("    local.get $tmp_i32\n");
            ctx.out.push_str("    global.get $TYPE_MAP\n");
            ctx.out.push_str("    i32.eq\n");
            ctx.out.push_str("    if\n");
            ctx.out.push_str("      local.get $tmp\n");
            ctx.out.push_str("      call $map_keys\n");
            ctx.out.push_str("      local.set $tmp\n");
            ctx.out.push_str("      local.get $tmp\n");
            ctx.out.push_str("      call $ptr_of\n");
            ctx.out.push_str("      local.set $tmp_ptr\n");
            ctx.out.push_str("      local.get $tmp_ptr\n");
            ctx.out.push_str("      i32.load\n");
            ctx.out.push_str("      local.set $tmp_i32\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    local.get $tmp_i32\n");
            ctx.out.push_str("    global.get $TYPE_ARRAY\n");
            ctx.out.push_str("    i32.ne\n");
            ctx.out.push_str("    if\n");
            ctx.out.push_str("      unreachable\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    local.get $tmp_ptr\n");
            ctx.out.push_str("    i32.const 4\n");
            ctx.out.push_str("    i32.add\n");
            ctx.out.push_str("    i32.load\n");
            ctx.out.push_str("    local.set $loop_limit\n");
            ctx.out.push_str("    i32.const 0\n");
            ctx.out.push_str("    local.set $loop_index\n");
            ctx.out.push_str("    block $for_exit\n");
            ctx.out.push_str("      loop $for_loop\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        local.get $loop_limit\n");
            ctx.out.push_str("        i32.ge_u\n");
            ctx.out.push_str("        br_if $for_exit\n");
            ctx.out.push_str("        local.get $tmp\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        call $array_get\n");
            ctx.out.push_str(&format!("        local.set $r{var_slot}\n"));
            emit_expr_value(ctx, body)?;
            ctx.out.push_str("        drop\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        i32.const 1\n");
            ctx.out.push_str("        i32.add\n");
            ctx.out.push_str("        local.set $loop_index\n");
            ctx.out.push_str("        br $for_loop\n");
            ctx.out.push_str("      end\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    call $tag_nil\n");
            ctx.out.push_str("    call $args_pop\n");
            ctx.out.push_str("    local.set $loop_limit\n");
            ctx.out.push_str("    call $args_pop\n");
            ctx.out.push_str("    local.set $loop_index\n");
        }
        ExprKind::For { .. } =>
        {
            return Err("WAT dump requires a slot for for-loop variables".to_string());
        }
        ExprKind::Loop {
            count,
            var_slot: Some(var_slot),
            body,
            ..
        } =>
        {
            ctx.out.push_str("    local.get $loop_index\n");
            ctx.out.push_str("    call $args_push\n");
            ctx.out.push_str("    local.get $loop_limit\n");
            ctx.out.push_str("    call $args_push\n");
            emit_expr_value(ctx, count)?;
            ctx.out.push_str("    call $untag_int\n");
            ctx.out.push_str("    i32.wrap_i64\n");
            ctx.out.push_str("    local.set $loop_limit\n");
            ctx.out.push_str("    i32.const 0\n");
            ctx.out.push_str("    local.set $loop_index\n");
            ctx.out.push_str("    block $loop_exit\n");
            ctx.out.push_str("      loop $loop_body\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        local.get $loop_limit\n");
            ctx.out.push_str("        i32.ge_u\n");
            ctx.out.push_str("        br_if $loop_exit\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        i64.extend_i32_u\n");
            ctx.out.push_str("        call $tag_int\n");
            ctx.out.push_str(&format!("        local.set $r{var_slot}\n"));
            emit_expr_value(ctx, body)?;
            ctx.out.push_str("        drop\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        i32.const 1\n");
            ctx.out.push_str("        i32.add\n");
            ctx.out.push_str("        local.set $loop_index\n");
            ctx.out.push_str("        br $loop_body\n");
            ctx.out.push_str("      end\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    call $tag_nil\n");
            ctx.out.push_str("    call $args_pop\n");
            ctx.out.push_str("    local.set $loop_limit\n");
            ctx.out.push_str("    call $args_pop\n");
            ctx.out.push_str("    local.set $loop_index\n");
        }
        ExprKind::Loop { .. } =>
        {
            return Err("WAT dump requires a slot for loop variables".to_string());
        }
        ExprKind::Collect {
            count,
            into: None,
            var_slot: Some(var_slot),
            body,
            ..
        } =>
        {
            ctx.out.push_str("    local.get $loop_index\n");
            ctx.out.push_str("    call $args_push\n");
            ctx.out.push_str("    local.get $loop_limit\n");
            ctx.out.push_str("    call $args_push\n");
            emit_expr_value(ctx, count)?;
            ctx.out.push_str("    call $untag_int\n");
            ctx.out.push_str("    i32.wrap_i64\n");
            ctx.out.push_str("    local.set $loop_limit\n");
            ctx.out.push_str("    local.get $loop_limit\n");
            ctx.out.push_str("    call $array_new\n");
            ctx.out.push_str("    local.set $tmp\n");
            ctx.out.push_str("    i32.const 0\n");
            ctx.out.push_str("    local.set $loop_index\n");
            ctx.out.push_str("    block $collect_exit\n");
            ctx.out.push_str("      loop $collect_loop\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        local.get $loop_limit\n");
            ctx.out.push_str("        i32.ge_u\n");
            ctx.out.push_str("        br_if $collect_exit\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        i64.extend_i32_u\n");
            ctx.out.push_str("        call $tag_int\n");
            ctx.out.push_str(&format!("        local.set $r{var_slot}\n"));
            emit_expr_value(ctx, body)?;
            ctx.out.push_str("        local.set $tmp2\n");
            ctx.out.push_str("        local.get $tmp\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        local.get $tmp2\n");
            ctx.out.push_str("        call $array_set\n");
            ctx.out.push_str("        drop\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        i32.const 1\n");
            ctx.out.push_str("        i32.add\n");
            ctx.out.push_str("        local.set $loop_index\n");
            ctx.out.push_str("        br $collect_loop\n");
            ctx.out.push_str("      end\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $args_pop\n");
            ctx.out.push_str("    local.set $loop_limit\n");
            ctx.out.push_str("    call $args_pop\n");
            ctx.out.push_str("    local.set $loop_index\n");
        }
        ExprKind::Collect {
            count,
            into: Some(into),
            var_slot: Some(var_slot),
            body,
            ..
        } =>
        {
            ctx.out.push_str("    local.get $loop_index\n");
            ctx.out.push_str("    call $args_push\n");
            ctx.out.push_str("    local.get $loop_limit\n");
            ctx.out.push_str("    call $args_push\n");
            emit_expr_value(ctx, count)?;
            ctx.out.push_str("    call $untag_int\n");
            ctx.out.push_str("    i32.wrap_i64\n");
            ctx.out.push_str("    local.set $loop_limit\n");
            emit_expr_value(ctx, into)?;
            ctx.out.push_str("    local.set $tmp\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    global.get $TAG_PTR\n");
            ctx.out.push_str("    call $is_tag\n");
            ctx.out.push_str("    i32.eqz\n");
            ctx.out.push_str("    if\n");
            ctx.out.push_str("      unreachable\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $ptr_of\n");
            ctx.out.push_str("    i32.load\n");
            ctx.out.push_str("    global.get $TYPE_ARRAY\n");
            ctx.out.push_str("    i32.ne\n");
            ctx.out.push_str("    if\n");
            ctx.out.push_str("      unreachable\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $array_len\n");
            ctx.out.push_str("    local.get $loop_limit\n");
            ctx.out.push_str("    i32.ne\n");
            ctx.out.push_str("    if\n");
            ctx.out.push_str("      unreachable\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    i32.const 0\n");
            ctx.out.push_str("    local.set $loop_index\n");
            ctx.out.push_str("    block $collect_into_exit\n");
            ctx.out.push_str("      loop $collect_into_loop\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        local.get $loop_limit\n");
            ctx.out.push_str("        i32.ge_u\n");
            ctx.out.push_str("        br_if $collect_into_exit\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        i64.extend_i32_u\n");
            ctx.out.push_str("        call $tag_int\n");
            ctx.out.push_str(&format!("        local.set $r{var_slot}\n"));
            emit_expr_value(ctx, body)?;
            ctx.out.push_str("        local.set $tmp2\n");
            ctx.out.push_str("        local.get $tmp\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        local.get $tmp2\n");
            ctx.out.push_str("        call $array_set\n");
            ctx.out.push_str("        drop\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        i32.const 1\n");
            ctx.out.push_str("        i32.add\n");
            ctx.out.push_str("        local.set $loop_index\n");
            ctx.out.push_str("        br $collect_into_loop\n");
            ctx.out.push_str("      end\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $args_pop\n");
            ctx.out.push_str("    local.set $loop_limit\n");
            ctx.out.push_str("    call $args_pop\n");
            ctx.out.push_str("    local.set $loop_index\n");
        }
        ExprKind::Collect { .. } =>
        {
            return Err("WAT dump only supports collect into new arrays".to_string());
        }
        ExprKind::ArrayGenerator { generator, size } =>
        {
            let gen_is_numeric = matches!(
                generator.kind,
                ExprKind::Float { .. } | ExprKind::Integer { .. } | ExprKind::Unsigned { .. }
            ) || expr_has_float(generator, &ctx.f64_arrays);
            ctx.out.push_str("    local.get $loop_index\n");
            ctx.out.push_str("    call $args_push\n");
            ctx.out.push_str("    local.get $loop_limit\n");
            ctx.out.push_str("    call $args_push\n");
            emit_expr_value(ctx, size)?;
            ctx.out.push_str("    call $untag_int\n");
            ctx.out.push_str("    i32.wrap_i64\n");
            ctx.out.push_str("    local.set $loop_limit\n");
            if !gen_is_numeric
            {
                emit_expr_value(ctx, generator)?;
                ctx.out.push_str("    local.set $tmp3\n");
            }
            ctx.out.push_str("    local.get $loop_limit\n");
            if gen_is_numeric
            {
                ctx.out.push_str("    call $f64array_new\n");
            }
            else
            {
                ctx.out.push_str("    call $array_new\n");
            }
            ctx.out.push_str("    local.set $tmp\n");
            ctx.out.push_str("    i32.const 0\n");
            ctx.out.push_str("    local.set $loop_index\n");
            ctx.out.push_str("    block $gen_exit\n");
            ctx.out.push_str("      loop $gen_loop\n");
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        local.get $loop_limit\n");
            ctx.out.push_str("        i32.ge_u\n");
            ctx.out.push_str("        br_if $gen_exit\n");
            if gen_is_numeric
            {
                ctx.out.push_str("        local.get $tmp\n");
                ctx.out.push_str("        local.get $loop_index\n");
                emit_expr_f64(ctx, generator)?;
                ctx.out.push_str("        call $f64array_set\n");
                ctx.out.push_str("        drop\n");
            }
            else
            {
                ctx.out.push_str("        local.get $tmp3\n");
                ctx.out.push_str("        call $is_func\n");
                ctx.out.push_str("        if\n");
                ctx.out.push_str("          i32.const 8\n");
                ctx.out.push_str("          call $alloc\n");
                ctx.out.push_str("          local.set $tmp_ptr2\n");
                ctx.out.push_str("          local.get $tmp_ptr2\n");
                ctx.out.push_str("          local.get $tmp_ptr\n");
                ctx.out.push_str("          i64.extend_i32_u\n");
                ctx.out.push_str("          call $tag_int\n");
                ctx.out.push_str("          i64.store\n");
                ctx.out.push_str("          local.get $tmp3\n");
                ctx.out.push_str("          local.get $tmp_ptr2\n");
                ctx.out.push_str("          i32.const 1\n");
                ctx.out.push_str("          call $call_func\n");
                ctx.out.push_str("          local.set $tmp2\n");
                ctx.out.push_str("        end\n");
                ctx.out.push_str("        local.get $tmp3\n");
                ctx.out.push_str("        call $is_func\n");
                ctx.out.push_str("        i32.eqz\n");
                ctx.out.push_str("        if\n");
                ctx.out.push_str("          local.get $tmp3\n");
                ctx.out.push_str("          local.set $tmp2\n");
                ctx.out.push_str("        end\n");
                ctx.out.push_str("        local.get $tmp\n");
                ctx.out.push_str("        local.get $loop_index\n");
                ctx.out.push_str("        local.get $tmp2\n");
                ctx.out.push_str("        call $array_set\n");
                ctx.out.push_str("        drop\n");
            }
            ctx.out.push_str("        local.get $loop_index\n");
            ctx.out.push_str("        i32.const 1\n");
            ctx.out.push_str("        i32.add\n");
            ctx.out.push_str("        local.set $loop_index\n");
            ctx.out.push_str("        br $gen_loop\n");
            ctx.out.push_str("      end\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $args_pop\n");
            ctx.out.push_str("    local.set $loop_limit\n");
            ctx.out.push_str("    call $args_pop\n");
            ctx.out.push_str("    local.set $loop_index\n");
        }
        ExprKind::Block(items) =>
        {
            if items.is_empty()
            {
                ctx.out.push_str("    call $tag_nil\n");
            }
            else
            {
                for expr in items.iter().take(items.len() - 1)
                {
                    emit_expr_value(ctx, expr)?;
                    ctx.out.push_str("    drop\n");
                }
                emit_expr_value(ctx, items.last().unwrap())?;
            }
        }
        ExprKind::Map(entries) =>
        {
            ctx.out.push_str("    call $map_new\n");
            ctx.out.push_str("    local.set $tmp\n");
            for (key, value) in entries
            {
                ctx.out.push_str("    local.get $tmp\n");
                emit_expr_value(ctx, key)?;
                emit_expr_value(ctx, value)?;
                ctx.out.push_str("    call $map_set\n");
                ctx.out.push_str("    drop\n");
            }
            ctx.out.push_str("    local.get $tmp\n");
        }
        ExprKind::Array(elements) =>
        {
            let is_numeric = !elements.is_empty()
                && elements.iter().all(|e| {
                    matches!(
                        e.kind,
                        ExprKind::Float { .. }
                            | ExprKind::Integer { .. }
                            | ExprKind::Unsigned { .. }
                    ) || expr_has_float(e, &ctx.f64_arrays)
                });
            ctx.out.push_str(&format!(
                "    i32.const {}\n",
                elements.len()
            ));
            if is_numeric
            {
                ctx.out.push_str("    call $f64array_new\n");
            }
            else
            {
                ctx.out.push_str("    call $array_new\n");
            }
            ctx.out.push_str("    local.set $tmp\n");
            for (idx, value) in elements.iter().enumerate()
            {
                if is_numeric
                {
                    ctx.out.push_str("    local.get $tmp\n");
                    ctx.out.push_str(&format!("    i32.const {idx}\n"));
                    emit_expr_f64(ctx, value)?;
                    ctx.out.push_str("    call $f64array_set\n");
                    ctx.out.push_str("    drop\n");
                }
                else
                {
                    ctx.out.push_str("    local.get $tmp\n");
                    ctx.out.push_str(&format!("    i64.const {idx}\n"));
                    ctx.out.push_str("    call $tag_int\n");
                    emit_expr_value(ctx, value)?;
                    ctx.out.push_str("    call $index_set\n");
                    ctx.out.push_str("    drop\n");
                }
            }
            ctx.out.push_str("    local.get $tmp\n");
        }
        ExprKind::Use(_) =>
        {
            ctx.out.push_str("    call $tag_nil\n");
        }
        ExprKind::FormatString(parts) =>
        {
            ctx.out.push_str("    i32.const 0\n");
            ctx.out.push_str("    i32.const 0\n");
            ctx.out.push_str("    call $string_new_from_data\n");
            ctx.out.push_str("    local.set $tmp\n");
            for part in parts
            {
                match part
                {
                    crate::ast::FormatPart::Literal(text) =>
                    {
                        let bytes = text.as_bytes();
                        let offset = ctx.push_data(bytes);
                        ctx.out.push_str(&format!("    i32.const {offset}\n"));
                        ctx.out
                            .push_str(&format!("    i32.const {}\n", bytes.len()));
                        ctx.out.push_str("    call $string_new_from_data\n");
                        ctx.out.push_str("    local.set $tmp2\n");
                    }
                    crate::ast::FormatPart::Expr { expr, .. } =>
                    {
                        emit_expr_value(ctx, expr)?;
                        ctx.out.push_str("    call $coerce_to_string\n");
                        ctx.out.push_str("    local.set $tmp2\n");
                    }
                }
                ctx.out.push_str("    local.get $tmp\n");
                ctx.out.push_str("    local.get $tmp2\n");
                ctx.out.push_str("    call $string_concat\n");
                ctx.out.push_str("    local.set $tmp\n");
            }
            ctx.out.push_str("    local.get $tmp\n");
        }
        ExprKind::FilePublic(expr) | ExprKind::FunctionPublic(expr) =>
        {
            emit_expr_value(ctx, expr)?;
        }
        ExprKind::Slice { target, start, end } =>
        {
            emit_expr_value(ctx, target)?;
            ctx.out.push_str("    local.set $tmp\n");
            emit_expr_value(ctx, start)?;
            ctx.out.push_str("    call $untag_int\n");
            ctx.out.push_str("    i32.wrap_i64\n");
            ctx.out.push_str("    local.set $tmp_ptr\n");
            emit_expr_value(ctx, end)?;
            ctx.out.push_str("    call $untag_int\n");
            ctx.out.push_str("    i32.wrap_i64\n");
            ctx.out.push_str("    local.set $tmp_ptr2\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    global.get $TAG_PTR\n");
            ctx.out.push_str("    call $is_tag\n");
            ctx.out.push_str("    i32.eqz\n");
            ctx.out.push_str("    if\n");
            ctx.out.push_str("      unreachable\n");
            ctx.out.push_str("    end\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $ptr_of\n");
            ctx.out.push_str("    local.set $tmp_i32\n");
            ctx.out.push_str("    local.get $tmp_i32\n");
            ctx.out.push_str("    i32.load\n");
            ctx.out.push_str("    global.get $TYPE_STRING\n");
            ctx.out.push_str("    i32.eq\n");
            ctx.out.push_str("    if (result i64)\n");
            ctx.out.push_str("      local.get $tmp\n");
            ctx.out.push_str("      local.get $tmp_ptr\n");
            ctx.out.push_str("      local.get $tmp_ptr2\n");
            ctx.out.push_str("      call $string_slice\n");
            ctx.out.push_str("    else\n");
            ctx.out.push_str("      local.get $tmp_i32\n");
            ctx.out.push_str("      i32.load\n");
            ctx.out.push_str("      global.get $TYPE_ARRAY\n");
            ctx.out.push_str("      i32.ne\n");
            ctx.out.push_str("      if\n");
            ctx.out.push_str("        unreachable\n");
            ctx.out.push_str("      end\n");
            ctx.out.push_str("      local.get $tmp\n");
            ctx.out.push_str("      local.get $tmp_ptr\n");
            ctx.out.push_str("      local.get $tmp_ptr2\n");
            ctx.out.push_str("      call $array_slice\n");
            ctx.out.push_str("    end\n");
        }
        ExprKind::Index { target, index } =>
        {
            if let ExprKind::Identifier { slot: None, name } = &target.kind
            {
                if let ExprKind::String(text) = &index.kind
                {
                    if *name == intern::intern_symbol("program") && text.as_str() == "args"
                    {
                        ctx.out.push_str("    call $args_to_array\n");
                        return Ok(());
                    }
                    if *name == intern::intern_symbol("std")
                    {
                        if text.as_str() == "Float64"
                        {
                            emit_wat_float64_map(ctx)?;
                            return Ok(());
                        }
                        if text.as_str() == "Int64"
                        {
                            emit_wat_int64_map(ctx)?;
                            return Ok(());
                        }
                    }
                }
            }
            emit_expr_value(ctx, target)?;
            if let ExprKind::Identifier { slot: None, name } = &index.kind
            {
                let text = symbol_name(*name);
                let bytes = text.as_bytes();
                let offset = ctx.push_data(bytes);
                ctx.out.push_str(&format!("    i32.const {offset}\n"));
                ctx.out
                    .push_str(&format!("    i32.const {}\n", bytes.len()));
                ctx.out.push_str("    call $string_new_from_data\n");
            }
            else
            {
                emit_expr_value(ctx, index)?;
            }
            ctx.out.push_str("    call $index_get\n");
        }
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            if let ExprKind::Identifier { name, .. } = &target.kind
            {
                if ctx.f64_arrays.contains(name)
                {
                    emit_expr_value(ctx, index)?;
                    ctx.out.push_str("    call $untag_int\n");
                    ctx.out.push_str("    i32.wrap_i64\n");
                    ctx.out.push_str("    local.set $tmp_i32\n");
                    emit_expr_value(ctx, target)?;
                    ctx.out.push_str("    local.set $tmp\n");
                    ctx.out.push_str("    local.get $tmp\n");
                    ctx.out.push_str("    local.get $tmp_i32\n");
                    emit_expr_f64(ctx, value)?;
                    ctx.out.push_str("    call $f64array_set\n");
                    ctx.out.push_str("    call $box_f64\n");
                    return Ok(());
                }
            }
            emit_expr_value(ctx, target)?;
            emit_expr_value(ctx, index)?;
            emit_expr_value(ctx, value)?;
            ctx.out.push_str("    call $index_set\n");
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
                return Err("WAT dump does not support call blocks yet".to_string());
            }
            if let ExprKind::Index { target, index } = &function.kind
            {
                let target_name = if let ExprKind::Identifier { name, .. } = &target.kind
                {
                    Some(*name)
                }
                else
                {
                    None
                };
                let index_name = match &index.kind
                {
                    ExprKind::Identifier { slot: None, name } =>
                    {
                        Some(symbol_name(*name).as_str().to_string())
                    }
                    ExprKind::String(text) => Some(text.as_str().to_string()),
                    _ => None,
                };
                if let (Some(target_name), Some(index_name)) = (target_name, index_name)
                {
                    let direct_builtin = if target_name == intern::intern_symbol("Float64")
                        && index_name.as_str() == "sqrt"
                    {
                        Some("builtin_float64_sqrt")
                    }
                    else if target_name == intern::intern_symbol("Int64")
                        && index_name.as_str() == "parse"
                    {
                        Some("builtin_int64_parse")
                    }
                    else
                    {
                        None
                    };
                    if let Some(builtin) = direct_builtin
                    {
                        ctx.out.push_str("    local.get $args_base\n");
                        ctx.out.push_str("    call $args_push\n");
                        ctx.out
                            .push_str(&format!("    i32.const {}\n", args.len() * 8));
                        ctx.out.push_str("    call $call_args_alloc\n");
                        ctx.out.push_str("    local.set $args_base\n");
                        for (idx, arg) in args.iter().enumerate()
                        {
                            emit_expr_value(ctx, arg)?;
                            ctx.out.push_str("    local.set $tmp2\n");
                            ctx.out.push_str("    local.get $args_base\n");
                            ctx.out
                                .push_str(&format!("    i32.const {}\n", idx * 8));
                            ctx.out.push_str("    i32.add\n");
                            ctx.out.push_str("    local.get $tmp2\n");
                            ctx.out.push_str("    i64.store\n");
                        }
                        ctx.out.push_str("    call $tag_nil\n");
                        ctx.out.push_str("    local.get $args_base\n");
                        ctx.out
                            .push_str(&format!("    i32.const {}\n", args.len()));
                        ctx.out
                            .push_str(&format!("    call ${builtin}\n"));
                        ctx.out.push_str("    local.set $tmp2\n");
                        ctx.out.push_str("    call $args_pop\n");
                        ctx.out.push_str("    local.set $args_base\n");
                        ctx.out.push_str("    local.get $tmp2\n");
                        return Ok(());
                    }
                }
            }
            ctx.out.push_str("    local.get $args_base\n");
            ctx.out.push_str("    call $args_push\n");
            ctx.out
                .push_str(&format!("    i32.const {}\n", args.len() * 8));
            ctx.out.push_str("    call $call_args_alloc\n");
            ctx.out.push_str("    local.set $args_base\n");
            for (idx, arg) in args.iter().enumerate()
            {
                emit_expr_value(ctx, arg)?;
                ctx.out.push_str("    local.set $tmp2\n");
                ctx.out.push_str("    local.get $args_base\n");
                ctx.out
                    .push_str(&format!("    i32.const {}\n", idx * 8));
                ctx.out.push_str("    i32.add\n");
                ctx.out.push_str("    local.get $tmp2\n");
                ctx.out.push_str("    i64.store\n");
            }
            emit_expr_value(ctx, function)?;
            ctx.out.push_str("    local.set $tmp\n");
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    local.get $args_base\n");
            ctx.out
                .push_str(&format!("    i32.const {}\n", args.len()));
            ctx.out.push_str("    call $call_func\n");
            ctx.out.push_str("    local.set $tmp2\n");
            ctx.out.push_str("    call $args_pop\n");
            ctx.out.push_str("    local.set $args_base\n");
            ctx.out.push_str("    local.get $tmp2\n");
        }
        ExprKind::FunctionDef { name, .. } =>
        {
            let key = expr as *const Expr as usize;
            let internal = ctx
                .func_def_names
                .get(&key)
                .cloned()
                .ok_or_else(|| "Unknown function definition".to_string())?;
            let idx = ctx
                .func_indices
                .get(&internal)
                .cloned()
                .ok_or_else(|| "Unknown function index".to_string())?;
            ctx.out.push_str(&format!("    i32.const {idx}\n"));
            let captures = ctx
                .func_def_captures
                .get(&key)
                .cloned()
                .unwrap_or_default();
            if captures.is_empty()
            {
                ctx.out.push_str("    call $tag_nil\n");
            }
            else
            {
                ctx.out
                    .push_str(&format!("    i32.const {}\n", captures.len()));
                ctx.out.push_str("    call $env_new\n");
                ctx.out.push_str("    local.set $tmp\n");
                for (cap_idx, sym) in captures.iter().enumerate()
                {
                    ctx.out.push_str("    local.get $tmp\n");
                    ctx.out.push_str(&format!("    i32.const {cap_idx}\n"));
                    if let Some(slot) = ctx.current_locals.get(sym)
                    {
                        ctx.out.push_str(&format!("    local.get $r{slot}\n"));
                    }
                    else if let Some(env_idx) = ctx.current_captures.get(sym)
                    {
                        ctx.out.push_str("    local.get $env\n");
                        ctx.out.push_str(&format!("    i32.const {env_idx}\n"));
                        ctx.out.push_str("    call $env_get\n");
                    }
                    else if let Some(global_idx) = ctx.global_names.get(sym)
                    {
                        ctx.out.push_str(&format!("    i32.const {global_idx}\n"));
                        ctx.out.push_str("    call $global_get\n");
                    }
                    else
                    {
                        return Err("Unknown capture".to_string());
                    }
                    ctx.out.push_str("    call $env_set\n");
                    ctx.out.push_str("    drop\n");
                }
                ctx.out.push_str("    local.get $tmp\n");
            }
            ctx.out.push_str("    call $make_func\n");
            if let Some(slot) = ctx.current_locals.get(name)
            {
                ctx.out.push_str(&format!("    local.tee $r{slot}\n"));
            }
            else if let Some(global_idx) = ctx.global_names.get(name)
            {
                ctx.out.push_str("    local.set $tmp\n");
                ctx.out.push_str(&format!("    i32.const {global_idx}\n"));
                ctx.out.push_str("    local.get $tmp\n");
                ctx.out.push_str("    call $global_set\n");
            }
            else
            {
                return Err("Unknown function binding".to_string());
            }
        }
        ExprKind::AnonymousFunction { .. } =>
        {
            let key = expr as *const Expr as usize;
            let internal = ctx
                .anon_names
                .get(&key)
                .cloned()
                .ok_or_else(|| "Unknown anonymous function".to_string())?;
            let idx = ctx
                .func_indices
                .get(&internal)
                .cloned()
                .ok_or_else(|| "Unknown function index".to_string())?;
            ctx.out.push_str(&format!("    i32.const {idx}\n"));
            let captures = ctx
                .anon_captures
                .get(&key)
                .cloned()
                .unwrap_or_default();
            if captures.is_empty()
            {
                ctx.out.push_str("    call $tag_nil\n");
            }
            else
            {
                ctx.out
                    .push_str(&format!("    i32.const {}\n", captures.len()));
                ctx.out.push_str("    call $env_new\n");
                ctx.out.push_str("    local.set $tmp\n");
                for (cap_idx, sym) in captures.iter().enumerate()
                {
                    ctx.out.push_str("    local.get $tmp\n");
                    ctx.out.push_str(&format!("    i32.const {cap_idx}\n"));
                    if let Some(slot) = ctx.current_locals.get(sym)
                    {
                        ctx.out.push_str(&format!("    local.get $r{slot}\n"));
                    }
                    else if let Some(env_idx) = ctx.current_captures.get(sym)
                    {
                        ctx.out.push_str("    local.get $env\n");
                        ctx.out.push_str(&format!("    i32.const {env_idx}\n"));
                        ctx.out.push_str("    call $env_get\n");
                    }
                    else if let Some(global_idx) = ctx.global_names.get(sym)
                    {
                        ctx.out.push_str(&format!("    i32.const {global_idx}\n"));
                        ctx.out.push_str("    call $global_get\n");
                    }
                    else
                    {
                        return Err("Unknown capture".to_string());
                    }
                    ctx.out.push_str("    call $env_set\n");
                    ctx.out.push_str("    drop\n");
                }
                ctx.out.push_str("    local.get $tmp\n");
            }
            ctx.out.push_str("    call $make_func\n");
        }
        ExprKind::Return(value) =>
        {
            if let Some(expr) = value
            {
                emit_expr_value(ctx, expr)?;
            }
            else
            {
                ctx.out.push_str("    call $tag_nil\n");
            }
            ctx.out.push_str("    return\n");
        }
        _ =>
        {
            return Err(format!(
                "WAT dump does not support this expression yet: {:?}",
                expr.kind
            ));
        }
    }
    Ok(())
}

fn local_count_for_expr(expr: &Expr) -> usize
{
    let mut max_slot = None;
    fn visit(expr: &Expr, max_slot: &mut Option<usize>)
    {
        match &expr.kind
        {
            ExprKind::Identifier { slot: Some(slot), .. }
            | ExprKind::Assignment { slot: Some(slot), .. } =>
            {
                *max_slot = Some(max_slot.map_or(*slot, |m| m.max(*slot)));
            }
            ExprKind::BinaryOp { left, right, .. } =>
            {
                visit(left, max_slot);
                visit(right, max_slot);
            }
            ExprKind::Not(expr) | ExprKind::Clone(expr) | ExprKind::EnvFreeze(expr) =>
            {
                visit(expr, max_slot);
            }
            ExprKind::And { left, right }
            | ExprKind::AndBool { left, right }
            | ExprKind::Or { left, right }
            | ExprKind::OrBool { left, right } =>
            {
                visit(left, max_slot);
                visit(right, max_slot);
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } =>
            {
                visit(condition, max_slot);
                visit(then_branch, max_slot);
                if let Some(expr) = else_branch
                {
                    visit(expr, max_slot);
                }
            }
            ExprKind::While { condition, body } =>
            {
                visit(condition, max_slot);
                visit(body, max_slot);
            }
            ExprKind::Block(items) =>
            {
                for item in items
                {
                    visit(item, max_slot);
                }
            }
            ExprKind::IndexAssignment { target, index, value } =>
            {
                visit(target, max_slot);
                visit(index, max_slot);
                visit(value, max_slot);
            }
            ExprKind::Return(value) =>
            {
                if let Some(expr) = value
                {
                    visit(expr, max_slot);
                }
            }
            _ => {}
        }
    }
    visit(expr, &mut max_slot);
    max_slot.map(|s| s + 1).unwrap_or(0)
}

fn collect_global_symbols(expr: &Expr, out: &mut Vec<SymbolId>, in_function: bool)
{
    fn add(sym: SymbolId, out: &mut Vec<SymbolId>)
    {
        if !out.contains(&sym)
        {
            out.push(sym);
        }
    }
    match &expr.kind
    {
        ExprKind::Identifier { name, slot: None } =>
        {
            if !in_function
            {
                add(*name, out);
            }
        }
        ExprKind::Reference(name) =>
        {
            if !in_function
            {
                add(*name, out);
            }
        }
        ExprKind::Assignment { name, slot: None, value } =>
        {
            if !in_function
            {
                add(*name, out);
            }
            collect_global_symbols(value, out, in_function);
        }
        ExprKind::IndexAssignment { target, index, value } =>
        {
            collect_global_symbols(target, out, in_function);
            collect_global_symbols(index, out, in_function);
            collect_global_symbols(value, out, in_function);
        }
        ExprKind::BinaryOp { left, right, .. } =>
        {
            collect_global_symbols(left, out, in_function);
            collect_global_symbols(right, out, in_function);
        }
        ExprKind::Not(expr) | ExprKind::Clone(expr) | ExprKind::EnvFreeze(expr) =>
        {
            collect_global_symbols(expr, out, in_function);
        }
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } =>
        {
            collect_global_symbols(left, out, in_function);
            collect_global_symbols(right, out, in_function);
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            collect_global_symbols(condition, out, in_function);
            collect_global_symbols(then_branch, out, in_function);
            if let Some(expr) = else_branch
            {
                collect_global_symbols(expr, out, in_function);
            }
        }
        ExprKind::While { condition, body } =>
        {
            collect_global_symbols(condition, out, in_function);
            collect_global_symbols(body, out, in_function);
        }
        ExprKind::Block(items) =>
        {
            for item in items
            {
                collect_global_symbols(item, out, in_function);
            }
        }
        ExprKind::Call { function, args, .. } =>
        {
            collect_global_symbols(function, out, in_function);
            for arg in args
            {
                collect_global_symbols(arg, out, in_function);
            }
        }
        ExprKind::Array(items) =>
        {
            for item in items
            {
                collect_global_symbols(item, out, in_function);
            }
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                collect_global_symbols(k, out, in_function);
                collect_global_symbols(v, out, in_function);
            }
        }
        ExprKind::Index { target, index } =>
        {
            collect_global_symbols(target, out, in_function);
            collect_global_symbols(index, out, in_function);
        }
        ExprKind::Return(value) =>
        {
            if let Some(expr) = value
            {
                collect_global_symbols(expr, out, in_function);
            }
        }
        ExprKind::FunctionDef { name, .. } =>
        {
            add(*name, out);
        }
        ExprKind::FilePublic(expr) | ExprKind::FunctionPublic(expr) =>
        {
            collect_global_symbols(expr, out, in_function);
        }
        ExprKind::AnonymousFunction { .. } => {}
        _ => {}
    }
}

fn collect_free_symbols(expr: &Expr, globals: &FxHashSet<SymbolId>, out: &mut FxHashSet<SymbolId>)
{
    match &expr.kind
    {
        ExprKind::Identifier { name, slot: None } =>
        {
            if !globals.contains(name)
            {
                out.insert(*name);
            }
        }
        ExprKind::Reference(name) =>
        {
            if !globals.contains(name)
            {
                out.insert(*name);
            }
        }
        ExprKind::Assignment { value, .. } => collect_free_symbols(value, globals, out),
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            collect_free_symbols(target, globals, out);
            collect_free_symbols(index, globals, out);
            collect_free_symbols(value, globals, out);
        }
        ExprKind::BinaryOp { left, right, .. } =>
        {
            collect_free_symbols(left, globals, out);
            collect_free_symbols(right, globals, out);
        }
        ExprKind::Not(expr) | ExprKind::Clone(expr) | ExprKind::EnvFreeze(expr) =>
        {
            collect_free_symbols(expr, globals, out);
        }
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } =>
        {
            collect_free_symbols(left, globals, out);
            collect_free_symbols(right, globals, out);
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            collect_free_symbols(condition, globals, out);
            collect_free_symbols(then_branch, globals, out);
            if let Some(expr) = else_branch
            {
                collect_free_symbols(expr, globals, out);
            }
        }
        ExprKind::While { condition, body } =>
        {
            collect_free_symbols(condition, globals, out);
            collect_free_symbols(body, globals, out);
        }
        ExprKind::Block(items) =>
        {
            for item in items
            {
                collect_free_symbols(item, globals, out);
            }
        }
        ExprKind::Call { function, args, .. } =>
        {
            collect_free_symbols(function, globals, out);
            for arg in args
            {
                collect_free_symbols(arg, globals, out);
            }
        }
        ExprKind::Array(items) =>
        {
            for item in items
            {
                collect_free_symbols(item, globals, out);
            }
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                collect_free_symbols(k, globals, out);
                collect_free_symbols(v, globals, out);
            }
        }
        ExprKind::Index { target, index } =>
        {
            collect_free_symbols(target, globals, out);
            collect_free_symbols(index, globals, out);
        }
        ExprKind::Return(value) =>
        {
            if let Some(expr) = value
            {
                collect_free_symbols(expr, globals, out);
            }
        }
        ExprKind::FilePublic(expr) | ExprKind::FunctionPublic(expr) =>
        {
            collect_free_symbols(expr, globals, out);
        }
        ExprKind::FunctionDef { .. } | ExprKind::AnonymousFunction { .. } => {}
        _ => {}
    }
}

#[derive(Clone)]
struct WatAnonFunction
{
    key: usize,
    params: Vec<Param>,
    body: Expr,
    slots: Option<Rc<Vec<Rc<String>>>>,
    line: usize,
}

#[derive(Clone)]
struct WatNamedFunction
{
    key: usize,
    name: SymbolId,
    params: Vec<Param>,
    body: Expr,
    slots: Option<Rc<Vec<Rc<String>>>>,
    line: usize,
    is_top_level: bool,
}

fn collect_anonymous_functions(expr: &Expr, out: &mut Vec<WatAnonFunction>)
{
    match &expr.kind
    {
        ExprKind::AnonymousFunction { params, body, slots } =>
        {
            out.push(WatAnonFunction {
                key: expr as *const Expr as usize,
                params: params.clone(),
                body: (*body.as_ref()).clone(),
                slots: slots.clone(),
                line: expr.line,
            });
            collect_anonymous_functions(body, out);
        }
        ExprKind::FilePublic(expr) | ExprKind::FunctionPublic(expr) =>
        {
            collect_anonymous_functions(expr, out);
        }
        ExprKind::FunctionDef { body, .. } => collect_anonymous_functions(body, out),
        ExprKind::Assignment { value, .. } => collect_anonymous_functions(value, out),
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            collect_anonymous_functions(target, out);
            collect_anonymous_functions(index, out);
            collect_anonymous_functions(value, out);
        }
        ExprKind::BinaryOp { left, right, .. } =>
        {
            collect_anonymous_functions(left, out);
            collect_anonymous_functions(right, out);
        }
        ExprKind::Not(expr) | ExprKind::Clone(expr) | ExprKind::EnvFreeze(expr) =>
        {
            collect_anonymous_functions(expr, out);
        }
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } =>
        {
            collect_anonymous_functions(left, out);
            collect_anonymous_functions(right, out);
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            collect_anonymous_functions(condition, out);
            collect_anonymous_functions(then_branch, out);
            if let Some(expr) = else_branch
            {
                collect_anonymous_functions(expr, out);
            }
        }
        ExprKind::While { condition, body } =>
        {
            collect_anonymous_functions(condition, out);
            collect_anonymous_functions(body, out);
        }
        ExprKind::Block(items) =>
        {
            for item in items
            {
                collect_anonymous_functions(item, out);
            }
        }
        ExprKind::Call { function, args, .. } =>
        {
            collect_anonymous_functions(function, out);
            for arg in args
            {
                collect_anonymous_functions(arg, out);
            }
        }
        ExprKind::Array(items) =>
        {
            for item in items
            {
                collect_anonymous_functions(item, out);
            }
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                collect_anonymous_functions(k, out);
                collect_anonymous_functions(v, out);
            }
        }
        ExprKind::Index { target, index } =>
        {
            collect_anonymous_functions(target, out);
            collect_anonymous_functions(index, out);
        }
        ExprKind::Return(value) =>
        {
            if let Some(expr) = value
            {
                collect_anonymous_functions(expr, out);
            }
        }
        _ => {}
    }
}

fn collect_named_functions(expr: &Expr, out: &mut Vec<WatNamedFunction>, in_function: bool)
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
            out.push(WatNamedFunction {
                key: expr as *const Expr as usize,
                name: *name,
                params: params.clone(),
                body: (*body.as_ref()).clone(),
                slots: slots.clone(),
                line: expr.line,
                is_top_level: !in_function,
            });
            collect_named_functions(body, out, true);
        }
        ExprKind::AnonymousFunction { body, .. } =>
        {
            collect_named_functions(body, out, true);
        }
        ExprKind::FilePublic(expr) | ExprKind::FunctionPublic(expr) =>
        {
            collect_named_functions(expr, out, in_function);
        }
        ExprKind::Assignment { value, .. } => collect_named_functions(value, out, in_function),
        ExprKind::IndexAssignment {
            target,
            index,
            value,
        } =>
        {
            collect_named_functions(target, out, in_function);
            collect_named_functions(index, out, in_function);
            collect_named_functions(value, out, in_function);
        }
        ExprKind::BinaryOp { left, right, .. } =>
        {
            collect_named_functions(left, out, in_function);
            collect_named_functions(right, out, in_function);
        }
        ExprKind::Not(expr) | ExprKind::Clone(expr) | ExprKind::EnvFreeze(expr) =>
        {
            collect_named_functions(expr, out, in_function);
        }
        ExprKind::And { left, right }
        | ExprKind::AndBool { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::OrBool { left, right } =>
        {
            collect_named_functions(left, out, in_function);
            collect_named_functions(right, out, in_function);
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } =>
        {
            collect_named_functions(condition, out, in_function);
            collect_named_functions(then_branch, out, in_function);
            if let Some(expr) = else_branch
            {
                collect_named_functions(expr, out, in_function);
            }
        }
        ExprKind::While { condition, body } =>
        {
            collect_named_functions(condition, out, in_function);
            collect_named_functions(body, out, in_function);
        }
        ExprKind::Block(items) =>
        {
            for item in items
            {
                collect_named_functions(item, out, in_function);
            }
        }
        ExprKind::Call { function, args, .. } =>
        {
            collect_named_functions(function, out, in_function);
            for arg in args
            {
                collect_named_functions(arg, out, in_function);
            }
        }
        ExprKind::Array(items) =>
        {
            for item in items
            {
                collect_named_functions(item, out, in_function);
            }
        }
        ExprKind::Map(entries) =>
        {
            for (k, v) in entries
            {
                collect_named_functions(k, out, in_function);
                collect_named_functions(v, out, in_function);
            }
        }
        ExprKind::Index { target, index } =>
        {
            collect_named_functions(target, out, in_function);
            collect_named_functions(index, out, in_function);
        }
        ExprKind::Return(value) =>
        {
            if let Some(expr) = value
            {
                collect_named_functions(expr, out, in_function);
            }
        }
        _ => {}
    }
}

fn emit_function_value(
    ctx: &mut WatContext,
    internal_name: &str,
    export_name: Option<&str>,
    param_count: usize,
    local_count: usize,
    captures: &[SymbolId],
    locals: &FxHashMap<SymbolId, usize>,
    body: &Expr,
    emit_wasi_prelude: bool,
) -> Result<(), String>
{
    let mut capture_map = FxHashMap::default();
    for (idx, sym) in captures.iter().enumerate()
    {
        capture_map.insert(*sym, idx);
    }
    let old_captures = std::mem::replace(&mut ctx.current_captures, capture_map);
    let old_locals = std::mem::replace(&mut ctx.current_locals, locals.clone());

    ctx.out.push_str(&format!(
        "  (func ${internal_name} (param $env i64) (param $args_ptr i32) (param $argc i32) (result i64)\n"
    ));
    for idx in 0..param_count
    {
        ctx.out.push_str(&format!("    (local $r{idx} i64)\n"));
    }
    for reg in param_count..local_count
    {
        ctx.out
            .push_str(&format!("    (local $r{reg} i64)\n"));
    }
    ctx.out.push_str("    (local $tmp i64)\n");
    ctx.out.push_str("    (local $tmp2 i64)\n");
    ctx.out.push_str("    (local $tmp3 i64)\n");
    ctx.out.push_str("    (local $tmp4 i64)\n");
    ctx.out.push_str("    (local $tmp_ptr i32)\n");
    ctx.out.push_str("    (local $tmp_ptr2 i32)\n");
    ctx.out.push_str("    (local $args_base i32)\n");
    ctx.out.push_str("    (local $tmp_i32 i32)\n");
    ctx.out.push_str("    (local $loop_index i32)\n");
    ctx.out.push_str("    (local $loop_limit i32)\n");
    if param_count > 0
    {
        for idx in 0..param_count
        {
            ctx.out.push_str("    local.get $args_ptr\n");
            ctx.out.push_str(&format!("    i32.const {}\n", idx * 8));
            ctx.out.push_str("    i32.add\n");
            ctx.out.push_str("    i64.load\n");
            ctx.out.push_str(&format!("    local.set $r{idx}\n"));
        }
    }
    let old_f64_arrays = std::mem::replace(&mut ctx.f64_arrays, collect_f64_arrays(body));
    if emit_wasi_prelude
    {
        let std_idx = ctx
            .global_names
            .get(&intern::intern_symbol("std"))
            .cloned();
        if let Some(idx) = std_idx
        {
            emit_wat_std_map(ctx)?;
            ctx.out.push_str(&format!("    i32.const {idx}\n"));
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $global_set\n");
            ctx.out.push_str("    drop\n");
        }
        let program_idx = ctx
            .global_names
            .get(&intern::intern_symbol("program"))
            .cloned();
        if let Some(idx) = program_idx
        {
            emit_wat_program_map(ctx)?;
            ctx.out.push_str(&format!("    i32.const {idx}\n"));
            ctx.out.push_str("    local.get $tmp\n");
            ctx.out.push_str("    call $global_set\n");
            ctx.out.push_str("    drop\n");
        }
    }
    emit_expr_value(ctx, body)?;
    ctx.f64_arrays = old_f64_arrays;
    ctx.out.push_str("    return\n");
    ctx.out.push_str("  )\n");
    if let Some(name) = export_name
    {
        ctx.out
            .push_str(&format!("  (export \"{name}\" (func ${internal_name}))\n"));
    }

    ctx.current_captures = old_captures;
    ctx.current_locals = old_locals;
    Ok(())
}

pub fn dump_wat(ast: &Expr, wasi: WasiTarget) -> Result<String, String>
{
    let mut ctx = WatContext::new();
    ctx.out.push_str("(module\n");
    ctx.out
        .push_str(&format!("  ;; kansei-wat wasi={}\n", wasi.as_str()));
    let rt_strings = WatRuntimeStrings::new(&mut ctx);
    ctx.runtime_strings = Some(rt_strings);
    emit_wat_runtime(&mut ctx, wasi, &rt_strings);
    if wasi == WasiTarget::Wasip1
    {
        emit_wat_builtins(&mut ctx);
    }

    let mut emitted = 0usize;
    let mut last_err: Option<String> = None;
    let mut has_main = false;
    let mut globals = Vec::new();
    collect_global_symbols(ast, &mut globals, false);
    if wasi == WasiTarget::Wasip1
    {
        for name in ["print", "puts", "eprint", "eputs", "log", "std", "program"]
        {
            let sym = intern::intern_symbol(name);
            if !globals.contains(&sym)
            {
                globals.push(sym);
            }
        }
    }
    for (idx, sym) in globals.iter().enumerate()
    {
        ctx.global_names.insert(*sym, idx);
    }
    ctx.global_set = globals.iter().cloned().collect();

    let mut functions = Vec::new();
    collect_named_functions(ast, &mut functions, false);
    let mut anon_exprs = Vec::new();
    collect_anonymous_functions(ast, &mut anon_exprs);

    let mut func_idx = 0usize;
    if wasi == WasiTarget::Wasip1
    {
        for (name, internal) in [
            ("print", "builtin_print"),
            ("puts", "builtin_puts"),
            ("eprint", "builtin_eprint"),
            ("eputs", "builtin_eputs"),
            ("log", "builtin_log"),
        ]
        {
            let sym = intern::intern_symbol(name);
            ctx.builtin_names
                .insert(sym, internal.to_string());
            ctx.func_indices
                .insert(internal.to_string(), func_idx as i32);
            func_idx += 1;
        }
        for internal in ["builtin_float64_sqrt", "builtin_int64_parse"]
        {
            ctx.func_indices
                .insert(internal.to_string(), func_idx as i32);
            func_idx += 1;
        }
    }
    for func in functions.iter().cloned()
    {
        let internal = format!("f{func_idx}");
        ctx.func_indices.insert(internal.clone(), func_idx as i32);
        ctx.func_def_names.insert(func.key, internal.clone());
        func_idx += 1;
        if func.is_top_level
        {
            ctx.func_names.entry(func.name).or_insert(internal);
        }
    }
    for anon in &anon_exprs
    {
        let internal = format!("f{func_idx}");
        ctx.func_indices.insert(internal.clone(), func_idx as i32);
        func_idx += 1;
        ctx.anon_names
            .insert(anon.key, internal);
    }

    for anon in &anon_exprs
    {
        let (resolved_body, _slot_map) = if let Some(slot_names) = anon.slots.clone()
        {
            let mut locals_map = FxHashMap::default();
            for (idx, name) in slot_names.iter().enumerate()
            {
                let sym = intern::intern_symbol(name.as_str());
                locals_map.insert(sym, idx);
            }
            (anon.body.clone(), locals_map)
        }
        else
        {
            let mut locals = HashSet::new();
            collect_declarations(&anon.body, &mut locals);
            let (map, _) = build_slot_map(&anon.params, locals);
            let mut resolved = anon.body.clone();
            resolve(&mut resolved, &map);
            (resolved, map)
        };
        let mut free = FxHashSet::default();
        collect_free_symbols(&resolved_body, &ctx.global_set, &mut free);
        let mut captures: Vec<SymbolId> = free.into_iter().collect();
        captures.sort_unstable();
        ctx.anon_captures.insert(anon.key, captures);
    }

    for func in &functions
    {
        let (resolved_body, _slot_map) = if let Some(slot_names) = func.slots.clone()
        {
            let mut locals_map = FxHashMap::default();
            for (idx, name) in slot_names.iter().enumerate()
            {
                let sym = intern::intern_symbol(name.as_str());
                locals_map.insert(sym, idx);
            }
            (func.body.clone(), locals_map)
        }
        else
        {
            let mut locals = HashSet::new();
            collect_declarations(&func.body, &mut locals);
            let (map, _) = build_slot_map(&func.params, locals);
            let mut resolved = func.body.clone();
            resolve(&mut resolved, &map);
            (resolved, map)
        };
        let mut free = FxHashSet::default();
        collect_free_symbols(&resolved_body, &ctx.global_set, &mut free);
        let mut captures: Vec<SymbolId> = free.into_iter().collect();
        captures.sort_unstable();
        ctx.func_def_captures.insert(func.key, captures);
    }

    let local_count = local_count_for_expr(ast);
    let empty_locals = FxHashMap::default();
    let out_len_before_main = ctx.out.len();
    match emit_function_value(
        &mut ctx,
        "__main",
        None,
        0,
        local_count,
        &[],
        &empty_locals,
        ast,
        wasi == WasiTarget::Wasip1,
    )
    {
        Ok(()) =>
        {
            emitted += 1;
            has_main = true;
        }
        Err(err) =>
        {
            last_err = Some(err.clone());
            ctx.out.truncate(out_len_before_main);
            ctx.out
                .push_str(&format!("  ;; top-level unsupported: {err}\n"));
        }
    }

    if !ctx.func_indices.is_empty()
    {
        let mut elems = vec![String::new(); ctx.func_indices.len()];
        for (name, idx) in &ctx.func_indices
        {
            elems[*idx as usize] = name.clone();
        }
        let table_len = elems.len();
        ctx.out
            .push_str(&format!("  (table $functable {table_len} funcref)\n"));
        ctx.out.push_str("  (elem (i32.const 0)");
        for name in &elems
        {
            ctx.out.push_str(&format!(" ${name}"));
        }
        ctx.out.push_str(")\n");
    }

    for func in functions.iter().cloned()
    {
        let internal = ctx
            .func_def_names
            .get(&func.key)
            .cloned()
            .unwrap_or_else(|| "f0".to_string());
        let mut slot_map = FxHashMap::default();
        let (resolved_body, slot_names) = if let Some(slot_names) = func.slots
        {
            for (idx, name) in slot_names.iter().enumerate()
            {
                let sym = intern::intern_symbol(name.as_str());
                slot_map.insert(sym, idx);
            }
            (func.body, slot_names)
        }
        else
        {
            let mut locals = HashSet::new();
            collect_declarations(&func.body, &mut locals);
            let (map, slot_names) = build_slot_map(&func.params, locals);
            let mut resolved = func.body;
            resolve(&mut resolved, &map);
            slot_map = map;
            (resolved, Rc::new(slot_names))
        };

        let captures = ctx
            .func_def_captures
            .get(&func.key)
            .cloned()
            .unwrap_or_default();

        let export_name = if func.is_top_level
        {
            Some(symbol_name(func.name).as_str().to_string())
        }
        else
        {
            None
        };
        let local_count = slot_names.len().max(func.params.len());
        let out_len_before_func = ctx.out.len();
        match emit_function_value(
            &mut ctx,
            &internal,
            export_name.as_deref(),
            func.params.len(),
            local_count,
            &captures,
            &slot_map,
            &resolved_body,
            false,
        )
        {
            Ok(()) => emitted += 1,
            Err(err) =>
            {
                last_err = Some(err.clone());
                ctx.out.truncate(out_len_before_func);
                ctx.out.push_str(&format!(
                    "  (func ${internal} (param $env i64) (param $args_ptr i32) (param $argc i32) (result i64)\n"
                ));
                ctx.out.push_str("    call $tag_nil\n");
                ctx.out.push_str("  )\n");
                if let Some(export_name) = export_name.as_deref()
                {
                    ctx.out.push_str(&format!(
                        "  (export \"{export_name}\" (func ${internal}))\n"
                    ));
                }
                ctx.out.push_str(&format!(
                    "  ;; skipped function at line {} ({err})\n",
                    func.line
                ));
            }
        }
    }

    for anon in &anon_exprs
    {
        let internal = ctx
            .anon_names
            .get(&anon.key)
            .cloned()
            .unwrap_or_else(|| "f0".to_string());
        let mut slot_map = FxHashMap::default();
        let (resolved_body, slot_names) = if let Some(slot_names) = anon.slots.clone()
        {
            for (idx, name) in slot_names.iter().enumerate()
            {
                let sym = intern::intern_symbol(name.as_str());
                slot_map.insert(sym, idx);
            }
            (anon.body.clone(), slot_names)
        }
        else
        {
            let mut locals = HashSet::new();
            collect_declarations(&anon.body, &mut locals);
            let (map, slot_names) = build_slot_map(&anon.params, locals);
            let mut resolved = anon.body.clone();
            resolve(&mut resolved, &map);
            slot_map = map;
            (resolved, Rc::new(slot_names))
        };
        let local_count = slot_names.len().max(anon.params.len());
        let captures = ctx
            .anon_captures
            .get(&anon.key)
            .cloned()
            .unwrap_or_default();
        let out_len_before_anon = ctx.out.len();
        match emit_function_value(
            &mut ctx,
            &internal,
            None,
            anon.params.len(),
            local_count,
            &captures,
            &slot_map,
            &resolved_body,
            false,
        )
        {
            Ok(()) => emitted += 1,
            Err(err) =>
            {
                last_err = Some(err.clone());
                ctx.out.truncate(out_len_before_anon);
                ctx.out.push_str(&format!(
                    "  (func ${internal} (param $env i64) (param $args_ptr i32) (param $argc i32) (result i64)\n"
                ));
                ctx.out.push_str("    call $tag_nil\n");
                ctx.out.push_str("  )\n");
                ctx.out.push_str(&format!(
                    "  ;; skipped anon function at line {} ({err})\n",
                    anon.line
                ));
            }
        }
    }

    if has_main
    {
        ctx.out.push_str("  (func $_start\n");
        ctx.out.push_str("    (local $tmp i64)\n");
        ctx.out.push_str("    (local $tmp2 i64)\n");
        ctx.out.push_str("    (local $tmp3 i64)\n");
        ctx.out.push_str("    (local $tmp4 i64)\n");
        ctx.out
            .push_str(&format!("    i32.const {}\n", globals.len()));
        ctx.out.push_str("    call $globals_init\n");
        ctx.out.push_str("    i32.const 1024\n");
        ctx.out.push_str("    call $alloc\n");
        ctx.out.push_str("    global.set $args_sp\n");
        for (sym, internal) in ctx.builtin_names.iter()
        {
            if let Some(idx) = ctx.global_names.get(sym)
            {
                let func_idx = ctx
                    .func_indices
                    .get(internal)
                    .cloned()
                    .unwrap_or(0);
                ctx.out.push_str(&format!("    i32.const {func_idx}\n"));
                ctx.out.push_str("    call $tag_nil\n");
                ctx.out.push_str("    call $make_func\n");
                ctx.out.push_str("    local.set $tmp\n");
                ctx.out.push_str(&format!("    i32.const {idx}\n"));
                ctx.out.push_str("    local.get $tmp\n");
                ctx.out.push_str("    call $global_set\n");
                ctx.out.push_str("    drop\n");
            }
        }
        ctx.out.push_str("    call $tag_nil\n");
        ctx.out.push_str("    i32.const 0\n");
        ctx.out.push_str("    i32.const 0\n");
        ctx.out.push_str("    call $__main\n");
        ctx.out.push_str("    drop\n");
        ctx.out.push_str("  )\n");
        ctx.out.push_str("  (export \"_start\" (func $_start))\n");
    }

    if emitted == 0
    {
        if let Some(err) = last_err
        {
            return Err(format!("WAT dump failed: {err}"));
        }
        return Err("WAT dump failed: no supported functions could be emitted.".to_string());
    }

    for (offset, bytes) in ctx.data_segments.iter()
    {
        ctx.out
            .push_str(&format!("  (data (i32.const {offset}) \""));
        for &b in bytes
        {
            match b
            {
                b'\\' => ctx.out.push_str("\\\\"),
                b'"' => ctx.out.push_str("\\\""),
                0x20..=0x7e => ctx.out.push(b as char),
                _ => ctx.out.push_str(&format!("\\{:02x}", b)),
            }
        }
        ctx.out.push_str("\")\n");
    }

    ctx.out.push_str(")\n");
    Ok(ctx.out)
}


#[cfg(test)]
mod tests
{
    use super::*;

    #[test]
    fn generated_wasip1_wat_validates()
    {
        let mut ast = crate::parser::parse_source("puts 1 + 2").unwrap();
        resolve_slots(&mut ast);
        let text = dump_wat(&ast, WasiTarget::Wasip1).unwrap();
        wasmtime::Module::new(&wasmtime::Engine::default(), text.as_bytes()).unwrap();
    }
}
