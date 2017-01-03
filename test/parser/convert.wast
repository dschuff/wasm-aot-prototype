;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: sexpr-wasm -d %t1 > %t2
;; RUN: sexpr-wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
(module
  (func
    (drop (i32.wrap/i64 (i64.const 0)))
    (drop (i64.extend_u/i32 (i32.const 0)))
    (drop (i64.extend_s/i32 (i32.const 0)))
    (drop (i32.trunc_s/f32 (f32.const 0)))
    (drop (i32.trunc_u/f32 (f32.const 0)))
    (drop (i32.trunc_s/f64 (f64.const 0)))
    (drop (i32.trunc_u/f64 (f64.const 0)))
    (drop (i64.trunc_s/f32 (f32.const 0)))
    (drop (i64.trunc_u/f32 (f32.const 0)))
    (drop (i64.trunc_s/f64 (f64.const 0)))
    (drop (i64.trunc_u/f64 (f64.const 0)))
    (drop (f32.convert_s/i32 (i32.const 0)))
    (drop (f32.convert_u/i32 (i32.const 0)))
    (drop (f32.convert_s/i64 (i64.const 0)))
    (drop (f32.convert_u/i64 (i64.const 0)))
    (drop (f64.convert_s/i32 (i32.const 0)))
    (drop (f64.convert_u/i32 (i32.const 0)))
    (drop (f64.convert_s/i64 (i64.const 0)))
    (drop (f64.convert_u/i64 (i64.const 0)))
    (drop (f32.demote/f64 (f64.const 0)))
    (drop (f64.promote/f32 (f32.const 0)))))
