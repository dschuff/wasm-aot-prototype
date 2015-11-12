;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: sexpr-wasm -d %t1 > %t2
;; RUN: sexpr-wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
(module
  (func
    (i32.load (i32.const 0))
    (i32.load8_s (i32.const 0))
    (i32.load16_s (i32.const 0))
    (i32.load8_u (i32.const 0))
    (i32.load16_u (i32.const 0))
    (i64.load (i32.const 0))
    (i64.load8_s (i32.const 0))
    (i64.load16_s (i32.const 0))
    (i64.load32_s (i32.const 0))
    (i64.load8_u (i32.const 0))
    (i64.load16_u (i32.const 0))
    (i64.load32_u (i32.const 0))
    (f32.load (i32.const 0))
    (f64.load (i32.const 0))))
