;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: sexpr-wasm -d %t1 > %t2
;; RUN: sexpr-wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
(module
  (func
    (i32.load offset=0 (i32.const 0))
    (i64.load offset=1 (i32.const 0))
    (i64.load8_s offset=2 (i32.const 0))
    (i64.load16_s offset=3 (i32.const 0))
    (i64.load32_s offset=4 (i32.const 0))
    (i64.load8_u offset=5 (i32.const 0))
    (i64.load16_u offset=6 (i32.const 0))
    (i64.load32_u offset=7 (i32.const 0))
    (i32.load8_s offset=8 (i32.const 0))
    (i32.load16_s offset=9 (i32.const 0))
    (i32.load8_u offset=10 (i32.const 0))
    (i32.load16_u offset=11 (i32.const 0))
    (f32.load offset=12 (i32.const 0))
    (f64.load offset=13 (i32.const 0))

    (i32.load offset=0 align=1 (i32.const 0))
    (i64.load offset=1 align=2 (i32.const 0))
    (i64.load8_s offset=2 align=4 (i32.const 0))
    (i64.load16_s offset=3 align=8 (i32.const 0))
    (i64.load32_s offset=4 align=16 (i32.const 0))
    (i64.load8_u offset=5 align=32 (i32.const 0))
    (i64.load16_u offset=6 align=64 (i32.const 0))
    (i64.load32_u offset=7 align=128 (i32.const 0))
    (i32.load8_s offset=8 align=64 (i32.const 0))
    (i32.load16_s offset=9 align=32 (i32.const 0))
    (i32.load8_u offset=10 align=16 (i32.const 0))
    (i32.load16_u offset=11 align=8 (i32.const 0))
    (f32.load offset=12 align=4 (i32.const 0))
    (f64.load offset=13 align=2 (i32.const 0))))
