;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: wast2wasm -d %t1 > %t2
;; RUN: wast2wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
(module
  (func
    (drop (i32.const 0))
    (drop (i32.const -2147483648))
    (drop (i32.const 4294967295))
    (drop (i32.const -0x80000000))
    (drop (i32.const 0xffffffff))
    (drop (i64.const 0))
    (drop (i64.const -9223372036854775808))
    (drop (i64.const 18446744073709551615))
    (drop (i64.const -0x8000000000000000))
    (drop (i64.const 0xffffffffffffffff))
    (drop (f32.const 0.0))
    (drop (f32.const 1e23))
    (drop (f32.const 1.234567e-5))
    (drop (f64.const 0.0))
    (drop (f64.const -0.987654321))
    (drop (f64.const 6.283185307179586))))
