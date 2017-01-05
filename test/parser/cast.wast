;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: wast2wasm -d %t1 > %t2
;; RUN: wast2wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
;; int->float reinterpret unsupported currently
(module
  (func
    (drop (f32.reinterpret/i32 (i32.const 0)))
    (drop (i32.reinterpret/f32 (f32.const 0)))
    (drop (f64.reinterpret/i64 (i64.const 0)))
    (drop (i64.reinterpret/f64 (f64.const 0)))))
