;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: sexpr-wasm -d %t1 > %t2
;; RUN: sexpr-wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
(module
 (func (local i32)
  (set_local 0 (i32.const 0)))
(func (param i32) (set_local 0 (i32.const 0)))
 (func
  (local $n i32)
  (set_local $n (i32.const 12)))
(func (param $n i32) (set_local $n (i32.const 0)))
(func (param i32) (local i32) (set_local 1 (i32.const 0)))
(func (param i32) (param $n f32)
    (local i32 i64)
    (local $m f64)
    (set_local 0 (i32.const 0))
    (set_local 1 (f32.const 0))
    (set_local $n (f32.const 0)) ;; 1
    (set_local 2 (i32.const 0))
    (set_local 3 (i64.const 0))
    (set_local $m (f64.const 0)) ;; 4
    (set_local 4 (f64.const 0)))
)