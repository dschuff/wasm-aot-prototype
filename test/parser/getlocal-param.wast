;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: sexpr-wasm -d %t1 > %t2
;; RUN: sexpr-wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
(module
 (func (param i32) (get_local 0))
 (func (param i64) (local i32) (get_local 0) (get_local 1))
 (func (param i64) (local i32) (get_local 1) (get_local 0))
 (func (result f32) (param i64) (local i32)
  (get_local 0)
  (get_local 1)
  (f32.const 1.0))
)
