;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: wast2wasm -d %t1 > %t2
;; RUN: wast2wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
(module (func (param i32)
  (call 0 (i32.const 1)))
  (func (param i32) (call 1 (i32.const 2)))
  (func (call 2))
  (func (call 1 (i32.const 1)))
)
