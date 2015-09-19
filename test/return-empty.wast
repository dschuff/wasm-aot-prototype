;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr-dump %s > %t1
;; RUN: %sexpr-wasm -d %t1 > %t2
;; RUN: %sexpr-wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr-dump %t1 | diff %t1 -
(module
  (func (return)))
