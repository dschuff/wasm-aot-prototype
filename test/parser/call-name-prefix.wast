;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: %sexpr-wasm -d %t1 > %t2
;; RUN: %sexpr-wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
;; Test that the callee name is printed
;; RUN: sexpr_dump %s | FileCheck %s
(module
  (func $foomore
    (call $foo (i32.const 0)))
;; CHECK: (call $foo (i32.const
  (func $foo (param i32)
    (nop)))
