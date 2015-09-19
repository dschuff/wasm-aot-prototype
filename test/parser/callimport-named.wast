;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr-dump %s > %t1
;; RUN: %sexpr-wasm -d %t1 > %t2
;; RUN: %sexpr-wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr-dump %t1 | diff %t1 -
;; Check that the callee name is printed
;; RUN: sexpr-dump %s | FileCheck %s
(module
  (import $bar "foo" "bar" (param f32))
  (func
    (call_import $bar (f32.const 0))))
;; CHECK: (call_import $bar (f32.const