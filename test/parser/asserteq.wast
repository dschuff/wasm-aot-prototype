;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump -spec-test-script %s > %t1
;; RUN: sexpr-wasm --multi-module -d %t1 > %t2
;; RUN: sexpr-wasm --multi-module -d %s | diff - %t2
;; Printing src locs with assert statements means that files with different
;; names have different output when dumped, so we can't do the round tripping
;; test, but, that's OK for asserts.
;; Test that the -spec-test-script flag is required for multi-module
;; RUN: not sexpr_dump %s
;; Test that the line number info is correct.
;; RUN: sexpr_dump -spec-test-script %s | FileCheck %s
(module
  (func $foo (result i32) (i32.const 0))
  (export "foo" $foo)
  (func $bar (param f32) (result f32) (f32.const 1.0))
  (export "bar" $bar))

;; CHECK: asserteq.wast:19
(assert_return (invoke "foo") (i32.const 0))
;; CHECK: asserteq.wast:21
(assert_return (invoke "bar" (f32.const 0)) (f32.const 0))
;; ok to use more complex exprs
(assert_return
  (invoke "bar"
    (block (f32.const 1) (f32.const 10)))
  (f32.const 11))

(assert_return (invoke "foo") (block (i32.const 1)(nop)(i32.const 2)))

(assert_trap (invoke "foo") "foo")
(assert_trap (invoke "bar" (f32.const 1)) "bar")