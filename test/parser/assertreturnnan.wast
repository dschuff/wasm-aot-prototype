;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump -spec-test-script %s > %t1
;; RUN: sexpr-wasm --spec %t1 > %t2
;; RUN: sexpr-wasm --spec %s | diff - %t2
;; Printing src locs with assert statements means that files with different
;; names have different output when dumped, so we can't do the round tripping
;; test, but, that's OK for asserts.
;; Test that the -spec-test-script flag is required for mult-module
;; RUN: not sexpr_dump %s
;; Test that the line number info is correct.
;; RUN: sexpr_dump -spec-test-script %s | FileCheck %s
(module
  (func $foo (param f32) (result f32)
    (f32.div (get_local 0) (f32.const 0)))
  (export "foo" $foo))

;; CHECK: assertreturnnan.wast:19
(assert_return_nan
  (invoke "foo" (f32.const 0)))
