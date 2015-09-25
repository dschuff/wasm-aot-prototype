;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump -spec-test-script %s > %t1
;; RUN: sexpr-wasm --multi-module -d %t1 > %t2
;; RUN: sexpr-wasm --multi-module -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump  -spec-test-script %t1 | diff %t1 -
;; Test that the -spec-test-script flag is required for multi-module
;; RUN: not sexpr_dump %s
(module
  (export "test" $test)
  (func $test (param i32) (result i32) (i32.const 3)))

(invoke "test" (i32.const 1))
(invoke "test" (i32.const 100))
(invoke "test" (i32.const -30))
