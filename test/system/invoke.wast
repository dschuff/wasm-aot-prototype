;; RUN: waot_test_cc.py --spec-test-script %s -o %t1
;; RUN: %t1 | FileCheck %s
(module
  (export "test" $test)
;; TODO: make this call print with its argument
  (func $test (param i32)  (call_import 0 (i32.const 3)))
  (import "stdio" "print" (param i32)))

(invoke "test" (i32.const 1))
(invoke "test" (i32.const 100))
(invoke "test" (i32.const -30))

;; CHECK: 3
