;; RUN: waot -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RUN: not waot -S %s
(module
  (export "test" $test)
  (func $test (param i32) (result i32) (i32.const 3)))
;; CHECK: define internal i32 @"$test"(i32)

(invoke "test" (i32.const 1))
;; CHECK: define i32 @Invoke
;; CHECK: call i32 @"$test"(i32 1)

(invoke "test" (i32.const 100))
;; CHECK: define i32 @Invoke
;; CHECK: call i32 @"$test"(i32 100)

(invoke "test" (i32.const -30))
;; CHECK: define i32 @Invoke
;; CHECK: call i32 @"$test"(i32 -30)