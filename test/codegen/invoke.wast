;; RUN: wat -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RUN: not wat -S %s
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

;; Check that the test script calls the invokes
;; CHECK: define i32 @main()
;; CHECK: call i32 @Invoke
;; CHECK: call i32 @Invoke.1
;; CHECK: call i32 @Invoke
;; CHECK: ret i32 0
