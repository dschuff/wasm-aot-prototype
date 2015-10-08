;; RUN: wat -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RUN: not wat -S %s
(module
  (func $foo (result i32) (i32.const 0))
;; CHECK: define internal i32 @"$foo"
  (export "foo" $foo)
  (func $bar (param f32) (result f32) (f32.const 1.0))
;; CHECK: define internal float @"$bar"
  (export "bar" $bar))

(assert_return (invoke "foo") (i32.const 0))
;; CHECK: define void @AssertReturn()
;; CHECK: call i32 @Invoke
;; CHECK: %assert_check = icmp eq i32 %0, 0
;; CHECK: br i1 %assert_check, label %AssertSuccess, label %AssertFail
;; CHECK: AssertSuccess:
;; CHECK: ret void
;; CHECK: AssertFail:
;; CHECK: call void @__assert_fail_i32(i32 1, i32 0, i32 %0)
;; CHECK: define i32 @Invoke
;; CHECK: call i32 @"$foo"()

(assert_return (invoke "bar" (f32.const 0)) (f32.const 0))
;; CHECK: define void @AssertReturn.1
;; CHECK: call float @Invoke
;; CHECK: fcmp oeq float %0, 0.000000e+00
;; CHECK: AssertFail:
;; CHECK: call void @__assert_fail_f32(i32 2, float 0.000000e+00, float %0)
;; CHECK: define float @Invoke
;; CHECK: call float @"$bar"(float 0.000000e+00

;; ok to use more complex exprs
(assert_return
  (invoke "bar"
    (block (f32.const 1) (f32.const 10)))
  (f32.const 11))

;; Check for main function that calls all the asserteqs
;; CHECK: define i32 @main()
;; CHECK: call void @AssertReturn()
;; CHECK: call void @AssertReturn.1()
;; CHECK: call void @AssertReturn
;; CHECK: [[STATUS:%.*]] = load i32, i32* @exit_status
;; CHECK: ret i32 [[STATUS]]
