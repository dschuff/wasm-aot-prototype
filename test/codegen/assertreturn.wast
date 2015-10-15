;; RUN: wat -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RUN: not wat -S %s
(module
  (func $foo (result i32) (i32.const 0))
;; CHECK: define internal i32 @"$foo"
  (export "foo" $foo)
  (func $bar (param f32) (result f32) (f32.const 1.0))
;; CHECK: define internal float @"$bar"
  (export "bar" $bar)
  (func $baz (i32.const 0))
  (export "baz" $baz)
)

(assert_return (invoke "foo") (i32.const 0))
;; CHECK: define void @AssertReturn_15()
;; CHECK: call i32 @Invoke
;; CHECK: %assert_check = icmp eq i32 %0, 0
;; CHECK: br i1 %assert_check, label %AssertSuccess, label %AssertFail
;; CHECK: AssertSuccess:
;; CHECK: ret void
;; CHECK: AssertFail:
;; CHECK: call void @__wasm_assert_fail_i32(i32 15, i32 0, i32 %0)
;; CHECK: define i32 @Invoke
;; CHECK: call i32 @"$foo"()

(assert_return (invoke "bar" (f32.const 0)) (f32.const 0))
;; CHECK: define void @AssertReturn_27()
;; CHECK: call float @Invoke
;; CHECK: fcmp oeq float %0, 0.000000e+00
;; CHECK: AssertFail:
;; CHECK: call void @__wasm_assert_fail_f32(i32 27, float 0.000000e+00, float %0)
;; CHECK: define float @Invoke
;; CHECK: call float @"$bar"(float 0.000000e+00

;; ok to use more complex exprs
(assert_return
  (invoke "bar"
    (block (f32.const 1) (f32.const 10)))
  (f32.const 11))

(assert_return (invoke "baz"))
;; CHECK: define void @AssertReturn_42()
;; CHECK: call void @Invoke_42()
;; CHECK-NEXT: ret void

;; Check for main function that calls all the assertreturns
;; CHECK: define i32 @main()
;; CHECK: call void @AssertReturn_15()
;; CHECK: call void @AssertReturn_27()
;; CHECK: call void @AssertReturn_37()
;; CHECK: call void @AssertReturn_42()
;; CHECK: [[STATUS:%.*]] = load i32, i32* @exit_status
;; CHECK: ret i32 [[STATUS]]


(assert_trap (invoke "foo") "foo")
(assert_trap (invoke "bar" (f32.const 1)) "bar")