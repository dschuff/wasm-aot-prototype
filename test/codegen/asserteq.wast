;; RUN: waot -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RUN: not waot -S %s
(module
  (func $foo (result i32) (i32.const 0))
;; CHECK: define internal i32 @"$foo"
  (export "foo" $foo)
  (func $bar (param f32) (result f32) (f32.const 1.0))
;; CHECK: define internal float @"$bar"
  (export "bar" $bar))

(assert_eq (invoke "foo") (i32.const 0))
;; CHECK: define void @AssertEq()
;; CHECK: call i32 @Invoke
;; CHECK: %1 = icmp eq i32 %0, 0
;; CHECK: br i1 %1, label %AssertSuccess, label %AssertFail
;; CHECK: AssertSuccess:
;; CHECK: ret void
;; CHECK: AssertFail:
;; CHECK: call void @llvm.trap
;; CHECK: define i32 @Invoke
;; CHECK: call i32 @"$foo"()

(assert_eq (invoke "bar" (f32.const 0)) (f32.const 0))
;; CHECK: define void @AssertEq
;; CHECK: call float @Invoke
;; CHECK: fcmp oeq float %0, 0.000000e+00
;; CHECK: define float @Invoke
;; CHECK: call float @"$bar"(float 0.000000e+00

;; ok to use more complex exprs
(assert_eq
  (invoke "bar"
    (block (f32.const 1) (f32.const 10)))
  (f32.const 11))
