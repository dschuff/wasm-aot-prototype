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
  (func $voidret (param f64) (f64.const 1.0))
;; CHECK: define internal void @"$voidret"
  (export "voidret" $voidret)
)



(assert_trap (invoke "foo") "foo")
;; CHECK: define void @AssertTrap_18()
;; CHECK: call void @__wasm_assert_trap(i32 18, void ()* @Invoke
;; @Invoke should have void return instead of i32 and no args
;; CHECK: define void @Invoke_18()
(assert_trap (invoke "bar" (f32.const 1)) "bar")
;; CHECK: define void @AssertTrap_23()
;; CHECK: call void @__wasm_assert_trap(i32 23, void ()* @Invoke_23
;; @Invoke should have void return and no args
;; CHECK: define void @Invoke_23()
