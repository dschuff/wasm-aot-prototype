;; RUN: wat -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RUN: not wat -S %s

;; CHECK: @__wasm_init_array = appending global [7 x void ()*] [void ()* @AssertReturn_19, void ()* @AssertReturn_31, void ()* @AssertReturn_42, void ()* @AssertReturn_47, void ()* @AssertTrap_52, void ()* @AssertTrap_53, void ()* null]
;; CHECK: @__wasm_fini_array = appending global [1 x void ()*] zeroinitializer

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
;; CHECK: define void @AssertReturn_19()
;; CHECK: call i32 @Invoke_19
;; CHECK: %assert_check = icmp eq i32 %0, 0
;; CHECK: br i1 %assert_check, label %AssertSuccess, label %AssertFail
;; CHECK: AssertSuccess:
;; CHECK: ret void
;; CHECK: AssertFail:
;; CHECK: call void @__wasm_assert_fail_i32(i32 19, i32 0, i32 %0)
;; CHECK: define i32 @Invoke
;; CHECK: call i32 @"$foo"()

(assert_return (invoke "bar" (f32.const 0)) (f32.const 0))
;; CHECK: define void @AssertReturn_31()
;; CHECK: call float @Invoke
;; CHECK: bitcast float %0 to i32
;; CHECK: icmp eq i32 %invoke_result_int, 0
;; CHECK: AssertFail:
;; CHECK: call void @__wasm_assert_fail_f32(i32 31, float 0.000000e+00, float %0)
;; CHECK: define float @Invoke
;; CHECK: call float @"$bar"(float 0.000000e+00

;; ok to use more complex exprs
(assert_return
  (invoke "bar"
    (block (f32.const 1) (f32.const 10)))
  (f32.const 11))

(assert_return (invoke "baz"))
;; CHECK: define void @AssertReturn_47()
;; CHECK: call void @Invoke_47()
;; CHECK: ret void

(assert_trap (invoke "foo") "foo")
(assert_trap (invoke "bar" (f32.const 1)) "bar")