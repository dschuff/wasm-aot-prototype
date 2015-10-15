;; RUN: wat -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RUN: not wat -S %s

(module
  (func $add (param $x f32) (param $y f32) (result f32) (f32.add (get_local $x) (get_local $y)))
  (export "add" $add)
  (func $ret_f64 (result f64) (f64.const 0.0))
  (export "ret_f64" $ret_f64)
)

(assert_return_nan (invoke "add" (f32.const -0x0p+0) (f32.const -nan)))
;; CHECK: define void @AssertReturnNaN_12()
;; CHECK: %0 = call float @Invoke_12
;; CHECK: call void @__wasm_assert_return_nan_f32(i32 12, float %0)

(assert_return_nan (invoke "ret_f64" ))
;; CHECK: define void @AssertReturnNaN_17()
;; CHECK: %0 = call double @Invoke_17
;; CHECK: call void @__wasm_assert_return_nan_f64(i32 17, double %0)
