;; RUN: wat -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RUN: not wat -S %s
(module (memory 100)
  (func $foo (result i32) (i32.const 0))
  (export "foo" $foo)
)

(assert_return (invoke "foo") (i32.const 0))
;; CHECK: call void @__wasm_allocate_memory(i64 100)
;; CHECK: call i32 @Invoke_
;; CHECK: call void @__wasm_free_memory()

(module (memory 200)
  (func $foo (result f32) (f32.const nan))
  (export "foo" $foo)
)

(assert_return_nan (invoke "foo"))
;; CHECK: call void @__wasm_allocate_memory(i64 200)
;; CHECK: call float @Invoke_
;; CHECK: call void @__wasm_free_memory()

(assert_trap (invoke "foo") "")
;; CHECK: call void @__wasm_allocate_memory(i64 200)
;; CHECK: call void @__wasm_assert_trap
;; CHECK: call void @__wasm_free_memory()
