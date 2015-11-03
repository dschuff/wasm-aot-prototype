;; RUN: wat -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RUN: not wat -S %s
(module (memory 100)
  (func $foo (result i32) (i32.const 0))
  (export "foo" $foo)
)

;; CHECK: define internal void @.memory_assert_ctor() {
;; CHECK: call void @__wasm_allocate_memory(i8** @.memory_assert_membase, i64 100)
;; CHECK: define internal void @.memory_assert_dtor() {
;; CHECK: call void @__wasm_free_memory(i8** @.memory_assert_membase)

;; CHECK: define internal void @.module1_ctor() {
;; CHECK: call void @__wasm_allocate_memory(i8** @.module1_membase, i64 200)
;; CHECK: define internal void @.module1_dtor() {
;; CHECK: call void @__wasm_free_memory(i8** @.module1_membase)


(assert_return (invoke "foo") (i32.const 0))

;; CHECK: call i32 @Invoke_


(module (memory 200)
  (func $foo (result f32) (f32.const nan))
  (export "foo" $foo)
)


(assert_return_nan (invoke "foo"))

;; CHECK: call float @Invoke_

(assert_trap (invoke "foo") "")
;; CHECK: call void @__wasm_assert_trap
