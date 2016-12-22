;; RUN: wat -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RUN: not wat -S %s

;; CHECK: @__wasm_init_array = global [5 x void ()*] [void ()* @.invoke_ctor, void ()* bitcast (i32 ()* @Invoke_13 to void ()*), void ()* bitcast (i32 ()* @Invoke_17 to void ()*), void ()* bitcast (i32 ()* @Invoke_21 to void ()*), void ()* null]
;; CHECK: @__wasm_fini_array = global [2 x void ()*] [void ()* @.invoke_dtor, void ()* null]

(module
  (export "test" $test)
  (func $test (param i32) (result i32) (i32.const 3)))
;; CHECK: define internal i32 @"$test"(i32)

(invoke "test" (i32.const 1))
;; CHECK: define i32 @Invoke_13
;; CHECK: call i32 @"$test"(i32 1)

(invoke "test" (i32.const 100))
;; CHECK: define i32 @Invoke_17
;; CHECK: call i32 @"$test"(i32 100)

(invoke "test" (i32.const -30))
;; CHECK: define i32 @Invoke_21
;; CHECK: call i32 @"$test"(i32 -30)
