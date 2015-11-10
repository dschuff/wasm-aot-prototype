;; RUN: wat -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RU N: not wat -S %s
(module
  (memory 100
    (segment 0 "hi")
    (segment 4 "hello")
    (segment 10 "goodbye")
    (segment 20 "adios")))

;; CHECK: @.memory-segment-many.segment_0 = internal global [2 x i8] c"hi"
;; CHECK: @.memory-segment-many.segment_4 = internal global [5 x i8] c"hello"
;; CHECK: @.memory-segment-many.segment_10 = internal global [7 x i8] c"goodbye"
;; CHECK: @.memory-segment-many.segment_20 = internal global [5 x i8] c"adios"
;; CHECK: @.wasm_membase = external global [4294967296 x i8], section ".membase"

;; CHECK: define internal void @.memory-segment-many_ctor() {
;; CHECK: call void @__wasm_init_memory({{.*}}, i64 100)
;; CHECK: call void{{.*}}@__wasm_init_segment{{.*}}@.wasm_membase{{.*}}, i64 0, i64 2
;; CHECK: call void{{.*}}@__wasm_init_segment{{.*}}@.wasm_membase{{.*}} i64 4, i64 5
;; CHECK: call void{{.*}}@__wasm_init_segment{{.*}}@.wasm_membase{{.*}} i64 10, i64 7
;; CHECK: call void{{.*}}@__wasm_init_segment{{.*}}@.wasm_membase{{.*}} i64 20, i64 5