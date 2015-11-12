;; RUN: wat -S %s | FileCheck %s

(module
  (func (local i32 i32)
    (i32.store8 offset=0 (i32.const 0) (i32.const 0))
;; CHECK: store i8 0, i8* getelementptr inbounds{{.*}}@.wasm_membase, i32 0, i32 0
    (i32.store16 offset=1 (i32.const 0) (i32.const 0))
;; CHECK: store i16 0, i16* bitcast (i8* getelementptr inbounds{{.*}}@.wasm_membase, i32 0, i32 1
    (i32.store offset=2 (i32.const 0) (i32.const 0))
;; CHECK: store i32 0, i32* bitcast (i8* getelementptr inbounds{{.*}}@.wasm_membase, i32 0, i32 2

    (i32.store offset=2 (i32.const 0) (i32.const 0))
;; CHECK: store i32 0, i32* bitcast (i8* getelementptr inbounds{{.*}}@.wasm_membase, i32 0, i32 2
    (i32.store offset=2 (get_local 0) (get_local 1))
;; TODO: test the logic for effective addr here. it should be the same as load, and may change soon

    (i64.store offset=3 (i32.const 0) (i64.const 0))
;; CHECK: store i64 0, i64* bitcast    (i8* getelementptr inbounds{{.*}}@.wasm_membase, i32 0, i32 3
    (i64.store8 offset=4 (i32.const 0) (i64.const 0x101))
;; CHECK: store i8 1, i8* getelementptr inbounds{{.*}}@.wasm_membase, i32 0, i32 4
    (i64.store16 offset=5 (i32.const 0) (i64.const 0x10001))
;; CHECK: store i16 1, i16* bitcast (i8* getelementptr inbounds{{.*}}@.wasm_membase, i32 0, i32 5
    (i64.store32 offset=6 (i32.const 0) (i64.const 0x100000001))
;; CHECK: store i32 1, i32* bitcast (i8* getelementptr inbounds{{.*}}@.wasm_membase, i32 0, i32 6
    (f32.store offset=7 (i32.const 0) (f32.const 1.0))
;; CHECK: store float 1.000000e+00, float* bitcast (i8* getelementptr inbounds{{.*}}@.wasm_membase, i32 0, i32 7
    (f64.store offset=8 (i32.const 0) (f64.const 2.0))
;; CHECK: store double 2.000000e+00, double* bitcast (i8* getelementptr inbounds{{.*}}@.wasm_membase, i32 0, i32 8

    ;; alignment must come after
    (i32.store8 offset=0 align=1 (i32.const 0) (i32.const 0))
    (i32.store16 offset=1 align=2 (i32.const 0) (i32.const 0))
    (i32.store offset=2 align=4 (i32.const 0) (i32.const 0))
    (i64.store offset=3 align=8 (i32.const 0) (i64.const 0))
    (i64.store8 offset=4 align=16 (i32.const 0) (i64.const 0))
    (i64.store16 offset=5 align=8 (i32.const 0) (i64.const 0))
    (i64.store32 offset=6 align=4 (i32.const 0) (i64.const 0))
    (f32.store offset=7 align=2 (i32.const 0) (f32.const 0))
    (f64.store offset=8 align=1 (i32.const 0) (f64.const 0))))
