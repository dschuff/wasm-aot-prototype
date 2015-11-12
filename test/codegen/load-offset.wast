;; RUN: wat -S %s | FileCheck %s

(module
  (func (local i32)
    (i32.load offset=0 (i32.const 0))
;; CHECK: load i32, i32* bitcast ({{.*}}@.wasm_membase to i32*)
    (i32.load offset=0 (i32.const 5))
;; CHECK: load i32, i32* bitcast (i8* getelementptr inbounds ({{.*}}@.wasm_membase, i32 0, i32 5) to i32*)
    (i32.load offset=0 (get_local 0))
;; CHECK: get_local = load i32, i32* %local
;; CHECK: %effective_addr = add i32 %get_local, 0
;; CHECK: %0 = getelementptr i8, i8* getelementptr inbounds ({{.*}}@.wasm_membase, i32 0, i32 0), i32 %effective_addr
;; CHECK: %1 = bitcast i8* %0 to i32*
;; CHECK: load i32, i32* %1


    (i64.load offset=1 (i32.const 3))
;; CHECK: load i64, i64* bitcast (i8* getelementptr{{.*}}@.wasm_membase, i32 0, i32 4) to i64
    (i64.load offset=1 (get_local 0))
;; CHECK: load i32, i32* %local
;; CHECK: add i32 %get_local{{[0-9]}}, 1
;; CHECK: load i64

    (i64.load8_s offset=2 (i32.const 0))
;; CHECK: %[[V1:.+]] = load i8,
;; CHECK: sext i8 %[[V1]] to i64
    (i64.load16_s offset=3 (i32.const 0))
;; CHECK: %[[V1:.+]] = load i16,
;; CHECK: sext i16 %[[V1]] to i64
    (i64.load32_s offset=4 (i32.const 0))
;; CHECK: %[[V1:.+]] = load i32,
;; CHECK: sext i32 %[[V1]] to i64
    (i64.load8_u offset=5 (i32.const 0))
;; CHECK: %[[V1:.+]] = load i8,
;; CHECK: zext i8 %[[V1]] to i64
    (i64.load16_u offset=6 (i32.const 0))
;; CHECK: %[[V1:.+]] = load i16,
;; CHECK: zext i16 %[[V1]] to i64
    (i64.load32_u offset=7 (i32.const 0))
;; CHECK: %[[V1:.+]] = load i32,
;; CHECK: zext i32 %[[V1]] to i64
    (i32.load8_s offset=8 (i32.const 0))
;; CHECK: %[[V1:.+]] = load i8,
;; CHECK: sext i8 %[[V1]] to i32
    (i32.load16_s offset=9 (i32.const 0))
;; CHECK: %[[V1:.+]] = load i16,
;; CHECK: sext i16 %[[V1]] to i32
    (i32.load8_u offset=10 (i32.const 0))
;; CHECK: %[[V1:.+]] = load i8,
;; CHECK: zext i8 %[[V1]] to i32
    (i32.load16_u offset=11 (i32.const 0))
;; CHECK: %[[V1:.+]] = load i16,
;; CHECK: zext i16 %[[V1]] to i32
    (f32.load offset=12 (i32.const 0))
;; CHECK: load float,
    (f64.load offset=13 (i32.const 0))
;; CHECK: load double

    (i32.load offset=0 align=1 (i32.const 0))
    (i64.load offset=1 align=2 (i32.const 0))
    (i64.load8_s offset=2 align=4 (i32.const 0))
    (i64.load16_s offset=3 align=8 (i32.const 0))
    (i64.load32_s offset=4 align=16 (i32.const 0))
    (i64.load8_u offset=5 align=32 (i32.const 0))
    (i64.load16_u offset=6 align=64 (i32.const 0))
    (i64.load32_u offset=7 align=128 (i32.const 0))
    (i32.load8_s offset=8 align=64 (i32.const 0))
    (i32.load16_s offset=9 align=32 (i32.const 0))
    (i32.load8_u offset=10 align=16 (i32.const 0))
    (i32.load16_u offset=11 align=8 (i32.const 0))
    (f32.load offset=12 align=4 (i32.const 0))
    (f64.load offset=13 align=2 (i32.const 0))))
