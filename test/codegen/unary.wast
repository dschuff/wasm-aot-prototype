;; RUN: wat -S %s | FileCheck %s
(module
  (func (param f32) (param f64)
    (f32.neg (get_local 0))
;; CHECK: fsub float -0.000000e+00, %get_local
    (f64.neg (get_local 1))
;; CHECK: fsub double -0.000000e+00, %get_local
    (f32.abs (f32.const 0))
;; CHECK: call float @llvm.fabs.f32
    (f64.abs (f64.const 0))
;; CHECK: call double @llvm.fabs.f64
    (f32.sqrt (f32.const 0))
;; CHECK: call float @llvm.sqrt.f32
    (f64.sqrt (f64.const 0))
;; CHECK: call double @llvm.sqrt.f64
    (i32.clz (i32.const 0))
;; CHECK: call i32 @llvm.ctlz.i32(i32 0, i1 false)
    (i64.clz (i64.const 0))
;; CHECK: call i64 @llvm.ctlz.i64(i64 0, i1 false)
    (i32.ctz (i32.const 0))
;; CHECK: call i32 @llvm.cttz.i32(i32 0, i1 false)
    (i64.ctz (i64.const 0))
;; CHECK: call i64 @llvm.cttz.i64(i64 0, i1 false)
    (i32.popcnt (i32.const 0))
;; CHECK: call i32 @llvm.ctpop.i32(i32 0)
    (i64.popcnt (i64.const 0))
;; CHECK: call i64 @llvm.ctpop.i64(i64 0)
    (f32.ceil (f32.const 0))
;; CHECK: call float @llvm.ceil.f32
    (f64.ceil (f64.const 0))
;; CHECK: call double @llvm.ceil.f64
    (f32.floor (f32.const 0))
;; CHECK: call float @llvm.floor.f32
    (f64.floor (f64.const 0))
;; CHECK: call double @llvm.floor.f64
    (f32.trunc (f32.const 0))
;; CHECK: call float @llvm.trunc.f32
    (f64.trunc (f64.const 0))
;; CHECK: call double @llvm.trunc.f64
    (f32.nearest (f32.const 0))
;; CHECK: call float @llvm.rint.f32
    (f64.nearest (f64.const 0))))
;; CHECK: call double @llvm.rint.f64