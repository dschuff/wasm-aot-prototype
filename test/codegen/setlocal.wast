;; RUN: wat -S %s | FileCheck %s
(module
;; Unnamed local
 (func (local i32)
  (set_local 0 (i32.const 0)))
;; CHECK: define internal void @0()
;; CHECK: %local = alloca i32
;; CHECK: store i32 0, i32* %local

;; Unnamed arg
(func (param i32) (set_local 0 (i32.const 0)))
;; CHECK: define internal void @1(i32)
;; CHECK: %arg = alloca i32
;; CHECK: store i32 %0, i32* %arg
;; CHECK: store i32 0, i32* %arg

;; Named local
 (func
  (local $n i32)
  (set_local $n (i32.const 12)))
;; CHECK: define internal void @2()
;; CHECK: %"$n" = alloca i32
;; CHECK: store i32 12, i32* %"$n"

;; Named float arg
(func (param $n f32) (set_local $n (f32.const 1)))
;; CHECK: define internal void @3(float %"$n")
;; CHECK: %"$n1" = alloca float
;; CHECK: store float %"$n", float* %"$n1"
;; CHECK: store float 1.000000e+00, float* %"$n1"

;; set local with param
(func (param i32) (local i32) (set_local 1 (i32.const 0)))
;; CHECK: define internal void @4(i32)
;; CHECK: %arg = alloca i32
;; CHECK: %local = alloca i32
;; CHECK: store i32 %0, i32* %arg
;; CHECK: store i32 0, i32* %local

;; Mixed locals and params
(func (param i32) (param $n f32)
;; CHECK: define internal void @5(i32, float %"$n")
;; CHECK: %arg = alloca i32
;; CHECK: %"$n1" = alloca float
    (local i32 i64)
    (local $m f64)
;; CHECK: %local = alloca i32
;; CHECK: %local2 = alloca i64
;; CHECK: %"$m" = alloca double
;; CHECK: store i32 %0, i32* %arg
;; CHECK: store float %"$n", float* %"$n1"
    (set_local 0 (i32.const 0))
;; CHECK: store i32 0, i32* %arg
    (set_local 1 (f32.const 0))
;; CHECK: store float 0.000000e+00, float* %"$n1"
    (set_local $n (f32.const 0)) ;; 1
;; CHECK: store float 0.000000e+00, float* %"$n1"
    (set_local 2 (i32.const 0))
;; CHECK: store i32 0, i32* %local
    (set_local 3 (i64.const 0))
;; CHECK: store i64 0, i64* %local2
    (set_local $m (f64.const 0)) ;; 4
;; CHECK: store double 0.000000e+00, double* %"$m"
    (set_local 4 (f64.const 0)))
;; CHECK: store double 0.000000e+00, double* %"$m"
)