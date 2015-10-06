;; RUN: wat -S %s | FileCheck %s
(module
;; Unnamed local
(func (local i32) (get_local 0))
;; CHECK: define internal void @0()
;; CHECK: %local = alloca i32
;; %get_local = load i32, i32* %local

;; Unnamed arg
(func (result i32) (param i32) (get_local 0))
;; CHECK: define internal i32 @1(i32)
;; CHECK: %arg = alloca i32
;; CHECK: store i32 %0, i32* %arg
;; CHECK: %get_local = load i32, i32* %arg
;; CHECK: ret i32 %get_local

;; Named local
(func (local $foo i32) (get_local $foo))
;; CHECK: define internal void @2()
;; CHECK: %"$foo" = alloca i32
;; CHECK: %get_local = load i32, i32* %"$foo"

;; Named float arg
(func (result f32) (param $n f32) (get_local $n))
;; CHECK: define internal float @3(float %"$n")
;; CHECK: %"$n1" = alloca float
;; CHECK: store float %"$n", float* %"$n1"
;; CHECK: %get_local = load float, float* %"$n1"
;; CHECK: ret float %get_local

;; get local with param
(func (param i32) (local i32) (get_local 1))
;; CHECK: define internal void @4(i32)
;; CHECK: %arg = alloca i32
;; CHECK: %local = alloca i32
;; CHECK: %get_local = load i32, i32* %local

;; Mixed locals and params
(func (result f32) (param i32) (param $n f32)
;; CHECK: define internal float @5(i32, float %"$n")
;; CHECK: %arg = alloca i32
;; CHECK: %"$n1" = alloca float
    (local i32 i64)
    (local $m f64)
;; CHECK: %local = alloca i32
;; CHECK: %local2 = alloca i64
;; CHECK: %"$m" = alloca double

;; CHECK: store i32 %0, i32* %arg
;; CHECK: store float %"$n", float* %"$n1"
    (get_local 0)
;; CHECK: load i32, i32* %arg
    (get_local 1)
;; CHECK: load float, float* %"$n1"
    (get_local 2)
;; CHECK: load i32, i32* %local
    (get_local 3)
;; CHECK: load i64, i64* %local2
    (get_local $m) ;; 4
;; CHECK: load double, double* %"$m"
    (get_local 4)
;; CHECK: load double, double* %"$m"
    (get_local $n) ;; 1
;; CHECK: %get_local8 = load float, float* %"$n1"
;; CHECK: ret float %get_local8
)
)
