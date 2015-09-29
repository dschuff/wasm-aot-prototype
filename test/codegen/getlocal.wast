;; RUN: wat -S %s | FileCheck %s
(module
;; Unnamed local
(func (local i32) (get_local 0))
;; CHECK: define internal void @0()
;; CHECK: %0 = alloca i32
;; %get_local = load i32, i32* %0

;; Unnamed arg
(func (result i32) (param i32) (get_local 0))
;; CHECK: define internal i32 @1(i32)
;; CHECK: ret i32 %0

;; Named local
(func (local $foo i32) (get_local $foo))
;; CHECK: define internal void @2()
;; CHECK: %"$foo" = alloca i32
;; CHECK: %get_local = load i32, i32* %"$foo"

;; Named float arg
(func (result f32) (param $n f32) (get_local $n))
;; CHECK: define internal float @3(float %"$n")
;; CHECK: ret float %"$n"

;; get local with param
(func (param i32) (local i32) (get_local 1))
;; CHECK: define internal void @4(i32)
;; CHECK: %1 = alloca i32
;; CHECK: %get_local = load i32, i32* %1

;; Mixed locals and params
(func (result f32) (param i32) (param $n f32)
;; CHECK: define internal float @5(i32, float %"$n")
    (local i32 i64)
;; CHECK: %1 = alloca i32
;; CHECK: %2 = alloca i64
    (local $m f64)
;; CHECK: %"$m" = alloca double
    (get_local 0)
;; nothing here because the value is unused
    (get_local 1)
;; TODO: use this value
    (get_local 2)
;; CHECK-NEXT: load i32, i32* %1
    (get_local 3)
;; CHECK: load i64, i64* %2
    (get_local $m) ;; 4
;; CHECK: load double, double* %"$m"
    (get_local 4)
;; CHECK: load double, double* %"$m"
    (get_local $n) ;; 1
;; CHECK: ret float %"$n"
)
)