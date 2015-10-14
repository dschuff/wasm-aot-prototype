;; RUN: wat -S %s | FileCheck %s
(module
  (func (local i32)(local i64)(local f32)(local f64)
    (f32.reinterpret/i32 (get_local 0))
;; CHECK: bitcast i32 %get_local to float
    (i32.reinterpret/f32 (get_local 2))
;; CHECK: bitcast float %get_local{{.+}} to i32
    (f64.reinterpret/i64 (get_local 1))
;; CHECK: bitcast i64 %get_local{{.*}} to double
    (i64.reinterpret/f64 (get_local 3))))
;; CHECK: bitcast double %get_local{{.*}} to i64
