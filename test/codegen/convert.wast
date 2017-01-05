;; RUN: wat -S %s | FileCheck %s
(module
  (func (local i32)(local i64)(local f32)(local f64)
    (drop (i32.wrap/i64 (get_local 1)))
;; CHECK: trunc i64 %get_local to i32
    (drop (i64.extend_u/i32 (get_local 0)))
;; CHECK: zext i32 %get_local{{.+}} to i64
    (drop (i64.extend_s/i32 (get_local 0)))
;; CHECK: sext i32 %get_local{{.+}} to i64
    (drop (i32.trunc_s/f32 (get_local 2)))
;; CHECK: fptosi float %get_local{{.+}} to i32
    (drop (i32.trunc_u/f32 (get_local 2)))
;; CHECK: fptoui float %get_local{{.+}} to i32
    (drop (i32.trunc_s/f64 (get_local 3)))
;; CHECK: fptosi double %get_local{{.+}} to i32
    (drop (i32.trunc_u/f64 (get_local 3)))
;; CHECK: fptoui double %get_local{{.+}} to i32
    (drop (i64.trunc_s/f32 (get_local 2)))
;; CHECK: fptosi float %get_local{{.+}} to i64
    (drop (i64.trunc_u/f32 (get_local 2)))
;; CHECK: fptoui float %get_local{{.+}} to i64
    (drop (i64.trunc_s/f64 (get_local 3)))
;; CHECK: fptosi double %get_local{{.+}} to i64
    (drop (i64.trunc_u/f64 (get_local 3)))
;; CHECK fptoui double %get_local{{.+}} to i64
    (drop (f32.convert_s/i32 (get_local 0)))
;; CHECK: sitofp i32 %get_local{{.+}} to float
    (drop (f32.convert_u/i32 (get_local 0)))
;; CHECK: uitofp i32 %get_local{{.+}} to float
    (drop (f32.convert_s/i64 (get_local 1)))
;; CHECK: sitofp i64 %get_local{{.+}} to float
    (drop (f32.convert_u/i64 (get_local 1)))
;; CHECK: uitofp i64 %get_local{{.+}} to float
    (drop (f64.convert_s/i32 (get_local 0)))
;; CHECK: sitofp i32 %get_local{{.+}} to double
    (drop (f64.convert_u/i32 (get_local 0)))
;; CHECK: uitofp i32 %get_local{{.+}} to double
    (drop (f64.convert_s/i64 (get_local 1)))
;; CHECK: sitofp i64 %get_local{{.+}} to double
    (drop (f64.convert_u/i64 (get_local 1)))
;; CHECK: uitofp i64 %get_local{{.+}} to double
    (drop (f32.demote/f64 (get_local 3)))
;; CHECK: fptrunc double %get_local{{.+}} to float
    (drop (f64.promote/f32 (get_local 2)))))
;; CHECK: fpext float %get_local{{.+}} to double
