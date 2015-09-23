;; RUN: waot %s | FileCheck %s
(module
;; CHECK: import.wast

;; unnamed
  (import "foo" "bar" (param i32) (result i64))
;; CHECK: declare i64 @"#foo#bar"(i32)

  ;; named
  (import $print_i32 "stdio" "print" (param i32))
;; CHECK: declare void @"#stdio#print"(i32)
  (import $add_i32 "math" "add" (param i32 i32) (result i32))
;; CHECK: declare i32 @"#math#add"(i32, i32)
  (import $f32 "test" "f32" (param f32) (result f32))
;; CHECK: declare float @"#test#f32"(float)
  (import $f64 "test" "f64" (param f64) (result f64))
;; CHECK: declare double @"#test#f64"(double)
  (import $i64 "test" "i64" (param i64) (result i64))
;; CHECK: declare i64 @"#test#i64"(i64)

;; call_import
  (func $foo (call_import 1 (i32.const 0)))
;; CHECK: call void @"#stdio#print"(i32 0)
  (func $bar (result f32) (call_import $f32 (f32.const 1.0)))
;; CHECK:  %0 = call float @"#test#f32"(float 1.000000e+00)
)
