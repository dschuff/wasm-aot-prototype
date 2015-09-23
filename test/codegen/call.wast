;; RUN: waot %s | FileCheck %s
(module
 (func (param i32)
;; CHECK: define internal void @0
  (call 0 (i32.const 1)))
;; CHECK: call void @0(i32 1)
  (func (param i64) (call 1 (i64.const 2)))
;; CHECK: define internal void @1
;; CHECK: call void @1(i64 2)
  (func $foo (param $p1 f32) (call $foo (f32.const 0.333)))
;; CHECK: define internal void @"$foo"
;; CHECK: call void @"$foo"(float 0x3FD54FDF40000000)
  (func $bar (param f32 f64 i64 i32 i64 i64)
;; CHECK: define internal void @"$bar"
    (call $bar (f32.const 0)
;; CHECK: call void @"$bar"(
;; CHECK: float  0.000000e+00,
               (f64.const 6.283185307179586)
;; CHECK: double  0x401921FB54442D18,
               (i64.const -9223372036854775808)
;; CHECK: i64 -9223372036854775808
               (i32.const -1)
;; CHECK: i32 -1
               (i64.const -2)
;; CHECK: i64 -2
               (i64.const 18446744073709551615)
;; CHECK: i64 -1
     )
   )
)