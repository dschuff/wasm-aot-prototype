;; RUN: waot -S %s | FileCheck %s
(module
;; CHECK: function.wast
 (func )
;; CHECK: define internal void @0()
 (func $foo)
;; CHECK: define internal void @"$foo"
 (func (param i32 f32 i64 f64))
;; CHECK: define internal void @1(i32, float, i64, double)
 (func (param $foo i32))
;; CHECK: define internal void @2(i32 
)
