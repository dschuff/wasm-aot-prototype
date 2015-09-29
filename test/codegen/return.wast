;; RUN: wat %s | FileCheck %s
(module

(func (return))
;; CHECK: define internal void @0
;; CHECK: ret void
(func (result i32)
;; CHECK: define internal i32 @1
  (return (i32.const 42)))
;; CHECK: ret i32 42
(func (result f64)
;; CHECK: define internal double @2
  (return (f64.const 0.1)))
;; CHECK: ret double 1.000000e-01

;; Implicit return
(func (result i32) (i32.const 1))
;; CHECK: define internal i32
;; ret i32 1
(func (result i32) (call 1))
;; CHECK: define internal i32
;; CHECK: %0 = call i32 @1
;; CHECK: ret i32 %0
)
