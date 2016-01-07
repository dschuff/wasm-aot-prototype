;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump -spec-test-script %s > %t1
;; RUN: sexpr-wasm --spec %t1 > %t2
;; RUN : sexpr-wasm --spec  %s | diff - %t2 ;; TODO: investigate?
;; Test the type inference
;; RUN: sexpr_dump -spec-test-script -t %s | FileCheck %s
(module
(import "foo" "bar" (param i64)(result f32))
;; Check param expectation, void return expectation
 (func (param i32)
  (call 0 (i32.const 1)))
;; CHECK: [void->void](call 0 [i32->i32](i32.const

;; Check param expectation, void return expectation, void block-list expectation,
;; and passthrough last-block-expr expectation
 (func (param f64)
  (call 1 (block
;; CHECK: [void->void](call 1 [f64->f64](block
           (i32.const 1)
;; CHECK: [void->i32](i32.const
           (nop)
;; CHECK: [void->void](nop)
           (f64.const 2)))
;; CHECK: [f64->f64](f64.const
  (i32.const 1)
;; CHECK: [void->i32](i32.const
 )

;; check that type is passed through blocks/implicit blocks
 (func (param f64) (param i32) (result i64)
       (local f32) (local i64) (local f64) (local i32)
  (get_local 0)
;; CHECK: [void->f64](get_local 0)
  (get_local 1)
;; CHECK: [void->i32](get_local 1)
  (get_local 2)
;; CHECK: [void->f32](get_local 2)
  (get_local 3)
;; CHECK: [void->i64](get_local 3)
  (get_local 4)
;; CHECK: [void->f64](get_local 4)
  (get_local 5)
;; CHECK: [void->i32](get_local 5)
  (get_local 3)
;; CHECK: [i64->i64](get_local 3)
 )

;; check polymorphic return
(func(param f32) (result f32) (return (get_local 0)))
;; CHECK: [f32->f32](return
;; CHECK: [f32->f32](get_local

;; check set_local
(func (param i64) (param f64) (local f64)
 (set_local 1 (block (nop)(i32.const 1)(get_local 2)))
)
;; CHECK: [void->f64](set_local 1
;; CHECK: [f64->f64](block
;; CHECK: [void->void](nop)
;; CHECK: [void->i32](i32.const
;; CHECK: [f64->f64](get_local 2

;; call_import
(func (local i64) (call_import 0 (get_local 0)))
;; CHECK: [void->f32](call_import
;; CHECK: [i64->i64](get_local 0
(func $foo (param f64) (result f64) (f64.const 2))
(export "foo" $foo) ;; param f64

(func (i64.extend_u/i32 (i32.const 0)))
;; CHECK: i64.extend_u/i32 [i32->i32](i32.const 0

(func (f64.add (f64.const 4)(f64.const 5)))
;; CHECK: [f64->f64](f64.const
)
;; invoke
(invoke "foo" (f64.const 1.0))
;; CHECK: [f64->f64](f64.const
;; CHECK: assert_return

(assert_return_nan (invoke "foo" (f64.const 5)))
;; CHECK: [f64->f64](f64.const
