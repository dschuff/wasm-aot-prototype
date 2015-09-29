;; RUN: wac.py --spec-test-script %s -o %t1
;; RUN: %t1
(module
  (func $foo (result i32) (i32.const 0))
  (export "foo" $foo)
;; TODO: make this return its argument
  (func $bar (param f32) (result f32) (f32.const 1.0))
  (export "bar" $bar))

(assert_eq (invoke "foo") (i32.const 0))

(assert_eq (invoke "bar" (f32.const 0)) (f32.const 1))

;; ok to use more complex exprs
(assert_eq
  (invoke "bar"
    (block (f32.const 1) (f32.const 10)))
  (f32.const 1))
