;; RUN: wac.py --spec-test-script %s -o %t1
;; RUN: not --crash %t1

(module
  (func $foo (result i32) (i32.const 0))
  (export "foo" $foo)
;; TODO: make this return its argument
  (func $bar (param f32) (result f32) (f32.const 1.0))
  (export "bar" $bar))

(assert_eq (invoke "foo") (i32.const 0))

;; Compares to 2, gets 1.
;; TODO: make better error behavior than trap, so we can test multiple failures.
(assert_eq (invoke "bar" (f32.const 0)) (f32.const 2))
