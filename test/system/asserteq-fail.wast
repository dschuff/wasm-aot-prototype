;; RUN: wac.py --spec-test-script %s -o %t1
;; RUN: not %t1 2>&1 | FileCheck %s

(module
  (func $foo (result i32) (i32.const 0))
  (export "foo" $foo)

  (func $bar (param f32) (result f32) (f32.const 1.0))
  (export "bar" $bar)
  (func $baz (param i64) (result i64) (get_local 0))
  (export "baz" $baz)
  (func $quux (param f64) (result f64) (get_local 0))
  (export "quux" $quux)
)

(assert_eq (invoke "foo") (i32.const 0))

;; Compares to 2, gets 1.
(assert_eq (invoke "bar" (f32.const 0)) (f32.const 2))
;; CHECK: failure in assert_eq expression 2: expected 2.000000, got 1

(assert_eq (invoke "bar" (f32.const 3)) (f32.const 6))
;; CHECK: failure in assert_eq expression 3: expected 6.000000, got 1

(assert_eq (invoke "baz" (i64.const 123456789)) (i64.const 123456789))
;; Should succeed
(assert_eq (invoke "baz" (i64.const 123456789)) (i64.const 1234567890))
;; CHECK: failure in assert_eq expression 5: expected 1234567890, got 123456789

(assert_eq (invoke "quux" (f64.const 0.123456789)) (f64.const 0.123456789))
;; Should succeed
(assert_eq (invoke "quux" (f64.const 0x1.123456789p-7)) (f64.const 0x1.091a2b3c48p-6))
;; CHECK: failure in assert_eq expression 7: expected 0x1.091a2b3c48p-6, got 0x1.123456789p-7
