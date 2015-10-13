;; RUN: wac.py --spec-test-script %s -o %t1
;; RUN: %t1
(module
  (func $foo (result i32) (i32.const 0))
  (export "foo" $foo)
  (func $bar (param f32) (result f32) (get_local 0))
  (export "bar" $bar)
  (func $ret_arg (param f64) (result f64)(get_local 0))
  (export "ret_arg" $ret_arg)
)

(assert_return (invoke "foo") (i32.const 0))

(assert_return (invoke "bar" (f32.const 0)) (f32.const 0))

;; ok to use more complex exprs
(assert_return
  (invoke "bar"
    (block (f32.const 1) (f32.const 10)))
  (f32.const 10))

(assert_return_nan (invoke "ret_arg" (f64.const nan)))
(assert_return_nan (invoke "ret_arg" (f64.const -nan)))
(assert_return_nan (invoke "bar" (f32.const -nan)))
