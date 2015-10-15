;; RUN: wac.py --spec-test-script %s -o %t1
;; RUN: %t1
(module
  (func $return-none (i32.const 1))
  (export "return-none" $return-none)
)

(assert_return (invoke "return-none"))
