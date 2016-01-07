;; RUN: wac.py --spec-test-script %s -o %t1
;; RUN: %t1
(module
 (func $noelse (param i32) (param i32) (result i32)
   (if (get_local 0) (return (get_local 0)))
   (get_local 1))
 (export "noelse" $noelse)

 (func $returns (param i32) (result i32)
    (if_else (get_local 0)
      (return (i32.const 2))
      (return (i32.const 3))))
 (export "returns" $returns)

 ;; This test previously disabled because of parser, now because of codgen.
 ;; (func (result i32)
 ;; (if_else (i32.const 2)
 ;;   (return (i32.const 1)) (i32.const 3)))

 (func $getlocals (param i32) (param i64) (param i64) (result i64) (return
  (if_else (get_local 0) (get_local 1)(get_local 2))))
 (export "getlocals" $getlocals)

 (func $block (param f32) (param f32) (param i32) (result f32)
  (block
   (i64.const 2)
   (nop)
   (if_else (i32.const 0) (f64.const 1)(f64.const 2))
   (if_else (get_local 2) (get_local 1) (get_local 0))
  )
 )
 (export "block" $block)

 (func $nested (param i32) (param i32) (result i64)
  (if (get_local 0)
    (if_else (get_local 1) (return (i64.const 1)) (return (i64.const 2)))
   )
  (return (i64.const 3))
 )
 (export "nested" $nested)
)

(assert_return (invoke "noelse" (i32.const -1) (i32.const 3)) (i32.const -1))
(assert_return (invoke "noelse" (i32.const 1) (i32.const 55)) (i32.const 1))
(assert_return (invoke "noelse" (i32.const 0) (i32.const 55)) (i32.const 55))

(assert_return (invoke "returns" (i32.const 1)) (i32.const 2))
(assert_return (invoke "returns" (i32.const 0)) (i32.const 3))

(assert_return (invoke "getlocals" (i32.const 1) (i64.const 55) (i64.const 2)) (i64.const 55))
(assert_return (invoke "getlocals" (i32.const 0) (i64.const 55) (i64.const 2)) (i64.const 2))

(assert_return (invoke "block" (f32.const 3)(f32.const 4)(i32.const 1)) (f32.const 4))
(assert_return (invoke "block" (f32.const 3)(f32.const 4)(i32.const 0)) (f32.const 3))

(assert_return (invoke "nested" (i32.const 0) (i32.const 0)) (i64.const 3))
(assert_return (invoke "nested" (i32.const 1) (i32.const 0)) (i64.const 2))
(assert_return (invoke "nested" (i32.const 0) (i32.const 1)) (i64.const 3))
(assert_return (invoke "nested" (i32.const 1) (i32.const 1)) (i64.const 1))
