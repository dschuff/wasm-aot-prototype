;; RUN: wac.py --spec-test-script %s -o %t1
;; RUN: %t1
(module
 (func $noelse (result i32) (param i32) (param i32)
   (if (get_local 0) (return (get_local 0)))
   (get_local 1))
 (export "noelse" $noelse)

 (func $returns (result i32) (param i32)
    (if (get_local 0)
      (return (i32.const 2))
      (return (i32.const 3))))
 (export "returns" $returns)

 ;; The spec interpreter likes this but our parser complains of a type mismatch.
 ;;(func (result i32)
 ;; (if (i32.const 2)
 ;;   (return (i32.const 1)) (i32.const 3)))

 (func $getlocals (result i64) (param i32) (param i64) (param i64) (return
  (if (get_local 0) (get_local 1)(get_local 2))))
 (export "getlocals" $getlocals)

 (func $block (result f32) (param f32) (param f32) (param i32)
  (block
   (i64.const 2)
   (nop)
   (if (i32.const 0) (f64.const 1)(f64.const 2))
   (if (get_local 2) (get_local 1) (get_local 0))
  )
 )
 (export "block" $block)

 (func $nested (result i64) (param i32) (param i32)
  (if (get_local 0)
    (if (get_local 1) (return (i64.const 1)) (return (i64.const 2)))
   )
  (return (i64.const 3))
 )
 (export "nested" $nested)
)

(assert_eq (invoke "noelse" (i32.const -1) (i32.const 3)) (i32.const -1))
(assert_eq (invoke "noelse" (i32.const 1) (i32.const 55)) (i32.const 1))
(assert_eq (invoke "noelse" (i32.const 0) (i32.const 55)) (i32.const 55))

(assert_eq (invoke "returns" (i32.const 1)) (i32.const 2))
(assert_eq (invoke "returns" (i32.const 0)) (i32.const 3))

(assert_eq (invoke "getlocals" (i32.const 1) (i64.const 55) (i64.const 2)) (i64.const 55))
(assert_eq (invoke "getlocals" (i32.const 0) (i64.const 55) (i64.const 2)) (i64.const 2))

(assert_eq (invoke "block" (f32.const 3)(f32.const 4)(i32.const 1)) (f32.const 4))
(assert_eq (invoke "block" (f32.const 3)(f32.const 4)(i32.const 0)) (f32.const 3))

(assert_eq (invoke "nested" (i32.const 0) (i32.const 0)) (i64.const 3))
(assert_eq (invoke "nested" (i32.const 1) (i32.const 0)) (i64.const 2))
(assert_eq (invoke "nested" (i32.const 0) (i32.const 1)) (i64.const 3))
(assert_eq (invoke "nested" (i32.const 1) (i32.const 1)) (i64.const 1))
