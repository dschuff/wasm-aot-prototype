;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: sexpr-wasm -d %t1 > %t2
;; RUN: sexpr-wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
(module
 (func (if (i32.const 1) (nop)))
 (func (if (i32.const 0) (nop) (nop)))
 (func (result i32)
    (if (i32.const 1)
      (return (i32.const 2))
      (return (i32.const 3))))
 ;; The spec interpreter likes this but our parser complains of a type mismatch.
 ;;(func (result i32)
 ;; (if (i32.const 2)
 ;;   (return (i32.const 1)) (i32.const 3)))
 (func (param i64) (param i64) (result i64) (return
  (if (i32.const 1) (get_local 0)(get_local 1))))
 (func (result i64) (return
  (if (i32.const 1) (i64.const 2)(i64.const 3))))
 (func (param f32) (param f32) (result f32)
  (block
   (i64.const 2)
   (nop)
   (if (i32.const 0) (f64.const 1)(f64.const 2))
   (if (i32.const 1) (get_local 1) (get_local 0))
  )
 )
 (func (param i32) (param i32) (result i64)
  (if (get_local 0)
    (if (get_local 1) (i64.const 1) (i64.const 2))
    (i64.const 3)
   )
 )
)
