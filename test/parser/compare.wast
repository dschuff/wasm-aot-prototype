;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: sexpr-wasm -d %t1 > %t2
;; RUN: sexpr-wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
(module
  (func
    (i32.eq (i32.const 0) (i32.const 0))
    (i64.eq (i64.const 0) (i64.const 0))
    (f32.eq (f32.const 0) (f32.const 0))
    (f64.eq (f64.const 0) (f64.const 0))
    (i32.ne (i32.const 0) (i32.const 0))
    (i64.ne (i64.const 0) (i64.const 0))
    (f32.ne (f32.const 0) (f32.const 0))
    (f64.ne (f64.const 0) (f64.const 0))
    (i32.lt_s (i32.const 0) (i32.const 0))
    (i64.lt_s (i64.const 0) (i64.const 0))
    (i32.lt_u (i32.const 0) (i32.const 0))
    (i64.lt_u (i64.const 0) (i64.const 0))
    (f32.lt (f32.const 0) (f32.const 0))
    (f64.lt (f64.const 0) (f64.const 0))
    (i32.le_s (i32.const 0) (i32.const 0))
    (i64.le_s (i64.const 0) (i64.const 0))
    (i32.le_u (i32.const 0) (i32.const 0))
    (i64.le_u (i64.const 0) (i64.const 0))
    (f32.le (f32.const 0) (f32.const 0))
    (f64.le (f64.const 0) (f64.const 0))
    (i32.gt_s (i32.const 0) (i32.const 0))
    (i64.gt_s (i64.const 0) (i64.const 0))
    (i32.gt_u (i32.const 0) (i32.const 0))
    (i64.gt_u (i64.const 0) (i64.const 0))
    (f32.gt (f32.const 0) (f32.const 0))
    (f64.gt (f64.const 0) (f64.const 0))
    (i32.ge_s (i32.const 0) (i32.const 0))
    (i64.ge_s (i64.const 0) (i64.const 0))
    (i32.ge_u (i32.const 0) (i32.const 0))
    (i64.ge_u (i64.const 0) (i64.const 0))
    (f32.ge (f32.const 0) (f32.const 0))
    (f64.ge (f64.const 0) (f64.const 0))))