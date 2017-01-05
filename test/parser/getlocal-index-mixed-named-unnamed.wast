;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: wast2wasm -d %t1 > %t2
;; RUN: wast2wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
(module
  (func (param i32) (param $n f32)
    (local i32 i64)
    (local $m f64)
    (drop (get_local 0))
    (drop (get_local 1))
    (drop (get_local $n)) ;; 1
    (drop (get_local 2))
    (drop (get_local 3))
    (drop (get_local $m)) ;; 4
    (drop (get_local 4))))
