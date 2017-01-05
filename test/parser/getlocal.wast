;; Test that the binary encoding of the dump matches that of the original
;; RUN: sexpr_dump %s > %t1
;; RUN: wast2wasm -d %t1 > %t2
;; RUN: wast2wasm -d %s | diff - %t2
;; Test that round-tripping is stable
;; RUN: sexpr_dump %t1 | diff %t1 -
(module
 (func (local i32) (drop (get_local 0)))
 (func (local i32) (local i64) (drop (get_local 1))(drop (get_local 0)))
)
