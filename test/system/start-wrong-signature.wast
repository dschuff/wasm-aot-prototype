;; RUN: not wac.py %s -o %t1 2>&1
(module
  (func $m (result i32) (return (i32.const 1)))
  (export "_start" 0)
  (import "stdio" "print" (param i32))
)

;; CHECK: export is not of type void ()*