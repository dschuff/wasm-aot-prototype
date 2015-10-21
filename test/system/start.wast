;; RUN: wac.py %s -o %t1
;; RUN: %t1 | FileCheck %s
(module
  (func $main (call_import 0 (i32.const 3)))
  (export "_start" 0)
  (import "stdio" "print" (param i32))
)

;; CHECK: 3