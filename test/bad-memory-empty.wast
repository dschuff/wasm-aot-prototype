;; RUN: not sexpr-dump %s 2>&1 | FileCheck %s
(module (memory))
;; CHECK: bad-memory-empty.wast:2:16: expected ATOM, not ")"
