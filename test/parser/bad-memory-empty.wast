;; RUN: not sexpr_dump %s 2>&1 | FileCheck %s
;; CHECK: bad-memory-empty.wast:3:16: syntax error, unexpected ), expecting INT
(module (memory))


