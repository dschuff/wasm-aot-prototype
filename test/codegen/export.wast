;; RUN: waot -S %s | FileCheck %s
(module
  (func (nop))
;; CHECK: @.export.nop = alias void ()* @0
  (export "nop" 0))
