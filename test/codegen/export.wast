;; RUN: wat -S %s | FileCheck %s
(module
  (func (nop))
;; CHECK: @.export.nop = alias void (), void ()* @0
  (export "nop" 0))
