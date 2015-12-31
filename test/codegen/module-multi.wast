;; RUN: wat -S %s | FileCheck %s
(module (func $foo)
(export "foo" $foo))
;; CHECK: define internal void @"$foo
(module (func $foo)
(export "foo" $foo))
;; CHECK: define internal void @"$foo
