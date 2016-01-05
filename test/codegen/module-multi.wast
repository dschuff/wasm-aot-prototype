;; RUN: wat -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RUN: not wat -S %s
(module (func $foo)
(export "foo" $foo))
;; CHECK: define internal void @"$foo
(module (func $foo)
(export "foo" $foo))
;; CHECK: define internal void @"$foo
