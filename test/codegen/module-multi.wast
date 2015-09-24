;; RUN: waot -spec-test-script -S %s | FileCheck %s
;; Check that the -spec-test-script flag is required to accept this file.
;; RUN: not waot -S %s
(module (func $foo))
;; CHECK: define internal void @"$foo
(module (func $foo))
;; CHECK: define internal void @"$foo
