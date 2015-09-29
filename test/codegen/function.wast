;; RUN: wat -S %s | FileCheck %s
(module

;; Unnamed function
 (func )
;; CHECK: define internal void @0()
;; Named function
 (func $foo)
;; CHECK: define internal void @"$foo"

;; Result type
 (func $result (result f64)(f64.const 0))
;; CHECK: define internal double @"$result"()

;; Function arg types and names
 (func $argtypes (param i32 f32 i64 f64))
;; CHECK: define internal void @"$argtypes"(i32, float, i64, double)
 (func $namedarg (param $foo i32))
;; CHECK: define internal void @"$namedarg"(i32 %"$foo")
 (func $mixednames (param i64)(param $bar f32)(param f64))
;; CHECK: define internal void @"$mixednames"(i64, float %"$bar", double)

;; Locals
 (func $local (local i32 f64))
;; CHECK-LABEL: $local
;; CHECK: alloca i32
;; CHECK: alloca double
 (func $namedlocal (local $foo i32) (local $bar i64))
;; CHECK-LABEL: $namedlocal
;; CHECK: %"$foo" = alloca i32
;; CHECK: %"$bar" = alloca i64
)