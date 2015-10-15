;; RUN: wat -S %s | FileCheck %s
(module
 (func (param i32) (if (i32.const 1) (get_local 0)))
;; CHECK: br i1 true, label %if.then, label %if.else
;; CHECK: if.then:
;; CHECK: load
;; CHECK: br label %if.end
;; CHECK: if.else:
;; CHECK-NEXT: br label %if.end
;; CHECK: if.end:
;; CHECK: ret void

 (func (param i32) (result i32)
    (if (get_local 0)
      (return (i32.const 2))
      (return (i32.const 3))))
;; CHECK: %get_local = load i32
;; CHECK: %if_cmp = icmp ne i32 %get_local, 0
;; CHECK: br i1 %if_cmp, label %if.then, label %if.else
;; CHECK: if.then:
;; CHECK: ret i32 2
;; CHECK: if.else:
;; CHECK: ret i32 3
;; CHECK: if.end
;; CHECK: unreachable

 ;; The spec interpreter likes this but our parser complains of a type mismatch.
 ;;(func (result i32)
 ;; (if (i32.const 2)
 ;;   (return (i32.const 1)) (i32.const 3)))

 (func (param i64) (param i64) (result i64) (return
  (if (i32.const 1) (get_local 0)(get_local 1))))
;; CHECK: br i1 true, label %if.then, label %if.else
;; CHECK: if.then:
;; CHECK-NEXT: %get_local = load i64, i64* %arg
;; CHECK: if.else:
;; CHECK-NEXT: %get_local2 = load i64, i64* %arg1
;; CHECK: if.end:
;; CHECK-NEXT: %if.result = phi i64 [ %get_local, %if.then ], [ %get_local2, %if.else ]
;; CHECK-NEXT: ret i64 %if.result


 (func (param f32) (param f32) (result f32)
  (block
   (i64.const 2)
   (nop)
   (if (i32.const 0) (f64.const 1)(f64.const 2))
;; CHECK: br i1 false, label %if.then, label %if.else
;; CHECK: if.then
;; CHECK-NEXT: br label %if.end
;; CHECK: if.else
;; CHECK-NEXT: br label %if.end
;; CHECK: if.end
   (if (i32.const 1) (get_local 1) (get_local 0))
;; CHECK: br i1 true, label %if.then2, label %if.else3
;; CHECK: if.then2
;; CHECK: get_local
;; CHECK: if.else3
;; CHECK: get_local5
;; CHECK: if.end4:
;; CHECK: %if.result = phi float [ %get_local, %if.then2 ], [ %get_local5, %if.else3 ]
  )
 )


 (func (param i32) (param i32) (result i64)
  (if (get_local 0)
;; CHECK: %get_local = load i32, i32* %arg
;; CHECK: %if_cmp = icmp ne i32 %get_local, 0
;; CHECK: br i1 %if_cmp, label %if.then, label %if.else
;; CHECK: if.then:
    (if (get_local 1) (i64.const 1) (i64.const 2))
;; CHECK: get_local2 = load i32, i32* %arg1
;; CHECK: if_cmp3 =  icmp ne i32 %get_local2, 0
;; CHECK: br i1 %if_cmp3, label %if.then4, label %if.else5

;; CHECK: if.else:
;; CHECK-NEXT: br label %if.end
;; CHECK: if.end:
;; CHECK-NEXT: %if.result7 = phi i64 [ %if.result, %if.end6 ], [ 3, %if.else ]
;; CHECK-NEXT: ret i64 %if.result7
    (i64.const 3)
;; CHECK: if.then4:
;; CHECK-NEXT: br label %if.end6
;; CHECK: if.else5:
;; CHECK-NEXT: br label %if.end6
;; CHECK: if.end6
;; CHECK:  %if.result = phi i64 [ 1, %if.then4 ], [ 2, %if.else5 ]
;; CHECK-NEXT: br label %if.end
   )
 )
)
