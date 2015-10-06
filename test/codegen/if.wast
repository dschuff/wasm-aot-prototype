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

 (func (result i32) (param i32)
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

 (func (result i64) (param i64) (param i64) (return
  (if (i32.const 1) (get_local 0)(get_local 1))))
;; CHECK: br i1 true, label %if.then, label %if.else
;; CHECK: if.then:
;; CHECK-NEXT: %get_local = load i64, i64* %2
;; CHECK: if.else:
;; CHECK-NEXT: %get_local1 = load i64, i64* %3
;; CHECK: if.end:
;; CHECK-NEXT: %if.result = phi i64 [ %get_local, %if.then ], [ %get_local1, %if.else ]
;; CHECK-NEXT: ret i64 %if.result


 (func (result f32) (param f32) (param f32)
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
;; CHECK: br i1 true, label %if.then1, label %if.else2
;; CHECK: if.then1
;; CHECK: get_local
;; CHECK: if.else2
;; CHECK: get_local
;; CHECK: if.end3:
;; CHECK: %if.result = phi float [ %get_local, %if.then1 ], [ %get_local4, %if.else2 ]
  )
 )


 (func (result i64) (param i32) (param i32)
  (if (get_local 0)
;; CHECK: %get_local = load i32, i32* %2
;; CHECK: %if_cmp = icmp ne i32 %get_local, 0
;; CHECK: br i1 %if_cmp, label %if.then, label %if.else
;; CHECK: if.then:
    (if (get_local 1) (i64.const 1) (i64.const 2))
;; CHECK: get_local1 = load i32, i32* %3
;; CHECK: if_cmp2 =  icmp ne i32 %get_local1, 0
;; CHECK: br i1 %if_cmp2, label %if.then3, label %if.else4

;; CHECK: if.else:
;; CHECK-NEXT: br label %if.end
;; CHECK: if.end:
;; CHECK-NEXT: %if.result6 = phi i64 [ %if.result, %if.end5 ], [ 3, %if.else ]
;; CHECK-NEXT: ret i64 %if.result6
    (i64.const 3)
;; CHECK: if.then3:
;; CHECK-NEXT: br label %if.end5
;; CHECK: if.else4:
;; CHECK-NEXT: br label %if.end5
;; CHECK: if.end5
;; CHECK:  %if.result = phi i64 [ 1, %if.then3 ], [ 2, %if.else4 ]
;; CHECK-NEXT: br label %if.end
   )
 )
)
