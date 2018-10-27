(in-package :cl-user)
(defpackage cl3a.mvmult_vop
  (:use :cl :sb-ext :sb-c :sb-vm)
  (:export :mvi2x4-pd :take-row-vector-pd))
(in-package :cl3a.mvmult_vop)


(defknown mvi2x4-pd (fixnum
                     (simple-array double-float (*))
                     (simple-array double-float (*))
                     (simple-array double-float (*))
                     (sb-kernel:simd-pack double-float))
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown take-row-vector-pd
    ((simple-array double-float (*))
     fixnum fixnum
     (simple-array double-float (*)))
    (simple-array double-float (*))
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown test
    ((simple-array double-float (*))
     (simple-array double-float (*)))
    (simple-array double-float (*))
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)


(in-package :sb-vm)

(define-vop (cl3a.mvmult_vop::mvi2x4-pd)
  (:translate cl3a.mvmult_vop::mvi2x4-pd)
  (:policy :fast-safe)
  (:args (k-ptr :scs (signed-reg) :target p)
         (va0 :scs (descriptor-reg))
         (va1 :scs (descriptor-reg))
         (vb :scs (descriptor-reg))
         (vci :scs (double-sse-reg) :target xmm6))
  (:arg-types tagged-num
              simple-array-double-float simple-array-double-float
              simple-array-double-float simd-pack-double)
  (:temporary (:sc signed-reg :from (:argument 0)) k)
  (:temporary (:sc signed-reg :offset rcx-offset) p)
  (:temporary (:sc double-sse-reg :from (:argument 1)) xmm0)
  (:temporary (:sc double-sse-reg :from (:argument 1)) xmm1)
  (:temporary (:sc double-sse-reg :from (:argument 2)) xmm2)
  (:temporary (:sc double-sse-reg :from (:argument 2)) xmm3)
  (:temporary (:sc double-sse-reg :from (:argument 3)) xmm4)
  (:temporary (:sc double-sse-reg :from (:argument 3)) xmm5)
  (:results (xmm6 :scs (double-sse-reg) :from (:argument 4)))
  (:result-types simd-pack-double)
  (:generator
   34
   (move k k-ptr)
   (inst xor p p)
   (inst movupd xmm6 vci)
   LOOP
   (inst movupd xmm0
         (make-ea-for-float-ref
          va0 p 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd xmm1
         (make-ea-for-float-ref
          va0 p 2 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd xmm2
         (make-ea-for-float-ref
          va1 p 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd xmm3
         (make-ea-for-float-ref
          va1 p 2 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd xmm4
         (make-ea-for-float-ref
          vb p 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd xmm5
         (make-ea-for-float-ref
          vb p 2 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst mulpd xmm0 xmm4)
   (inst mulpd xmm1 xmm5)
   (inst mulpd xmm2 xmm4)
   (inst mulpd xmm3 xmm5)
   (inst addpd xmm0 xmm1)
   (inst addpd xmm2 xmm3)
   (inst haddpd xmm0 xmm0)
   (inst haddpd xmm2 xmm2)
   (inst unpckhpd xmm0 xmm2)
   (inst addpd xmm6 xmm0)
   (inst add p 4)
   (inst cmp p k)
   (inst jmp :b LOOP)
   DONE))

(define-vop (cl3a.mvmult_vop::take-row-vector-pd)
  (:translate cl3a.mvmult_vop::take-row-vector-pd)
  (:policy :fast-safe)
  (:args (src :scs (descriptor-reg))
         (rmi :scs (signed-reg))
         (k :scs (signed-reg))
         (dst :scs (descriptor-reg)))
  (:arg-types simple-array-double-float
              tagged-num tagged-num
              simple-array-double-float)
  (:temporary (:sc signed-reg :from (:argument 1)) p-src)
  (:temporary (:sc signed-reg) p-dst)
  (:temporary (:sc double-sse-reg) xmm0)
  (:temporary (:sc double-sse-reg) xmm1)
  (:results (r-dst :scs (descriptor-reg)))
  (:result-types simple-array-double-float)
  (:generator
   10
   (move p-src rmi)
   (inst xor p-dst p-dst)
   LOOP
   (inst movupd
         xmm0
         (make-ea-for-float-ref
          src p-src 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd
         xmm1
         (make-ea-for-float-ref
          src p-src 2 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd
         (make-ea-for-float-ref
          dst p-dst 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
         xmm0)
   (inst movupd
         (make-ea-for-float-ref
          dst p-dst 2 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
         xmm1)
   (inst add p-src 4)
   (inst add p-dst 4)
   (inst cmp p-dst k)
   (inst jmp :b LOOP)
   (move r-dst dst)
   DONE))

(define-vop (cl3a.mvmult_vop::test)
  (:translate cl3a.mvmult_vop::test)
  (:policy :fast-safe)
  (:args (src :scs (descriptor-reg))
         (dst :scs (descriptor-reg)))
  (:arg-types simple-array-double-float simple-array-double-float)
  (:results (r-dst :scs (descriptor-reg)))
  (:result-types simple-array-double-float)
  (:generator
   5
   (move dst src)
   (move r-dst dst)
   (inst add r-dst n-fixnum-tag-bits)))

(in-package :cl3a.mvmult_vop)


(defun mvi2x4-pd (k va0 va1 vb vci)
  (mvi2x4-pd k va0 va1 vb vci))

(defun take-row-vector-pd (src rmi k dst)
  (take-row-vector-pd src rmi k dst))

(defun test (src dst)
  (test src dst))
