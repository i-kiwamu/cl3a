(in-package :cl-user)
(defpackage cl3a.mvmult_vop
  (:use :cl :sb-ext :sb-c :sb-vm)
  (:export :mvi2x4-pd :mvi2x8-pd))
(in-package :cl3a.mvmult_vop)


(defknown mvi2x4-pd
    (fixnum fixnum fixnum
     (simple-array double-float (*))
     (simple-array double-float (*))
     (sb-kernel:simd-pack double-float))
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown mvi2x8-pd
    (fixnum fixnum fixnum
     (simple-array double-float (*))
     (simple-array double-float (*)))
    (sb-ext:simd-pack-256 double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)


(in-package :sb-vm)

(define-vop (cl3a.mvmult_vop::mvi2x4-pd)
  (:translate cl3a.mvmult_vop::mvi2x4-pd)
  (:policy :fast-safe)
  (:args (k-ptr :scs (signed-reg) :target i1)
         (k0-ptr :scs (signed-reg) :target p)
         (i-ptr :scs (signed-reg))
         (va :scs (descriptor-reg))
         (vb :scs (descriptor-reg))
         (vci :scs (double-sse-reg) :target xmm6))
  (:arg-types tagged-num tagged-num tagged-num
              simple-array-double-float simple-array-double-float
              simd-pack-double)
  (:temporary (:sc signed-reg :from (:argument 0)) k)
  (:temporary (:sc signed-reg :from (:argument 1)) k0)
  (:temporary (:sc signed-reg :from (:argument 2)) i0)
  (:temporary (:sc signed-reg :from (:argument 2)) i1)
  (:temporary (:sc signed-reg :offset rcx-offset) p)
  (:temporary (:sc double-sse-reg :from (:argument 3)) xmm0)
  (:temporary (:sc double-sse-reg :from (:argument 3)) xmm1)
  (:temporary (:sc double-sse-reg :from (:argument 4)) xmm2)
  (:temporary (:sc double-sse-reg :from (:argument 4)) xmm3)
  (:temporary (:sc double-sse-reg :from (:argument 5)) xmm4)
  (:temporary (:sc double-sse-reg :from (:argument 5)) xmm5)
  (:results (xmm6 :scs (double-sse-reg) :from (:argument 6)))
  (:result-types simd-pack-double)
  (:generator
   34
   (move k k-ptr)
   (move k0 k0-ptr)
   (move i0 i-ptr)
   (move i1 i-ptr)
   (inst add i1 k)
   (inst xor p p)
   (inst movupd xmm6 vci)
   LOOP
   (inst movupd xmm0
         (make-ea-for-float-ref
          va i0 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd xmm1
         (make-ea-for-float-ref
          va i0 2 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd xmm2
         (make-ea-for-float-ref
          va i1 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd xmm3
         (make-ea-for-float-ref
          va i1 2 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
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
   (inst add i0 4)
   (inst add i1 4)
   (inst add p 4)
   (inst cmp p k0)
   (inst jmp :b LOOP)
   DONE))

(define-vop (cl3a.mvmult_vop::mvi2x8-pd)
  (:translate cl3a.mvmult_vop::mvi2x8-pd)
  (:policy :fast-safe)
  (:args (k-ptr :scs (signed-reg) :target i1)
         (k0-ptr :scs (signed-reg) :target p)
         (i-ptr :scs (signed-reg))
         (va :scs (descriptor-reg))
         (vb :scs (descriptor-reg)))
  (:arg-types tagged-num tagged-num tagged-num
              simple-array-double-float simple-array-double-float)
  (:temporary (:sc signed-reg :from (:argument 0)) k)
  (:temporary (:sc signed-reg :from (:argument 1)) k0)
  (:temporary (:sc signed-reg :from (:argument 2)) i0)
  (:temporary (:sc signed-reg :from (:argument 2)) i1)
  (:temporary (:sc signed-reg :offset rcx-offset) p)
  (:temporary (:sc double-avx2-reg :from (:argument 3)) ymm0)
  (:temporary (:sc double-avx2-reg :from (:argument 3)) ymm1)
  (:temporary (:sc double-avx2-reg :from (:argument 4)) ymm2)
  (:temporary (:sc double-avx2-reg :from (:argument 4)) ymm3)
  (:temporary (:sc double-avx2-reg :from (:argument 5)) ymm4)
  (:temporary (:sc double-avx2-reg :from (:argument 5)) ymm5)
  (:temporary (:sc double-avx2-reg) ymm7)
  (:results (ymm6 :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator
   34
   (move k k-ptr)
   (move k0 k0-ptr)
   (move i0 i-ptr)
   (move i1 i-ptr)
   (inst add i1 k)
   (inst xor p p)
   (inst vxorpd ymm6 ymm6 ymm6)
   (inst vxorpd ymm7 ymm7 ymm7)
   LOOP
   (inst vmovupd ymm0
         (make-ea-for-float-ref
          va i0 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm1
         (make-ea-for-float-ref
          va i0 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm2
         (make-ea-for-float-ref
          va i1 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm3
         (make-ea-for-float-ref
          va i1 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm4
         (make-ea-for-float-ref
          vb p 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm5
         (make-ea-for-float-ref
          vb p 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm6 ymm0 ymm4)
   (inst vfmadd231pd ymm6 ymm1 ymm5)
   (inst vfmadd231pd ymm7 ymm2 ymm4)
   (inst vfmadd231pd ymm7 ymm3 ymm5)
   (inst add i0 8)
   (inst add i1 8)
   (inst add p 8)
   (inst cmp p k0)
   (inst jmp :b LOOP)
   DONE
   (inst vhaddpd ymm6 ymm6 ymm7)))

(in-package :cl3a.mvmult_vop)


(defun mvi2x4-pd (k k0 i va vb vci)
  (mvi2x4-pd k k0 i va vb vci))

(defun mvi2x8-pd (k k0 i va vb)
  (mvi2x8-pd k k0 i va vb))
