(in-package :cl-user)
(defpackage cl3a.mvmult_vop
  (:use :cl :sb-ext :sb-c :sb-vm)
  (:export :mvi2x4-pd :mvi2x8-pd))
(in-package :cl3a.mvmult_vop)


(defknown mvi2x4-pd
    (fixnum fixnum fixnum
     (simple-array double-float (*))
     (simple-array double-float (*)))
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

(progn
  (defmacro load-pd (ymm vector index &optional (offset 0))
    `(inst vmovupd ,ymm
           (make-ea-for-float-ref
            ,vector ,index ,offset 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))))
  (defmacro store-pd (ymm vector index &optional (offset 0))
    `(inst vmovupd
           (make-ea-for-float-ref
            ,vector ,index ,offset 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
           ,ymm)))

(define-vop (cl3a.mvmult_vop::mvi2x4-pd)
  (:translate cl3a.mvmult_vop::mvi2x4-pd)
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
  (:temporary (:sc double-sse-reg :from (:argument 3)) xmm0)
  (:temporary (:sc double-sse-reg :from (:argument 3)) xmm1)
  (:temporary (:sc double-sse-reg :from (:argument 4)) xmm2)
  (:temporary (:sc double-sse-reg :from (:argument 4)) xmm3)
  (:temporary (:sc double-sse-reg :from (:argument 5)) xmm4)
  (:temporary (:sc double-sse-reg :from (:argument 5)) xmm5)
  (:results (xmm6 :scs (double-sse-reg)))
  (:result-types simd-pack-double)
  (:generator
   34
   (move k k-ptr)
   (move k0 k0-ptr)
   (move i0 i-ptr)
   (move i1 i-ptr)
   (inst add i1 k)
   (inst xor p p)
   (inst xorpd xmm6 xmm6 xmm6)
   LOOP
   (load-pd xmm4 vb p)
   (load-pd xmm5 vb p 2)
   (load-pd xmm0 va i0)
   (inst mulpd xmm0 xmm4)    ; xmm0 = {va[i0] * vb[p], va[i0+1] * vb[p+1]}     = {ab[i0p], ab[i0p+1]}
   (load-pd xmm1 va i0 2)
   (load-pd xmm2 va i1)
   (inst mulpd xmm1 xmm5)    ; xmm1 = {va[i0+2] * vb[p+2], va[i0+3] * vb[p+3]} = {ab[i0p+2], ab[i0p+3]}
   (inst mulpd xmm2 xmm4)    ; xmm2 = {va[i1] * vb[p], va[i1+1] * vb[p+1]}     = {ab[i1p], ab[i1p+1]}
   (load-pd xmm3 va i1 2)
   (inst mulpd xmm3 xmm5)    ; xmm3 = {va[i1+2] * vb[p+2], va[i1+3] * vb[p+3]} = {ab[i1p+2], ab[i1p+3]}
   (inst addpd xmm0 xmm1)    ; xmm0 = {ab[i0p] + ab[i0p+2], ab[i0p+1] + ab[i0p+3]}
   (inst addpd xmm2 xmm3)    ; xmm2 = {ab[i1p] + ab[i1p+2], ab[i1p+1] + ab[i1p+3]}
   (inst haddpd xmm0 xmm0)   ; xmm0 = rep({ab[i0p:i0p+3]}, 2)
   (inst haddpd xmm2 xmm2)   ; xmm2 = rep({ab[i1p:i1p+3]}, 2)
   (inst unpckhpd xmm2 xmm0) ; xmm2 = {ab[i0p:i0p+3], ab[i1p:i1p+3]}
   (inst addpd xmm6 xmm2)
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
   (load-pd ymm4 vb p)
   (load-pd ymm5 vb p 4)
   (load-pd ymm0 va i0)
   (inst vfmadd231pd ymm6 ymm0 ymm4)  ; ymm6 += {va[i0] * vb[p], va[i0+1] * vb[p+1], va[i0+2] * vb[p+2]. va[i0+3] * vb[p+3]}
                                      ;         = {ab[i0p:i0p+3]}
   (load-pd ymm1 va i0 4)
   (load-pd ymm2 va i1)
   (inst vfmadd231pd ymm7 ymm2 ymm4)  ; ymm7 += {ab[i1p:i1p+3]}
   (inst vfmadd231pd ymm6 ymm1 ymm5)  ; ymm6 += {ab[i0p+4:i0p+7]}
                                      ;         = {ab[i0p] + ab[i0p+4], ab[i0p+1] + ab[i0p+5],
                                      ;            ab[i0p+2]+ ab[i0p+6], ab[i0p+3] + ab[i0p+7]}
   (load-pd ymm3 va i1 4)
   (inst vfmadd231pd ymm7 ymm3 ymm5)  ; ymm7 = {ab[i1p] + ab[i1p+4], ab[i1p+1] + ab[i1p+5],
                                      ;         ab[i1p+2]+ ab[i1p+6], ab[i1p+3] + ab[i1p+7]}
   (inst add i0 8)
   (inst add i1 8)
   (inst add p 8)
   (inst cmp p k0)
   (inst jmp :b LOOP)
   DONE
   (inst vhaddpd ymm6 ymm6 ymm7)      ; ymm6 = {ab[i0p] + ab[i0p+1] + ab[i0p+4] + ab[i0p+5],
                                      ;         ab[i1p] + ab[i1p+1] + ab[i1p+4] + ab[i1p+5],
                                      ;         ab[i0p+2] + ab[i0p+3] + ab[i0p+6] + ab[i0p+7],
                                      ;         ab[i1p+2] + ab[i1p+3] + ab[i1p+6] + ab[i1p+7]}
   ))

(in-package :cl3a.mvmult_vop)


(defun mvi2x4-pd (k k0 i va vb)
  (mvi2x4-pd k k0 i va vb))

(defun mvi2x8-pd (k k0 i va vb)
  (mvi2x8-pd k k0 i va vb))
