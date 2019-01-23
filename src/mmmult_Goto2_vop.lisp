(in-package :cl-user)
(defpackage cl3a.mmmult_Goto2_vop
  (:use :cl :sb-ext :sb-c :sb-vm)
  (:export :dgebp-reg-ker))
(in-package :cl3a.mmmult_Goto2_vop)


(defknown dgebp-reg-ker
    (fixnum (simple-array double-float (* *))
     fixnum (simple-array double-float (* *)) fixnum)
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)


(in-package :sb-vm)

(define-vop (cl3a.mmmult_Goto2_vop::dgebp-reg-ker)
  (:translate cl3a.mmmult_Goto2_vop::dgebp-reg-ker)
  (:policy :fast-safe)
  (:args (t2-ptr :scs (signed-reg) :target p)
         (Atd :scs (descriptor-reg) :target xmm0)
         (ia-ptr :scs (signed-reg))
         (Bp :scs (descriptor-reg))
         (ib-ptr :scs (signed-reg)))
  (:arg-types tagged-num simple-array-double-float
              tagged-num simple-array-double-float tagged-num)
  (:temporary (:sc signed-reg :offset rcx-offset) p)
  (:temporary (:sc signed-reg :from (:argument 0)) t2)
  (:temporary (:sc signed-reg :from (:argument 2)) ia)
  (:temporary (:sc signed-reg :from (:argument 4)) ib)
  (:temporary (:sc double-sse-reg :from (:argument 1)) xmm1)
  (:temporary (:sc double-sse-reg :from (:argument 3)) xmm2)
  (:temporary (:sc double-sse-reg :from (:argument 3)) xmm3)
  (:results (xmm0 :scs (double-sse-reg) :from (:argument 1)))
  (:result-types simd-pack-double)
  (:generator
   30
   (inst xor p p)
   (move t2 t2-ptr)
   (move ia ia-ptr)
   (move ib ib-ptr)
   LOOP
   (inst movupd xmm0
         (make-ea-for-float-ref
          Atd ia 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd xmm1
         (make-ea-for-float-ref
          Atd ia 2 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd xmm2
         (make-ea-for-float-ref
          Bp ib 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst movupd xmm3
         (make-ea-for-float-ref
          Bp ib 2 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst mulpd xmm0 xmm2)
   (inst mulpd xmm1 xmm3)
   (inst addpd xmm0 xmm1)
   (inst haddpd xmm0 xmm0)
   (inst add p 4)
   (inst add ia 4)
   (inst add ib 4)
   (inst cmp p t2)
   (inst jmp :b LOOP)
   DONE))


(in-package :cl3a.mmmult_Goto2_vop)


(defun dgebp-reg-ker (t2 Atd ia Bp ib)
  (dgebp-reg-ker t2 Atd ia Bp ib))
