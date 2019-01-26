(in-package :cl-user)
(defpackage cl3a.mmmult8_vop
  (:use :cl :sb-ext :sb-c :sb-vm)
  (:export :dgebp-reg-ker))
(in-package :cl3a.mmmult8_vop)


(defknown dgebp-reg-ker
    (fixnum
     (simple-array double-float (*)) fixnum
     (simple-array double-float (*)) fixnum)
    (simd-pack-256 double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)


(in-package :sb-vm)

(define-vop (cl3a.mmmult8_vop::dgebp-reg-ker)
  (:translate cl3a.mmmult8_vop::dgebp-reg-ker)
  (:policy :fast-safe)
  (:args (t2-ptr :scs (signed-reg) :target p)
         (Atd :scs (descriptor-reg) :target ymm0)
         (ia-ptr :scs (signed-reg))
         (Bp :scs (descriptor-reg) :target ymm2)
         (ib-ptr :scs (signed-reg)))
  (:arg-types tagged-num
              simple-array-double-float tagged-num
              simple-array-double-float tagged-num)
  (:temporary (:sc signed-reg :from (:argument 0)) t2)
  (:temporary (:sc signed-reg :from (:argument 2)) ia)
  (:temporary (:sc signed-reg :from (:argument 4)) ib)
  (:temporary (:sc signed-reg :offset rcx-offset) p)
  (:temporary (:sc double-avx2-reg :from (:argument 1)) ymm0)
  (:temporary (:sc double-avx2-reg :from (:argument 1)) ymm1)
  (:temporary (:sc double-avx2-reg :from (:argument 3)) ymm2)
  (:temporary (:sc double-avx2-reg :from (:argument 3)) ymm3)
  (:results (ymm4 :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator
   30
   (move t2 t2-ptr)
   (move ia ia-ptr)
   (move ib ib-ptr)
   (inst xor p p)
   (inst vxorpd ymm4 ymm4 ymm4)
   LOOP
   (inst vmovupd ymm0
         (make-ea-for-float-ref
          Atd ia 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm1
         (make-ea-for-float-ref
          Atd ia 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm2
         (make-ea-for-float-ref
          Bp ib 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm3
         (make-ea-for-float-ref
          Bp ib 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm4 ymm0 ymm2)
   (inst vfmadd231pd ymm4 ymm1 ymm3)
   (inst add p 8)
   (inst add ia 8)
   (inst add ib 8)
   (inst cmp p t2)
   (inst jmp :b LOOP)
   DONE))


(in-package :cl3a.mmmult8_vop)


(defun dgebp-reg-ker (t2 Atd ia Bp ib)
  (dgebp-reg-ker t2 Atd ia Bp ib))
