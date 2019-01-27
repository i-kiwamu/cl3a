(in-package :cl-user)
(defpackage cl3a.mmmult9_vop
  (:use :cl :sb-ext :sb-c :sb-vm)
  (:export :dgebp-reg-ker :dgebp-reg-ker4))
(in-package :cl3a.mmmult9_vop)


(defknown dgebp-reg-ker
    (fixnum
     (simple-array double-float (*)) fixnum
     (simple-array double-float (*)) fixnum)
    (simd-pack-256 double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown dgebp-reg-ker4
    (fixnum fixnum
     (simple-array double-float (*)) fixnum
     (simple-array double-float (*)) fixnum)
    (simd-pack-256 double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)


(in-package :sb-vm)

(define-vop (cl3a.mmmult9_vop::dgebp-reg-ker)
  (:translate cl3a.mmmult9_vop::dgebp-reg-ker)
  (:policy :fast-safe)
  (:args (k0-ptr :scs (signed-reg) :target p)
         (A :scs (descriptor-reg) :target ymm0)
         (rma-ptr :scs (signed-reg))
         (Bt :scs (descriptor-reg) :target ymm2)
         (rmb-ptr :scs (signed-reg)))
  (:arg-types tagged-num
              simple-array-double-float tagged-num
              simple-array-double-float tagged-num)
  (:temporary (:sc signed-reg :from (:argument 0)) k0)
  (:temporary (:sc signed-reg :from (:argument 2)) rma)
  (:temporary (:sc signed-reg :from (:argument 4)) rmb)
  (:temporary (:sc signed-reg :offset rcx-offset) p)
  (:temporary (:sc double-avx2-reg :from (:argument 1)) ymm0)
  (:temporary (:sc double-avx2-reg :from (:argument 1)) ymm1)
  (:temporary (:sc double-avx2-reg :from (:argument 3)) ymm2)
  (:temporary (:sc double-avx2-reg :from (:argument 3)) ymm3)
  (:results (ymm4 :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator
   30
   (move k0 k0-ptr)
   (move rma rma-ptr)
   (move rmb rmb-ptr)
   (inst xor p p)
   (inst vxorpd ymm4 ymm4 ymm4)
   LOOP
   (inst vmovupd ymm0
         (make-ea-for-float-ref
          A rma 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm1
         (make-ea-for-float-ref
          A rma 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm2
         (make-ea-for-float-ref
          Bt rmb 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm3
         (make-ea-for-float-ref
          Bt rmb 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm4 ymm0 ymm2)
   (inst vfmadd231pd ymm4 ymm1 ymm3)
   (inst add p 8)
   (inst add rma 8)
   (inst add rmb 8)
   (inst cmp p k0)
   (inst jmp :b LOOP)
   DONE))


(define-vop (cl3a.mmmult9_vop::dgebp-reg-ker4)
  (:translate cl3a.mmmult9_vop::dgebp-reg-ker4)
  (:policy :fast-safe)
  (:args (k-ptr :scs (signed-reg))
         (k0-ptr :scs (signed-reg) :target p)
         (A :scs (descriptor-reg) :target ymm0)
         (rma-ptr :scs (signed-reg))
         (Bt :scs (descriptor-reg) :target ymm2)
         (rmb-ptr :scs (signed-reg)))
  (:arg-types tagged-num tagged-num
              simple-array-double-float tagged-num
              simple-array-double-float tagged-num)
  (:temporary (:sc signed-reg :from (:argument 0)) k)
  (:temporary (:sc signed-reg :from (:argument 1)) k0)
  (:temporary (:sc signed-reg :from (:argument 3)) rma)
  (:temporary (:sc signed-reg :from (:argument 5)) rmb0)
  (:temporary (:sc signed-reg :from (:argument 5)) rmb1)
  (:temporary (:sc signed-reg :from (:argument 5)) rmb2)
  (:temporary (:sc signed-reg :from (:argument 5)) rmb3)
  (:temporary (:sc signed-reg :offset rcx-offset) p)
  (:temporary (:sc double-avx2-reg :from (:argument 2)) ymm4)
  (:temporary (:sc double-avx2-reg :from (:argument 2)) ymm5)
  (:temporary (:sc double-avx2-reg :from (:argument 4)) ymm6)
  (:temporary (:sc double-avx2-reg :from (:argument 4)) ymm7)
  (:temporary (:sc double-avx2-reg :from (:argument 4)) ymm8)
  (:temporary (:sc double-avx2-reg :from (:argument 4)) ymm9)
  (:temporary (:sc double-avx2-reg :from (:argument 4)) ymm10)
  (:temporary (:sc double-avx2-reg :from (:argument 4)) ymm11)
  (:temporary (:sc double-avx2-reg :from (:argument 4)) ymm12)
  (:temporary (:sc double-avx2-reg :from (:argument 4)) ymm13)
  (:temporary (:sc double-avx2-reg) ymm1)
  (:temporary (:sc double-avx2-reg) ymm2)
  (:temporary (:sc double-avx2-reg) ymm3)
  (:results (ymm0 :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator
   30
   (move k k-ptr)
   (move k0 k0-ptr)
   (move rma rma-ptr)
   (move rmb0 rmb-ptr)
   (move rmb1 rmb-ptr)
   (move rmb2 rmb-ptr)
   (move rmb3 rmb-ptr)
   (inst add rmb1 k)
   (inst add rmb2 k)
   (inst add rmb2 k)
   (inst add rmb3 k)
   (inst add rmb3 k)
   (inst add rmb3 k)
   (inst xor p p)
   (inst vxorpd ymm0 ymm0 ymm0)
   (inst vxorpd ymm1 ymm1 ymm1)
   (inst vxorpd ymm2 ymm2 ymm2)
   (inst vxorpd ymm3 ymm3 ymm3)
   LOOP
   (inst vmovupd ymm4
         (make-ea-for-float-ref
          A rma 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm5
         (make-ea-for-float-ref
          A rma 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm6
         (make-ea-for-float-ref
          Bt rmb0 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm7
         (make-ea-for-float-ref
          Bt rmb0 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm8
         (make-ea-for-float-ref
          Bt rmb1 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm9
         (make-ea-for-float-ref
          Bt rmb1 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm10
         (make-ea-for-float-ref
          Bt rmb2 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm11
         (make-ea-for-float-ref
          Bt rmb2 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm12
         (make-ea-for-float-ref
          Bt rmb3 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm13
         (make-ea-for-float-ref
          Bt rmb3 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm0 ymm4 ymm6)
   (inst vfmadd231pd ymm0 ymm5 ymm7)
   (inst vfmadd231pd ymm1 ymm4 ymm8)
   (inst vfmadd231pd ymm1 ymm5 ymm9)
   (inst vfmadd231pd ymm2 ymm4 ymm10)
   (inst vfmadd231pd ymm2 ymm5 ymm11)
   (inst vfmadd231pd ymm3 ymm4 ymm12)
   (inst vfmadd231pd ymm3 ymm5 ymm13)
   (inst add p 8)
   (inst add rma 8)
   (inst add rmb0 8)
   (inst add rmb1 8)
   (inst add rmb2 8)
   (inst add rmb3 8)
   (inst cmp p k0)
   (inst jmp :b LOOP)
   DONE
   (inst vhaddpd ymm0 ymm0 ymm1)
   (inst vhaddpd ymm2 ymm2 ymm3)
   (inst vpermpd ymm0 ymm0 216)
   (inst vpermpd ymm2 ymm2 216)
   (inst vhaddpd ymm0 ymm0 ymm2)))


(in-package :cl3a.mmmult9_vop)


(defun dgebp-reg-ker (k0 A rma Bt rmb)
  (dgebp-reg-ker k0 A rma Bt rmb))

(defun dgebp-reg-ker4 (k k0 A rma Bt rmb)
  (dgebp-reg-ker4 k k0 A rma Bt rmb))
