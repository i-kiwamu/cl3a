(in-package :cl-user)
(defpackage cl3a.mmmult10_vop
  (:use :cl :sb-ext :sb-c :sb-vm)
  (:export :dgebp-1-nr-ker :dgebp-mr-nr-ker))
(in-package :cl3a.mmmult10_vop)


(defknown dgebp-1-nr-ker
    (fixnum fixnum
     (simple-array double-float (*))
     (simple-array double-float (*))
     (simple-array double-float (*))
     fixnum fixnum fixnum)
    (simple-array double-float (*))
    (any always-translatable)
  :overwrite-fndb-silently t)

(defknown dgebp-mr-nr-ker
    (fixnum fixnum
     (simple-array double-float (*))
     (simple-array double-float (*))
     (simple-array double-float (*))
     fixnum fixnum fixnum)
    (simple-array double-float (*))
    (any always-translatable)
  :overwrite-fndb-silently t)


(in-package :sb-vm)

(define-vop (cl3a.mmmult10_vop::dgebp-1-nr-ker)
  (:translate cl3a.mmmult10_vop::dgebp-1-nr-ker)
  (:policy :fast-safe)
  (:args (p-ptr :scs (signed-reg) :target p)
         (n-ptr :scs (signed-reg) :target n)
         (Apd :scs (descriptor-reg) :to :eval)
         (B :scs (descriptor-reg) :to :eval)
         (Caux :scs (descriptor-reg) :to :eval)
         (rma-init :scs (signed-reg) :target rma)
         (rmb-init :scs (signed-reg) :target rmb)
         (rmc :scs (signed-reg)))
  (:arg-types tagged-num tagged-num
              simple-array-double-float
              simple-array-double-float
              simple-array-double-float
              tagged-num tagged-num tagged-num)
  (:temporary (:sc signed-reg :from (:argument 0)) p)
  (:temporary (:sc signed-reg :from (:argument 1)) n)
  (:temporary (:sc signed-reg :from (:argument 5)) rma)
  (:temporary (:sc signed-reg :from (:argument 6)) rmb)
  (:temporary (:sc signed-reg :offset rcx-offset) k)
  (:temporary (:sc double-avx2-reg) ymm0)  ; for Apd
  (:temporary (:sc double-avx2-reg) ymm1)  ; for B
  (:temporary (:sc double-avx2-reg) ymm2)
  (:temporary (:sc double-avx2-reg) ymm3)  ; for Caux
  (:temporary (:sc double-avx2-reg) ymm4)
  (:results (result :scs (descriptor-reg)))
  (:result-types simple-array-double-float)
  (:generator
   30
   (move p p-ptr)
   (move n n-ptr)
   (move rma rma-init)
   (move rmb rmb-init)
   (inst xor k k)
   (inst vxorpd ymm3 ymm3 ymm3)
   (inst vxorpd ymm4 ymm4 ymm4)
   LOOP
   (inst vbroadcastsd ymm0
         (make-ea-for-float-ref
          Apd rma 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm1
         (make-ea-for-float-ref
          B rmb 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm2
         (make-ea-for-float-ref
          B rmb 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm3 ymm0 ymm1)
   ;; (inst prefetch :t0
   ;;       (make-ea :byte :base B
   ;;                :index rmb
   ;;                :scale (ash 16 (- n-fixnum-tag-bits))
   ;;                :disp 128))
   (inst vfmadd231pd ymm4 ymm0 ymm2)
   ;; (inst prefetch :t0
   ;;       (make-ea :byte :base B
   ;;                :index rmb
   ;;                :scale (ash 16 (- n-fixnum-tag-bits))
   ;;                :disp 192))
   (inst add k 1)
   (inst add rma 1)
   (inst add rmb n)
   (inst cmp k p)
   (inst jmp :b LOOP)
   DONE
   (sc-case rmc
     (immediate
      (inst vmovupd
            (make-ea-for-float-ref
             Caux (tn-value rmc) 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
            ymm3)
      (inst vmovupd
            (make-ea-for-float-ref
             Caux (tn-value rmc) 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
            ymm4))
     (signed-reg
      (inst vmovupd
            (make-ea-for-float-ref
             Caux rmc 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
            ymm3)
      (inst vmovupd
            (make-ea-for-float-ref
             Caux rmc 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
            ymm4)))
   (move result Caux)))

(define-vop (cl3a.mmmult10_vop::dgebp-mr-nr-ker)
  (:translate cl3a.mmmult10_vop::dgebp-mr-nr-ker)
  (:policy :fast-safe)
  (:args (pc-ptr :scs (signed-reg) :target pc)
         (n-ptr :scs (signed-reg) :target n)
         (Apd :scs (descriptor-reg) :to :eval)
         (B :scs (descriptor-reg) :to :eval)
         (Caux :scs (descriptor-reg) :to :eval)
         (rma-init :scs (signed-reg) :target rma)
         (rmb-init :scs (signed-reg) :target rmb)
         (rmc-init :scs (signed-reg) :target rmc))
  (:arg-types tagged-num tagged-num
              simple-array-double-float
              simple-array-double-float
              simple-array-double-float
              tagged-num tagged-num tagged-num)
  (:temporary (:sc signed-reg :from (:argument 0)) pc)
  (:temporary (:sc signed-reg :from (:argument 1)) n)
  (:temporary (:sc signed-reg :from (:argument 5)) rma)
  (:temporary (:sc signed-reg :from (:argument 5)) rma0)
  (:temporary (:sc signed-reg :from (:argument 6)) rmb)
  (:temporary (:sc signed-reg :from (:argument 7)) rmc)
  (:temporary (:sc signed-reg :offset rcx-offset) k)
  (:temporary (:sc double-avx2-reg) ymm0)  ; for A
  (:temporary (:sc double-avx2-reg) ymm1)  ; for B
  (:temporary (:sc double-avx2-reg) ymm2)
  (:temporary (:sc double-avx2-reg) ymm3)  ; for C
  (:temporary (:sc double-avx2-reg) ymm4)
  (:temporary (:sc double-avx2-reg) ymm5)
  (:temporary (:sc double-avx2-reg) ymm6)
  (:temporary (:sc double-avx2-reg) ymm7)
  (:temporary (:sc double-avx2-reg) ymm8)
  (:temporary (:sc double-avx2-reg) ymm9)
  (:temporary (:sc double-avx2-reg) ymm10)
  (:results (result :scs (descriptor-reg)))
  (:result-types simple-array-double-float)
  ; (:results)
  (:generator
   30
   (move pc pc-ptr)
   (move n n-ptr)
   (move rma0 rma-init)
   (move rmb rmb-init)
   (inst xor k k)
   (inst vxorpd ymm3 ymm3 ymm3)
   (inst vxorpd ymm4 ymm4 ymm4)
   (inst vxorpd ymm5 ymm5 ymm5)
   (inst vxorpd ymm6 ymm6 ymm6)
   (inst vxorpd ymm7 ymm7 ymm7)
   (inst vxorpd ymm8 ymm8 ymm8)
   (inst vxorpd ymm9 ymm9 ymm9)
   (inst vxorpd ymm10 ymm10 ymm10)
   LOOP
   (move rma rma0)
   (inst vbroadcastsd ymm0
         (make-ea-for-float-ref
          Apd rma 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm1
         (make-ea-for-float-ref
          B rmb 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm2
         (make-ea-for-float-ref
          B rmb 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm3 ymm0 ymm1)
   (inst vfmadd231pd ymm4 ymm0 ymm2)
   (inst add rma pc)
   (inst vbroadcastsd ymm0
         (make-ea-for-float-ref
          Apd rma 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm5 ymm0 ymm1)
   (inst vfmadd231pd ymm6 ymm0 ymm2)
   (inst add rma pc)
   (inst vbroadcastsd ymm0
         (make-ea-for-float-ref
          Apd rma 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm7 ymm0 ymm1)
   (inst vfmadd231pd ymm8 ymm0 ymm2)
   (inst add rma pc)
   (inst vbroadcastsd ymm0
         (make-ea-for-float-ref
          Apd rma 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm9 ymm0 ymm1)
   (inst vfmadd231pd ymm10 ymm0 ymm2)
   (inst add k 1)
   (inst add rma0 1)
   (inst add rmb n)
   (inst cmp k pc)
   (inst jmp :b LOOP)
   DONE
   (move rmc rmc-init)
   (inst vmovupd
         (make-ea-for-float-ref
          Caux rmc 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
         ymm3)
   (inst vmovupd
         (make-ea-for-float-ref
          Caux rmc 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
         ymm4)
   (inst add rmc 8)
   (inst vmovupd
         (make-ea-for-float-ref
          Caux rmc 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
         ymm5)
   (inst vmovupd
         (make-ea-for-float-ref
          Caux rmc 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
         ymm6)
   (inst add rmc 8)
   (inst vmovupd
         (make-ea-for-float-ref
          Caux rmc 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
         ymm7)
   (inst vmovupd
         (make-ea-for-float-ref
          Caux rmc 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
         ymm8)
   (inst add rmc 8)
   (inst vmovupd
         (make-ea-for-float-ref
          Caux rmc 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
         ymm9)
   (inst vmovupd
         (make-ea-for-float-ref
          Caux rmc 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
         ymm10)
   (move result Caux)))


(in-package :cl3a.mmmult10_vop)

(defun dgebp-1-nr-ker (p n Apd B Caux rma rmb rmc)
  (dgebp-1-nr-ker p n Apd B Caux rma rmb rmc))

(defun dgebp-mr-nr-ker (p n Apd B Caux rma rmb rmc)
  (dgebp-mr-nr-ker p n Apd B Caux rma rmb rmc))
