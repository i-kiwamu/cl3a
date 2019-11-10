(in-package :cl-user)
(defpackage cl3a.dotprod_vop
  (:use :cl :sb-ext :sb-c :sb-vm)
  (:export :dpi8-ps :dpi8-avx2-ps :dpi8-pd :dpi8-avx2-pd :dp16-avx2-pd))
(in-package :cl3a.dotprod_vop)


(defknown dpi8-ps ((simple-array single-float (*))
                   (simple-array single-float (*))
                   fixnum)
    (sb-kernel:simd-pack single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown dpi8-avx2-ps ((simple-array single-float (*))
                        (simple-array single-float (*))
                        fixnum)
    (sb-ext:simd-pack-256 single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown dpi8-pd ((simple-array double-float (*))
                   (simple-array double-float (*))
                   fixnum)
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown dpi8-avx2-pd ((simple-array double-float (*))
                        (simple-array double-float (*))
                        fixnum)
    (sb-ext:simd-pack-256 double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown dp16-avx2-pd ((simple-array double-float (*))
                        (simple-array double-float (*))
                        fixnum)
    (sb-ext:simd-pack-256 double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)


(in-package :sb-vm)

(define-vop (cl3a.dotprod_vop::dpi8-ps)
  (:translate cl3a.dotprod_vop::dpi8-ps)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (y :scs (descriptor-reg))
         (i-tn :scs (signed-reg)))
  (:arg-types simple-array-single-float simple-array-single-float
              tagged-num)
  (:temporary (:sc single-sse-reg :from (:argument 0)) xmm0)
  (:temporary (:sc single-sse-reg :from (:argument 1)) xmm1)
  (:results (xmm2 :scs (single-sse-reg) :from (:argument 1)))
  (:result-types simd-pack-single)
  (:generator
   25
   (let ((i (if (sc-is i-tn immediate)
                (tn-value i-tn)
                i-tn)))
     (inst movups xmm0 (make-ea-for-float-ref x i 0 4
                                              :scale (ash 8 (- n-fixnum-tag-bits))))
     (inst movups xmm1 (make-ea-for-float-ref y i 0 4
                                              :scale (ash 8 (- n-fixnum-tag-bits))))
     (inst mulps xmm1 xmm0)
     (inst movups xmm0 (make-ea-for-float-ref x i 4 4
                                              :scale (ash 8 (- n-fixnum-tag-bits))))
     (inst movups xmm2 (make-ea-for-float-ref y i 4 4
                                              :scale (ash 8 (- n-fixnum-tag-bits))))
     (inst mulps xmm2 xmm0))
   (inst addps xmm2 xmm1)))

(define-vop (cl3a.dotprod_vop::dpi8-avx2-ps)
  (:translate cl3a.dotprod_vop::dpi8-avx2-ps)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (y :scs (descriptor-reg))
         (i-tn :scs (signed-reg)))
  (:arg-types simple-array-single-float simple-array-single-float
              tagged-num)
  (:temporary (:sc single-avx2-reg :from (:argument 0)) ymm0)
  (:temporary (:sc single-avx2-reg :from (:argument 1)) ymm1)
  (:results (ymm2 :scs (single-avx2-reg) :from (:argument 1)))
  (:result-types simd-pack-256-single)
  (:generator
   25
   (inst vxorps ymm2 ymm2 ymm2)
   (let ((i (if (sc-is i-tn immediate)
                (tn-value i-tn)
                i-tn)))
     (inst vmovups ymm0 (make-ea-for-float-ref x i 0 4
                                               :scale (ash 8 (- n-fixnum-tag-bits))))
     (inst vmovups ymm1 (make-ea-for-float-ref y i 0 4
                                               :scale (ash 8 (- n-fixnum-tag-bits)))))
   (inst vfmadd231ps ymm2 ymm0 ymm1)))

(define-vop (cl3a.dotprod_vop::dpi8-pd)
  (:translate cl3a.dotprod_vop::dpi8-pd)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (y :scs (descriptor-reg))
         (i-tn :scs (signed-reg)))
  (:arg-types simple-array-double-float simple-array-double-float
              tagged-num)
  (:temporary (:sc double-sse-reg :from (:argument 0)) xmm0)
  (:temporary (:sc double-sse-reg :from (:argument 1)) xmm1)
  (:results (xmm2 :scs (double-sse-reg) :from (:argument 1)))
  (:result-types simd-pack-double)
  (:generator
   25
   (let ((i (if (sc-is i-tn immediate)
                (tn-value i-tn)
                i-tn)))
     (inst movupd xmm0
           (make-ea-for-float-ref
            x i 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
     (inst movupd xmm1
           (make-ea-for-float-ref
            y i 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
     (inst mulpd xmm1 xmm0)
     (inst movupd xmm0
           (make-ea-for-float-ref
            x i 2 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
     (inst movupd xmm2
           (make-ea-for-float-ref
            y i 2 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
     (inst mulpd xmm2 xmm0)
     (inst addpd xmm2 xmm1)
     (inst movupd xmm0
           (make-ea-for-float-ref
            x i 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
     (inst movupd xmm1
           (make-ea-for-float-ref
            y i 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
     (inst mulpd xmm1 xmm0)
     (inst addpd xmm1 xmm2)
     (inst movupd xmm0
           (make-ea-for-float-ref
            x i 6 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
     (inst movupd xmm2
           (make-ea-for-float-ref
            y i 6 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
     (inst mulpd xmm2 xmm0))
   (inst addpd xmm2 xmm1)))

(define-vop (cl3a.dotprod_vop::dpi8-avx2-pd)
  (:translate cl3a.dotprod_vop::dpi8-avx2-pd)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (y :scs (descriptor-reg))
         (i-tn :scs (signed-reg)))
  (:arg-types simple-array-double-float simple-array-double-float
              tagged-num)
  (:temporary (:sc double-avx2-reg :from (:argument 0)) ymm0)
  (:temporary (:sc double-avx2-reg :from (:argument 1)) ymm1)
  (:results (ymm2 :scs (double-avx2-reg) :from (:argument 1)))
  (:result-types simd-pack-256-double)
  (:generator
   25
   (inst vxorpd ymm2 ymm2 ymm2)
   (let ((i (if (sc-is i-tn immediate)
                (tn-value i-tn)
                i-tn)))
     (inst vmovupd ymm0
           (make-ea-for-float-ref
            x i 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
     (inst vmovupd ymm1
           (make-ea-for-float-ref
            y i 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
     (inst vfmadd231pd ymm2 ymm0 ymm1)
     (inst vmovupd ymm0
           (make-ea-for-float-ref
            x i 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
     (inst vmovupd ymm1
           (make-ea-for-float-ref
            y i 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
     (inst vfmadd231pd ymm2 ymm0 ymm1))))

(define-vop (cl3a.dotprod_vop::dp16-avx2-pd)
  (:translate cl3a.dotprod_vop::dp16-avx2-pd)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (y :scs (descriptor-reg))
         (n0-tn :scs (signed-reg)))
  (:arg-types simple-array-double-float simple-array-double-float
              tagged-num)
  (:temporary (:sc signed-reg) i)
  (:temporary (:sc signed-reg) n0)
  (:temporary (:sc double-avx2-reg) ymm0)
  (:temporary (:sc double-avx2-reg) ymm1)
  (:temporary (:sc double-avx2-reg) ymm2)
  (:temporary (:sc double-avx2-reg) ymm3)
  (:temporary (:sc double-avx2-reg) ymm5)
  (:temporary (:sc double-avx2-reg) ymm6)
  (:temporary (:sc double-avx2-reg) ymm7)
  (:results (ymm4 :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator
   25
   (move n0 n0-tn)
   (inst vxorpd ymm4 ymm4 ymm4)
   (inst vxorpd ymm5 ymm5 ymm5)
   (inst vxorpd ymm6 ymm6 ymm6)
   (inst vxorpd ymm7 ymm7 ymm7)
   (inst xor i i)
   LOOP
   (inst vmovupd ymm0
         (make-ea-for-float-ref
          x i 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm1
         (make-ea-for-float-ref
          x i 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm2
         (make-ea-for-float-ref
          x i 8 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vmovupd ymm3
         (make-ea-for-float-ref
          x i 12 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm4 ymm0
         (make-ea-for-float-ref
          y i 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm5 ymm1
         (make-ea-for-float-ref
          y i 4 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm6 ymm2
         (make-ea-for-float-ref
          y i 8 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst vfmadd231pd ymm7 ymm3
         (make-ea-for-float-ref
          y i 12 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))
   (inst add i 16)
   (inst cmp i n0)
   (inst jmp :b LOOP)
   DONE
   (inst vaddpd ymm4 ymm4 ymm5)
   (inst vaddpd ymm6 ymm6 ymm7)
   (inst vaddpd ymm4 ymm4 ymm6)))


(in-package :cl3a.dotprod_vop)

(defun dpi8-ps (x y i)
  (dpi8-ps x y i))

(defun dpi8-avx2-ps (x y i)
  (dpi8-avx2-ps x y i))

(defun dpi8-pd (x y i)
  (dpi8-pd x y i))

(defun dpi8-avx2-pd (x y i)
  (dpi8-avx2-pd x y i))

(defun dp16-avx2-pd (x y n0)
  (dp16-avx2-pd x y n0))
