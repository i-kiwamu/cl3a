(in-package :cl-user)
(defpackage cl3a.dotprod_vop
  (:use :cl :sb-ext :sb-c :sb-vm :alexandria)
  (:export :aref-ps :aref-pd :f4*-ps :f2*-pd :f4+-ps :f2+-pd :dpi2-ps :dpi4-pd
           :setzero-ps :setzero-pd))
(in-package :cl3a.dotprod_vop)


(defknown aref-ps ((simple-array single-float (*))
                   (unsigned-byte 64))
    (sb-kernel:simd-pack single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown aref-pd ((simple-array double-float (*))
                   (unsigned-byte 64))
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f4*-ps ((sb-kernel:simd-pack single-float)
                  (sb-kernel:simd-pack single-float))
    (sb-kernel:simd-pack single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f2*-pd ((sb-kernel:simd-pack double-float)
                  (sb-kernel:simd-pack double-float))
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f4+-ps ((sb-kernel:simd-pack single-float)
                  (sb-kernel:simd-pack single-float))
    (sb-kernel:simd-pack single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f2+-pd ((sb-kernel:simd-pack double-float)
                  (sb-kernel:simd-pack double-float))
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown dpi2-ps ((simple-array single-float (*))
                   (simple-array single-float (*))
                   (unsigned-byte 64))
    (sb-kernel:simd-pack single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown dpi4-pd ((simple-array double-float (*))
                   (simple-array double-float (*))
                   (unsigned-byte 64))
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)


(in-package :sb-vm)

(define-vop (cl3a.dotprod_vop::aref-ps)
  (:translate cl3a.dotprod_vop::aref-ps)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (i :scs (unsigned-reg)))
  (:arg-types simple-array-single-float unsigned-num)
  (:results (r :scs (single-sse-reg)))
  (:result-types simd-pack-single)
  (:generator
   5
   (inst movups r (make-ea-for-float-ref x i 0 4
                   :scale (ash 8 (- n-fixnum-tag-bits))))))

(define-vop (cl3a.dotprod_vop::aref-pd)
  (:translate cl3a.dotprod_vop::aref-pd)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (i :scs (unsigned-reg)))
  (:arg-types simple-array-double-float unsigned-num)
  (:results (r :scs (double-sse-reg)))
  (:result-types simd-pack-double)
  (:generator
   7
   (inst movupd r
         (make-ea-for-float-ref
          x i 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))))

(define-vop (cl3a.dotprod_vop::f4*-ps)
  (:translate cl3a.dotprod_vop::f4*-ps)
  (:policy :fast-safe)
  (:args (x :scs (single-sse-reg))
         (y :scs (single-sse-reg)))
  (:arg-types simd-pack-single simd-pack-single)
  (:results (dst :scs (single-sse-reg) :from (:argument 0)))
  (:result-types simd-pack-single)
  (:generator 4
              (move dst x)
              (inst mulps dst y)))

(define-vop (cl3a.dotprod_vop::f2*-pd)
  (:translate cl3a.dotprod_vop::f2*-pd)
  (:policy :fast-safe)
  (:args (x :scs (double-sse-reg))
         (y :scs (double-sse-reg)))
  (:arg-types simd-pack-double simd-pack-double)
  (:results (dst :scs (double-sse-reg) :from (:argument 0)))
  (:result-types simd-pack-double)
  (:generator
   4
   (move dst x)
   (inst mulpd dst y)))

(define-vop (cl3a.dotprod_vop::f4+-ps)
  (:translate cl3a.dotprod_vop::f4+-ps)
  (:policy :fast-safe)
  (:args (x :scs (single-sse-reg))
         (y :scs (single-sse-reg)))
  (:arg-types simd-pack-single simd-pack-single)
  (:results (dst :scs (single-sse-reg) :from (:argument 0)))
  (:result-types simd-pack-single)
  (:generator 4
              (move dst x)
              (inst addps dst y)))

(define-vop (cl3a.dotprod_vop::f2+-pd)
  (:translate cl3a.dotprod_vop::f2+-pd)
  (:policy :fast-safe)
  (:args (x :scs (double-sse-reg))
         (y :scs (double-sse-reg)))
  (:arg-types simd-pack-double simd-pack-double)
  (:results (dst :scs (double-sse-reg) :from (:argument 0)))
  (:result-types simd-pack-double)
  (:generator
   4
   (move dst x)
   (inst addpd dst y)))

(define-vop (cl3a.dotprod_vop::dpi2-ps)
  (:translate cl3a.dotprod_vop::dpi2-ps)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (y :scs (descriptor-reg))
         (i :scs (unsigned-reg)))
  (:arg-types simple-array-single-float simple-array-single-float
              unsigned-num)
  (:temporary (:sc single-sse-reg :from (:argument 0)) xmm0)
  (:temporary (:sc single-sse-reg :from (:argument 1)) xmm1)
  (:results (xmm2 :scs (single-sse-reg) :from (:argument 1)))
  (:result-types simd-pack-single)
  (:generator
   25
   (inst movups xmm0 (make-ea-for-float-ref x i 0 4
                                            :scale (ash 8 (- n-fixnum-tag-bits))))
   (inst movups xmm1 (make-ea-for-float-ref y i 0 4
                                            :scale (ash 8 (- n-fixnum-tag-bits))))
   (inst mulps xmm1 xmm0)
   (inst movups xmm0 (make-ea-for-float-ref x i 4 4
                                            :scale (ash 8 (- n-fixnum-tag-bits))))
   (inst movups xmm2 (make-ea-for-float-ref y i 4 4
                                            :scale (ash 8 (- n-fixnum-tag-bits))))
   (inst mulps xmm2 xmm0)
   (inst addps xmm2 xmm1)))

(define-vop (cl3a.dotprod_vop::dpi4-pd)
  (:translate cl3a.dotprod_vop::dpi4-pd)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (y :scs (descriptor-reg))
         (i :scs (unsigned-reg)))
  (:arg-types simple-array-double-float simple-array-double-float
              unsigned-num)
  (:temporary (:sc double-sse-reg :from (:argument 0)) xmm0)
  (:temporary (:sc double-sse-reg :from (:argument 1)) xmm1)
  (:results (xmm2 :scs (double-sse-reg) :from (:argument 1)))
  (:result-types simd-pack-double)
  (:generator
   25
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
   (inst mulpd xmm2 xmm0)
   (inst addpd xmm2 xmm1)))


(in-package :cl3a.dotprod_vop)

(defun aref-ps (x i)
  (aref-ps x i))

(defun aref-pd (x i)
  (aref-pd x i))

(defun f4*-ps (x y)
  (f4*-ps x y))

(defun f2*-pd (x y)
  (f2*-pd x y))

(defun f4+-ps (x y)
  (f4+-ps x y))

(defun f2+-pd (x y)
  (f2+-pd x y))

(defun dpi2-ps (x y i)
  (dpi2-ps x y i))

(defun dpi4-pd (x y i)
  (dpi4-pd x y i))

(defun setzero-ps ()
  (sb-kernel::%make-simd-pack-single 0s0 0s0 0s0 0s0))

(defun setzero-pd ()
  (sb-kernel::%make-simd-pack-double 0d0 0d0))
