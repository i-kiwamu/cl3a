(in-package :cl-user)
(defpackage cl3a.utilities_vop
  (:use :cl :sb-ext :sb-c :sb-vm)
  (:export :aref4-ps :aref8-ps :aref2-pd :aref4-pd
           :aset4-ps :aset8-ps :aset2-pd :aset4-pd
           :f4*-ps :f8*-ps :f2*-pd :f4*-pd
           :f4+-ps :f8+-ps :f2+-pd :f4+-pd
           :setzero4-ps :setzero8-ps :setzero2-pd :setzero4-pd))
(in-package :cl3a.utilities_vop)


(defknown aref4-ps ((simple-array single-float (*))
                    fixnum)
    (sb-kernel:simd-pack single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown aref8-ps ((simple-array single-float (*))
                    fixnum)
    (sb-ext:simd-pack-256 single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown aref2-pd ((simple-array double-float (*))
                    fixnum)
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown aref4-pd ((simple-array double-float (*))
                    fixnum)
    (sb-ext:simd-pack-256 double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown aset4-ps ((simple-array single-float (*))
                    fixnum
                    (sb-kernel:simd-pack single-float))
    (sb-kernel:simd-pack single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown aset8-ps ((simple-array single-float (*))
                    fixnum
                    (sb-ext:simd-pack-256 single-float))
    (sb-ext:simd-pack-256 single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown aset2-pd ((simple-array double-float (*))
                    fixnum
                    (sb-kernel:simd-pack double-float))
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown aset4-pd ((simple-array double-float (*))
                    fixnum
                    (sb-ext:simd-pack-256 double-float))
    (sb-ext:simd-pack-256 double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f4*-ps ((sb-kernel:simd-pack single-float)
                  (sb-kernel:simd-pack single-float))
    (sb-kernel:simd-pack single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f8*-ps ((sb-ext:simd-pack-256 single-float)
                  (sb-ext:simd-pack-256 single-float))
    (sb-ext:simd-pack-256 single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f2*-pd ((sb-kernel:simd-pack double-float)
                  (sb-kernel:simd-pack double-float))
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f4*-pd ((sb-ext:simd-pack-256 double-float)
                  (sb-ext:simd-pack-256 double-float))
    (sb-ext:simd-pack-256 double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f4+-ps ((sb-kernel:simd-pack single-float)
                  (sb-kernel:simd-pack single-float))
    (sb-kernel:simd-pack single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f8+-ps ((sb-ext:simd-pack-256 single-float)
                  (sb-ext:simd-pack-256 single-float))
    (sb-ext:simd-pack-256 single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f2+-pd ((sb-kernel:simd-pack double-float)
                  (sb-kernel:simd-pack double-float))
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown f4+-pd ((sb-ext:simd-pack-256 double-float)
                  (sb-ext:simd-pack-256 double-float))
    (sb-ext:simd-pack-256 double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown setzero4-ps ()
    (sb-kernel:simd-pack single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown setzero8-ps ()
    (sb-ext:simd-pack-256 single-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown setzero2-pd ()
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown setzero4-pd ()
    (sb-ext:simd-pack-256 double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)


(in-package :sb-vm)

(define-vop (cl3a.utilities_vop::aref4-ps)
  (:translate cl3a.utilities_vop::aref4-ps)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (i :scs (signed-reg)))
  (:arg-types simple-array-single-float tagged-num)
  (:results (r :scs (single-sse-reg)))
  (:result-types simd-pack-single)
  (:generator
   5
   (inst movups r (make-ea-for-float-ref
                   x i 0 4 :scale (ash 8 (- n-fixnum-tag-bits))))))

(define-vop (cl3a.utilities_vop::aref8-ps)
  (:translate cl3a.utilities_vop::aref8-ps)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (i :scs (signed-reg)))
  (:arg-types simple-array-single-float tagged-num)
  (:results (r :scs (single-avx2-reg)))
  (:result-types simd-pack-256-single)
  (:generator
   5
   (inst vmovups r (make-ea-for-float-ref
                    x i 0 4 :scale (ash 8 (- n-fixnum-tag-bits))))))

(define-vop (cl3a.utilities_vop::aref2-pd)
  (:translate cl3a.utilities_vop::aref2-pd)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (i :scs (signed-reg)))
  (:arg-types simple-array-double-float tagged-num)
  (:results (r :scs (double-sse-reg)))
  (:result-types simd-pack-double)
  (:generator
   7
   (inst movupd r
         (make-ea-for-float-ref
          x i 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))))

(define-vop (cl3a.utilities_vop::aref4-pd)
  (:translate cl3a.utilities_vop::aref4-pd)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (i :scs (signed-reg)))
  (:arg-types simple-array-double-float tagged-num)
  (:results (r :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator
   7
   (inst vmovupd r
         (make-ea-for-float-ref
          x i 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits))))))

(define-vop (cl3a.utilities_vop::aset4-ps)
  (:translate cl3a.utilities_vop::aset4-ps)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (i :scs (signed-reg))
         (src :scs (single-sse-reg)))
  (:arg-types simple-array-single-float tagged-num simd-pack-single)
  (:results (r :scs (single-sse-reg)))
  (:result-types simd-pack-single)
  (:generator
   5
   (inst movups
         (make-ea-for-float-ref
          x i 0 4 :scale (ash 8 (- n-fixnum-tag-bits)))
         src)
   (move r src)))

(define-vop (cl3a.utilities_vop::aset8-ps)
  (:translate cl3a.utilities_vop::aset8-ps)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (i :scs (signed-reg))
         (src :scs (single-avx2-reg)))
  (:arg-types simple-array-single-float tagged-num simd-pack-256-single)
  (:results (r :scs (single-avx2-reg)))
  (:result-types simd-pack-256-single)
  (:generator
   5
   (inst vmovups
         (make-ea-for-float-ref
          x i 0 4 :scale (ash 8 (- n-fixnum-tag-bits)))
         src)
   (move r src)))

(define-vop (cl3a.utilities_vop::aset2-pd)
  (:translate cl3a.utilities_vop::aset2-pd)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (i :scs (signed-reg))
         (src :scs (double-sse-reg) :target r))
  (:arg-types simple-array-double-float tagged-num simd-pack-double)
  (:results (r :scs (double-sse-reg) :from (:argument 3)))
  (:result-types simd-pack-double)
  (:generator
   7
   (when (not (location= r src))
     (move r src))
   (inst movupd
         (make-ea-for-float-ref
          x i 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
         r)))

(define-vop (cl3a.utilities_vop::aset4-pd)
  (:translate cl3a.utilities_vop::aset4-pd)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (i :scs (signed-reg))
         (src :scs (double-avx2-reg) :target r))
  (:arg-types simple-array-double-float tagged-num simd-pack-256-double)
  (:results (r :scs (double-avx2-reg) :from (:argument 3)))
  (:result-types simd-pack-256-double)
  (:generator
   7
   (when (not (location= r src))
     (move r src))
   (inst vmovupd
         (make-ea-for-float-ref
          x i 0 8 :scale (ash 2 (- word-shift n-fixnum-tag-bits)))
         r)))

(define-vop (cl3a.utilities_vop::f4*-ps)
  (:translate cl3a.utilities_vop::f4*-ps)
  (:policy :fast-safe)
  (:args (x :scs (single-sse-reg))
         (y :scs (single-sse-reg)))
  (:arg-types simd-pack-single simd-pack-single)
  (:results (dst :scs (single-sse-reg) :from (:argument 0)))
  (:result-types simd-pack-single)
  (:generator 4
              (move dst x)
              (inst mulps dst y)))

(define-vop (cl3a.utilities_vop::f8*-ps)
  (:translate cl3a.utilities_vop::f8*-ps)
  (:policy :fast-safe)
  (:args (x :scs (single-avx2-reg))
         (y :scs (single-avx2-reg)))
  (:arg-types simd-pack-256-single simd-pack-256-single)
  (:results (dst :scs (single-avx2-reg) :from (:argument 0)))
  (:result-types simd-pack-256-single)
  (:generator 4
              (inst vmulps dst x y)))

(define-vop (cl3a.utilities_vop::f2*-pd)
  (:translate cl3a.utilities_vop::f2*-pd)
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

(define-vop (cl3a.utilities_vop::f4*-pd)
  (:translate cl3a.utilities_vop::f4*-pd)
  (:policy :fast-safe)
  (:args (x :scs (double-avx2-reg))
         (y :scs (double-avx2-reg)))
  (:arg-types simd-pack-256-double simd-pack-256-double)
  (:results (dst :scs (double-avx2-reg) :from (:argument 0)))
  (:result-types simd-pack-256-double)
  (:generator
   4
   (inst vmulpd dst x y)))

(define-vop (cl3a.utilities_vop::f4+-ps)
  (:translate cl3a.utilities_vop::f4+-ps)
  (:policy :fast-safe)
  (:args (x :scs (single-sse-reg))
         (y :scs (single-sse-reg)))
  (:arg-types simd-pack-single simd-pack-single)
  (:results (dst :scs (single-sse-reg) :from (:argument 0)))
  (:result-types simd-pack-single)
  (:generator 4
              (move dst x)
              (inst addps dst y)))

(define-vop (cl3a.utilities_vop::f8+-ps)
  (:translate cl3a.utilities_vop::f8+-ps)
  (:policy :fast-safe)
  (:args (x :scs (single-avx2-reg))
         (y :scs (single-avx2-reg)))
  (:arg-types simd-pack-256-single simd-pack-256-single)
  (:results (dst :scs (single-avx2-reg) :from (:argument 0)))
  (:result-types simd-pack-256-single)
  (:generator 4
              (inst vaddps dst x y)))

(define-vop (cl3a.utilities_vop::f2+-pd)
  (:translate cl3a.utilities_vop::f2+-pd)
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

(define-vop (cl3a.utilities_vop::f4+-pd)
  (:translate cl3a.utilities_vop::f4+-pd)
  (:policy :fast-safe)
  (:args (x :scs (double-avx2-reg))
         (y :scs (double-avx2-reg)))
  (:arg-types simd-pack-256-double simd-pack-256-double)
  (:results (dst :scs (double-avx2-reg) :from (:argument 0)))
  (:result-types simd-pack-256-double)
  (:generator
   4
   (inst vaddpd dst x y)))

(define-vop (cl3a.utilities_vop::setzero4-ps)
  (:translate cl3a.utilities_vop::setzero4-ps)
  (:policy :fast-safe)
  (:results (r :scs (single-sse-reg)))
  (:result-types simd-pack-single)
  (:generator
   5
   (inst xorps r r)))

(define-vop (cl3a.utilities_vop::setzero8-ps)
  (:translate cl3a.utilities_vop::setzero8-ps)
  (:policy :fast-safe)
  (:results (r :scs (single-avx2-reg)))
  (:result-types simd-pack-256-single)
  (:generator
   5
   (inst vxorps r r r)))

(define-vop (cl3a.utilities_vop::setzero2-pd)
  (:translate cl3a.utilities_vop::setzero2-pd)
  (:policy :fast-safe)
  (:results (r :scs (double-sse-reg)))
  (:result-types simd-pack-double)
  (:generator
   5
   (inst xorpd r r)))

(define-vop (cl3a.utilities_vop::setzero4-pd)
  (:translate cl3a.utilities_vop::setzero4-pd)
  (:policy :fast-safe)
  (:results (r :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator
   5
   (inst vxorpd r r r)))


(in-package :cl3a.utilities_vop)

(defun aref4-ps (x i)
  (aref4-ps x i))

(defun aref8-ps (x i)
  (aref8-ps x i))

(defun aref2-pd (x i)
  (aref2-pd x i))

(defun aref4-pd (x i)
  (aref4-pd x i))

(defun aset4-ps (x i src)
  (aset4-ps x i src))

(defun aset8-ps (x i src)
  (aset8-ps x i src))

(defun aset2-pd (x i src)
  (aset2-pd x i src))

(defun aset4-pd (x i src)
  (aset4-pd x i src))

(defsetf aref4-ps (x i) (val)
  `(aset4-ps ,x ,i ,val))

(defsetf aref8-ps (x i) (val)
  `(aset8-ps ,x ,i ,val))

(defsetf aref2-pd (x i) (val)
  `(aset2-pd ,x ,i ,val))

(defsetf aref4-pd (x i) (val)
  `(aset4-pd ,x ,i ,val))

(defun f4*-ps (x y)
  (f4*-ps x y))

(defun f8*-ps (x y)
  (f8*-ps x y))

(defun f2*-pd (x y)
  (f2*-pd x y))

(defun f4*-pd (x y)
  (f4*-pd x y))

(defun f4+-ps (x y)
  (f4+-ps x y))

(defun f8+-ps (x y)
  (f8+-ps x y))

(defun f2+-pd (x y)
  (f2+-pd x y))

(defun f4+-pd (x y)
  (f4+-pd x y))

(defun setzero4-ps ()
  (setzero4-ps))

(defun setzero8-ps ()
  (setzero8-ps))

(defun setzero2-pd ()
  (setzero2-pd))

(defun setzero4-pd ()
  (setzero4-pd))
