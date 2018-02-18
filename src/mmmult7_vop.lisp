(in-package :cl-user)
(defpackage cl3a.mmmult7vop
  (:use :cl :sb-ext :sb-c :sb-vm)
  (:export :f2+ :f2* :simd-sum))
(in-package :cl3a.mmmult7vop)


(defknown (f2+ f2*) ((sb-kernel:simd-pack double-float)
                     (sb-kernel:simd-pack double-float))
    (sb-kernel:simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown simd-sum ((sb-kernel:simd-pack double-float))
    double-float
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

;; (defknown load2-sse-from-array ((simple-array double-float (*))
;;                                  fixnum)
;;     (sb-kernel:simd-pack double-float)
;;     (movable flushable always-translatable)
;;   :overwrite-fndb-silently t)

;; (defknown store2-sse-to-array ((simple-array double-float (*))
;;                                fixnum
;;                                (sb-kernel:simd-pack double-float))
;;     (simple-array double-float (*))
;;     (movable flushable always-translatable)
;;   :overwrite-fndb-silently t)


(in-package :sb-vm)

(define-vop (cl3a.mmmult7vop::f2+)
  (:translate cl3a.mmmult7vop::f2+)
  (:policy :fast-safe)
  (:args (x :scs (double-sse-reg) :target res)
         (y :scs (double-sse-reg)))
  (:arg-types simd-pack-double simd-pack-double)
  (:results (res :scs (double-sse-reg)))
  (:result-types simd-pack-double)
  (:generator 4
    (cond ((location= res y)
           (inst addpd y x))
          (t
           (move res x)
           (inst addpd res y)))))

(define-vop (cl3a.mmmult7vop::f2*)
  (:translate cl3a.mmmult7vop::f2*)
  (:policy :fast-safe)
  (:args (x :scs (double-sse-reg) :target res)
         (y :scs (double-sse-reg)))
  (:arg-types simd-pack-double simd-pack-double)
  (:results (res :scs (double-sse-reg)))
  (:result-types simd-pack-double)
  (:generator 4
    (cond ((location= res y)
           (inst mulpd y x))
          (t
           (move res x)
           (inst mulpd res y)))))


(define-vop (cl3a.mmmult7vop::simd-sum)
  (:translate cl3a.mmmult7vop::simd-sum)
  (:policy :fast-safe)
  (:args (x :scs (double-sse-reg) :target res))
  (:arg-types simd-pack-double)
  (:results (res :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:sc double-reg) tmp)
  (:generator 3
    (cond ((location= res x)
           (move tmp x)
           (inst xorpd res res)
           (inst movsd res tmp)
           (inst psrldq tmp 8)
           (inst addsd res tmp))
          (t
           (move tmp x)
           (inst psrldq tmp 8)
           (inst xorpd res res)
           (inst movsd res x)
           (inst addsd res tmp)))))

;; (define-vop (cl3a.mmmult7::load2-sse-from-array)
;;   (:translate cl3a.mmmult7::load2-sse-from-array)
;;   (:policy :fast-safe)
;;   (:args (object :scs (descriptor-reg) :target res)
;;          (index :scs (any-reg immediate)))
;;   (:arg-types simple-array-double-float tagged-num)
;;   (:temporary (:sc double-reg) tmp)
;;   (:results (res :scs (double-sse-reg) :from (:argument 0)))
;;   (:result-types simd-pack-double)
;;   (:generator 7
;;     (inst movsd res
;;           (make-ea-for-float-ref object index 0 8
;;                                  :scale (ash 1 (- word-shift
;;                                                   n-fixnum-tag-bits))))
;;     (inst movsd tmp 
;;           (make-ea-for-float-ref object index 1 8
;;                                  :scale (ash 1 (- word-shift
;;                                                   n-fixnum-tag-bits))))
;;     (inst unpcklpd res tmp)))

;; (define-vop (cl3a.mmmult7::store2-sse-to-array)
;;   (:translate cl3a.mmmult7::store2-sse-to-array)
;;   (:policy :fast-safe)
;;   (:args (object :scs (descriptor-reg))
;;          (index :scs (any-reg immediate))
;;          (value :scs (double-sse-reg)))
;;   (:arg-types simple-array-double-float tagged-num simd-pack-double)
;;   (:temporary (:sc double-reg) tmp)
;;   (:generator 20
;;     (inst xorpd tmp tmp)
;;     (move tmp value)
;;     (inst movsd
;;           (make-ea-for-float-ref object index 0 8
;;                                  :scale (ash 1 (- word-shift
;;                                                   n-fixnum-tag-bits)))
;;           tmp)
;;     (inst psrldq tmp 8)
;;     (inst movsd
;;           (make-ea-for-float-ref object index 1 8
;;                                  :scale (ash 1 (- word-shift
;;                                                   n-fixnum-tag-bits)))
;;           tmp)))


(in-package :cl3a.mmmult7vop)

(defun f2+ (x y)
  (f2+ x y))

(defun f2* (x y)
  (f2* x y))

(defun simd-sum (x)
  (simd-sum x))
