(in-package :cl-user)
(defpackage cl3a.mvmult
  (:use :cl :sb-ext :sb-c :alexandria :cl3a.utilities :cl3a.utilities_vop :cl3a.mvmult_vop)
  (:export :sm*v :dm*v))
(in-package :cl3a.mvmult)


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (*))
                           (simple-array double-float (*))))
                dm*v))
(defun dm*v (matA vb vc)
  "Multiply matrix and vector of double-float"
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) matA)
           (type (simple-array double-float (*)) vb vc))
  (let* ((m (array-dimension matA 0))
         (k (array-dimension matA 1))
         (m0 (min-factor m 2))
         (k0 (min-factor k 4)))
    (declare (type fixnum m k m0 k0))
    (do ((i 0 (+ i 2)))
        ((>= i m0))
      (declare (type fixnum i))
      (let ((idx (array-row-major-index matA i 0)))
        (declare (type fixnum idx))
        (sb-sys:%primitive
         aset2-pd vc i
                 (f2+-pd (mvi2x4-pd k k0 idx (sb-kernel:%array-data-vector matA) vb (aref2-pd vc i))
                         (loop :for p :of-type fixnum :from k0 :below k
                               :sum (* (aref matA i p) (aref vb p))
                                 :into c0 :of-type double-float
                               :sum (* (aref matA (1+ i) p) (aref vb p))
                                 :into c1 :of-type double-float
                               :finally
                                  (return (sb-kernel:%make-simd-pack-double c0 c1)))))))
    (do ((i m0 (1+ i)))
        ((>= i m))
      (declare (type fixnum i))
      (incf (aref vc i)
            (loop :for p :of-type fixnum :below k
               :sum (* (aref matA i p) (aref vb p))
               :into c :of-type double-float
               :finally (return c))))))


(declaim (ftype (function ((simple-array single-float (* *))
                           (simple-array single-float (*))
                           (simple-array single-float (*))))
                sm*v))
;; (defun sm*v (ma vb vc)
;;   "Multiply matrix and vector of long-float"
;;   (declare (optimize (speed 3))
;;            (type (simple-array long-float (* *)) ma)
;;            (type (simple-array long-float (*)) vb vc))
;;   (m*v long-float ma vb vc))
