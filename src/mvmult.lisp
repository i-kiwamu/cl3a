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
         ;; (k0 (min-factor k 4)))
         (k0 (min-factor k 8)))
    (declare (type fixnum m k m0 k0))
    (do ((i 0 (+ i 2)))
        ((>= i m0))
      (declare (type fixnum i))
      (let ((idx (array-row-major-index matA i 0)))
        (declare (type fixnum idx))
        (multiple-value-bind (r0 r1 r2 r3)
            (%simd-pack-256-doubles
             (mvi2x8-pd k k0 idx (sb-kernel:%array-data-vector matA) vb))
          (incf (aref vc i)
                (+ r0 r2
                   (loop :for p :of-type fixnum :from k0 :below k
                         :sum (* (aref matA i p) (aref vb p))
                           :into c :of-type double-float
                         :finally (return c))))
          (incf (aref vc (1+ i))
                (+ r1 r3
                   (loop :for p :of-type fixnum :from k0 :below k
                         :sum (* (aref matA (1+ i) p) (aref vb p))
                           :into c :of-type double-float
                         :finally (return c)))))))
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
