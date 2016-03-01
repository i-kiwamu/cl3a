(in-package :cl-user)
(defpackage cl3a.dnorm
  (:use :cl)
  (:import-from :cl3a.utilities
                :dvvs-calc-within-L1
                :different-length-warn)
  (:import-from :cl3a.ddotprod
                :dv*v)
  (:export :dnorm))
(in-package :cl3a.dnorm)


(declaim (inline dnorm)
         (ftype (function ((simple-array double-float (*)))
                          double-float)
                dnorm))
(defun dnorm (va)
  "Euclidean norm of vector"
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (type (simple-array double-float (*)) va))
  (let ((norm2 (dv*v va va)))
    (declare (type (double-float 0d0 *) norm2))
    (sqrt norm2)))
