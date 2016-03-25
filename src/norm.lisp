(in-package :cl-user)
(defpackage cl3a.norm
  (:use :cl :cl3a.dotprod)
  (:export :dnorm :lnorm))
(in-package :cl3a.norm)


(declaim (ftype (function ((simple-array double-float (*)))
                          double-float)
                dnorm))
(defun dnorm (va)
  "Euclidean norm of vector"
  (declare (type (simple-array double-float (*)) va))
  (let ((norm2 (dv*v va va)))
    (declare (type (double-float 0d0 *) norm2))
    (sqrt norm2)))


(declaim (ftype (function ((simple-array long-float (*)))
                          long-float)
                lnorm))
(defun lnorm (va)
  "Euclidean norm of vector"
  (declare (type (simple-array long-float (*)) va))
  (let ((norm2 (lv*v va va)))
    (declare (type (long-float 0l0 *) norm2))
    (sqrt norm2)))
