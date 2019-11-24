(in-package :cl-user)
(defpackage cl3a.norm
  (:use :cl :cl3a.typedef :cl3a.dotprod)
  (:export :snorm :dnorm))
(in-package :cl3a.norm)


(declaim (ftype (function ((simple-array single-float (*)))
                          single-float)
                snorm))
(defun snorm (va)
  "Euclidean norm of vector"
  (declare (type (simple-array single-float (*)) va))
  (let ((norm2 (sv*v va va)))
    (declare (type (single-float 0.0 *) norm2))
    (sqrt norm2)))


(declaim (ftype (function ((vec double-float))
                          double-float)
                dnorm))
(defun dnorm (va)
  "Euclidean norm of vector"
  (declare (type (vec double-float) va))
  (let ((norm2 (dv*v va va)))
    (declare (type (double-float 0d0 *) norm2))
    (sqrt norm2)))
