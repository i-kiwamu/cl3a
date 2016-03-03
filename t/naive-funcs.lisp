(in-package :cl-user)
(defpackage naive-funcs
  (:use :cl :prove :cl3a)
  (:nicknames nf)
  (:export :dv*v-naive
           :dnorm-naive))
(in-package #:naive-funcs)


(declaim (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                dv*v-naive))
(defun dv*v-naive (va vb)
  (declare (type (simple-array double-float (*)) va vb))
  (let ((nv (min (length va) (length vb)))
        (res 0d0))
    (declare (type fixnum nv))
    (dotimes (i nv res)
      (incf res (* (aref va i) (aref vb i))))))


(declaim (ftype (function ((simple-array double-float (*)))
                          double-float)
                dnorm-naive))
(defun dnorm-naive (va)
  (declare (type (simple-array double-float (*)) va))
  (sqrt (dv*v-naive va va)))
