(in-package :cl-user)
(defpackage naive-funcs
  (:use :cl :prove :cl3a)
  (:nicknames naive)
  (:export :dv*v
           :dnorm
           :dv+v))
(in-package #:naive-funcs)


(declaim (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                dv*v-naive))
(defun dv*v (va vb)
  (declare (type (simple-array double-float (*)) va vb))
  (let ((nv (min (length va) (length vb)))
        (res 0d0))
    (declare (type fixnum nv))
    (dotimes (i nv res)
      (incf res (* (aref va i) (aref vb i))))))


(declaim (ftype (function ((simple-array double-float (*)))
                          double-float)
                dnorm-naive))
(defun dnorm (va)
  (declare (type (simple-array double-float (*)) va))
  (sqrt (dv*v va va)))


(defun dv+v (a va b vb)
  (declare (type (simple-array double-float (*)) va vb)
           (type double-float a b))
  (let* ((nv (min (length va) (length vb)))
         (res (make-array (list nv) :element-type 'double-float)))
    (dotimes (i nv)
      (setf (aref res i) (+ (* a (aref va i))
                            (* b (aref vb i)))))
    res))
