(in-package :cl-user)
(defpackage cl3a-test
  (:use :cl :prove :cl3a :naive-funcs))
(in-package #:cl3a-test)


(plan 2)


(defun ddotprod-test (n)
  (declare (type integer n))
  (let ((va (make-array (list n) :element-type 'double-float))
        (vb (make-array (list n) :element-type 'double-float)))
    (declare (type (simple-array double-float (*)) va vb))
    (dotimes (i n)
      (setf (aref va i) (random 1d0))
      (setf (aref vb i) (random 1d0)))
    (let ((res1 (coerce (cl3a:dv*v va vb) 'single-float))
          (res2 (coerce (nf:dv*v-naive va vb) 'single-float)))
      (declare (type single-float res1 res2))
      (= res1 res2))))
(ok (ddotprod-test 5000))


(defun dnorm-test (n)
  (declare (type integer n))
  (let ((va (make-array (list n) :element-type 'double-float)))
    (declare (type (simple-array double-float (*)) va))
    (dotimes (i n)
      (setf (aref va i) (random 1d0)))
    (let ((res1 (coerce (cl3a:dnorm va) 'single-float))
          (res2 (coerce (nf:dnorm-naive va) 'single-float)))
      (declare (type single-float res1 res2))
      (= res1 res2))))
(ok (dnorm-test 5000))


(finalize)
