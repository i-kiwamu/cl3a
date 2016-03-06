(in-package :cl-user)
(defpackage cl3a-test
  (:use :cl :prove :cl3a :naive-funcs))
(in-package #:cl3a-test)


(plan 3)


(defun ddotprod-test (n)
  (declare (type integer n))
  (let ((va (make-array (list n) :element-type 'double-float))
        (vb (make-array (list n) :element-type 'double-float)))
    (declare (type (simple-array double-float (*)) va vb))
    (dotimes (i n)
      (setf (aref va i) (random 1d0))
      (setf (aref vb i) (random 1d0)))
    (let ((res1 (coerce (cl3a:dv*v va vb) 'single-float))
          (res2 (coerce (naive:dv*v va vb) 'single-float)))
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
          (res2 (coerce (naive:dnorm va) 'single-float)))
      (declare (type single-float res1 res2))
      (= res1 res2))))
(ok (dnorm-test 5000))


(defun dadd-test (n)
  (declare (type integer n))
  (let ((va (make-array (list n) :element-type 'double-float))
        (vb (make-array (list n) :element-type 'double-float))
        (a (random 1d0))
        (b (random 1d0)))
    (declare (type (simple-array double-float (*)) va vb)
             (type double-float a b))
    (dotimes (i n)
      (setf (aref va i) (random 1d0))
      (setf (aref vb i) (random 1d0)))
    (let ((res1 (cl3a:dv+v a va b vb))
          (res2 (naive:dv+v a va b vb))
          (test t))
      (declare (type (simple-array double-float (*)) res1 res2)
               (type boolean test))
      (dotimes (i n)
        (let ((r1 (coerce (aref res1 i) 'single-float))
              (r2 (coerce (aref res2 i) 'single-float)))
          (when (= r1 r2)
            (setf test (and test t)))))
      test)))
(ok (dadd-test 5000))

(finalize)
