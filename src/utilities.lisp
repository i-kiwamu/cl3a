(in-package :cl-user)
(defpackage cl3a.utilities
  (:use :cl :trivial-types)
  (:export :ifloor
           :min-factor
           :dvvs-calc-within-L1
           :different-length-warn))
(in-package :cl3a.utilities)


(defparameter *L1-size* (the fixnum 32768))


(declaim (ftype (function (integer integer) integer) ifloor))
(defun ifloor (x y)
  "Take integer part of floor function"
  (declare (type integer x y))
  (let* ((m (mod x y))
         (x0 (- x m)))
    (declare (type integer m x0))
    (if (= m 0) (/ x y) (/ x0 y))))


(declaim (ftype (function (integer integer) integer) min-factor))
(defun min-factor (x y)
  (declare (type integer x y))
  (let* ((m (mod x y)))
    (declare (type integer m))
    (if (= m 0) (- x y) (- x m))))


(declaim (ftype (function ((function (fixnum fixnum fixnum
                                      (simple-array double-float (*))
                                      (simple-array double-float (*)))
                                     double-float)
                           fixnum
                           (simple-array double-float (*))
                           (simple-array double-float (*)))
                          (proper-list double-float))
                dvvs-calc-within-L1))
(defun dvvs-calc-within-L1 (fun n xa xb)
  "Divide data into small parts (< L1 cache size), and calculate.
   fun should have arguments of 
   position (fixnum), length for calc (fixnum), vector length (fixnum),
   vector xa (simple-array double-float (*)),
   vector xb (simple-array double-float (*))"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type (function (fixnum fixnum fixnum
                            (simple-array double-float (*))
                            (simple-array double-float (*)))
                           double-float) fun)
           (type fixnum n)
           (type (simple-array double-float (*)) xa xb))
  (let* ((m (ifloor *L1-size* 8))  ; calc length at a time (8-octet-vector)
         (nres (ifloor n m))  ; number of calc
         (k (* nres m))
         (res (if (> n k)  ; n = k if (mod n m) = 0
                  (make-list (1+ nres) :initial-element 0d0)
                  (make-list nres :initial-element 0d0))))
    (declare (type fixnum m nres k)
             (type (proper-list double-float) res))
    (do ((i 0 (the fixnum (+ i m)))
         (ir 0 (the fixnum (1+ ir))))
        ((>= (the fixnum i) k))
      (setf (nth ir res) (funcall fun i m n xa xb)))
    (when (> n k)
      (let ((nk (- n k)))
        (declare (type fixnum nk))
        (setf (nth nres res) (funcall fun k nk n xa xb))))
    res))


(declaim (inline different-length-warn))
(defun different-length-warn (na nb)
  "Warn different lengths"
  (declare (optimize (speed 3))
           (type fixnum na nb))
  (warn (format nil "Length of two vectors were different (~D and ~D). Shorter one is used." na nb)))

