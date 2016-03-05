(in-package :cl-user)
(defpackage cl3a.utilities
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:export :ifloor
           :min-factor
           :dvvs-calc-within-L1
           :lvvs-calc-within-L1
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


(declaim (ftype (function (symbol) integer) type-byte-length))
(defun type-byte-length (val-type)
  (ecase val-type
    (short-float 2)
    (single-float 4)
    (double-float 8)
    (long-float 16)))


(defmacro vvs-calc-within-L1 (val-type fun n va vb)
  "Divide data into small parts (< L1 cache size), and calculate.
   fun should have arguments of 
   position (fixnum), length for calc (fixnum), vector length (fixnum),
   vector va (simple-array double-float (*)),
   vector vb (simple-array double-float (*))"
  (with-gensyms (tbl m nres k fzero res i ir nk)
    `(let* ((,tbl (type-byte-length (quote ,val-type)))
            (,m (ifloor *L1-size* ,tbl))  ; calc length at a time
            (,nres (ifloor ,n ,m))  ; number of calc
            (,k (* ,nres ,m))
            (,fzero (coerce 0.0 (quote ,val-type)))
            (,res (if (> ,n ,k)  ; n = k if (mod n m) = 0
                      (make-list (1+ ,nres) :initial-element ,fzero)
                      (make-list ,nres :initial-element ,fzero))))
       (declare (type fixnum ,tbl ,m ,nres ,k)
                (type ,val-type ,fzero)
                (type (proper-list ,val-type) ,res))
       (do ((,i 0 (the fixnum (+ ,i ,m)))
            (,ir 0 (the fixnum (1+ ,ir))))
           ((>= (the fixnum ,i) ,k))
         (setf (nth ,ir ,res) (funcall ,fun ,i ,m ,n ,va ,vb)))
       (when (> ,n ,k)
         (let ((,nk (- ,n ,k)))
           (declare (type fixnum ,nk))
           (setf (nth ,nres ,res) (funcall ,fun ,k ,nk ,n ,va ,vb))))
       ,res)))


(declaim (ftype (function ((function (fixnum fixnum fixnum
                                      (simple-array double-float (*))
                                      (simple-array double-float (*)))
                                     double-float)
                           fixnum
                           (simple-array double-float (*))
                           (simple-array double-float (*)))
                          (proper-list double-float))
                dvvs-calc-within-L1))
(defun dvvs-calc-within-L1 (fun n va vb)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type (function (fixnum fixnum fixnum
                            (simple-array double-float (*))
                            (simple-array double-float (*)))
                           double-float) fun)
           (type fixnum n)
           (type (simple-array double-float (*)) va vb))
  (vvs-calc-within-L1 double-float fun n va vb))


(declaim (ftype (function ((function (fixnum fixnum fixnum
                                      (simple-array long-float (*))
                                      (simple-array long-float (*)))
                                     long-float)
                           fixnum
                           (simple-array long-float (*))
                           (simple-array long-float (*)))
                          (proper-list long-float))
                lvvs-calc-within-L1))
(defun lvvs-calc-within-L1 (fun n va vb)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type (function (fixnum fixnum fixnum
                            (simple-array long-float (*))
                            (simple-array long-float (*)))
                           long-float) fun)
           (type fixnum n)
           (type (simple-array long-float (*)) va vb))
  (vvs-calc-within-L1 long-float fun n va vb))


(declaim (inline different-length-warn))
(defun different-length-warn (na nb)
  "Warn different lengths"
  (declare (type fixnum na nb))
  (warn (format nil "Length of two vectors were different (~D and ~D). Shorter one is used." na nb)))

