(in-package :cl-user)
(defpackage cl3a.utilities
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:export :+L1-size+
           :+L2-size+
           :different-length-warn
           :ifloor
           :min-factor
           :dotimes-unroll
           :dotimes-interval
           :type-byte-length))
(in-package :cl3a.utilities)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +L1-size+ (the fixnum 32768))
  (defconstant +L2-size+ (the fixnum 262144))
  (defconstant +unroll+ (the fixnum 5)))


(defun different-length-warn (na nb)
  "Warn different lengths"
  (declare (type integer na nb))
  (warn (format nil "Length of two vectors were different (~D and ~D). Shorter one is used." na nb)))


(declaim (ftype (function (integer integer &rest integer) integer)
                ifloor))
(defun ifloor (x y0 &rest ys)
  "Take integer part of floor function"
  (declare (type integer x y0)
           (type (proper-list integer) ys))
  (let* ((y (reduce #'* ys :initial-value y0))
         (m (mod x y))
         (x0 (- x m)))
    (declare (type integer y m x0))
    (/ x0 y)))


(declaim (ftype (function (integer integer &rest integer) integer)
                min-factor))
(defun min-factor (x y0 &rest ys)
  (declare (type integer x y0)
           (type (proper-list integer) ys))
  (let* ((y (reduce #'* ys :initial-value y0))
         (m (mod x y)))
    (declare (type integer y m))
    (- x m)))


(defmacro dotimes-unroll ((i p n) &body body)
  "Loop for i from p to n with unrolling of +unroll+"
  (with-gensyms (nu maxi)
    `(let ((,nu (min-factor ,n +unroll+)))
       (declare (type fixnum ,nu))
       (cond ((< ,n +unroll+) (do ((,i ,p (1+ ,i)))
                                  ((>= ,i ,n))
                                ,@body))
             (t (let ((,maxi
                       (do ((,i ,p (1+ ,i)))
                           ((>= ,i ,nu) ,i)
                         ,@(loop :repeat (1- +unroll+)
                              :append (append body `((incf ,i))))
                         ,@body)))
                  (declare (type fixnum ,maxi))
                  (when (< ,maxi ,n)
                    (do ((,i ,maxi (1+ ,i)))
                        ((>= ,i ,n))
                      ,@body))))))))


(defmacro dotimes-interval ((i m n) &body body)
  (with-gensyms (n0)
    `(let ((,n0 (min-factor ,n ,m)))
       (declare (type fixnum ,n0))
       (do ((,i 0 (the fixnum (+ ,i ,m))))
           ((>= (the fixnum ,i) ,n0))
         ,@body)
       (when (> (the fixnum ,n) ,n0)
         (let ((,i ,n0)
               (,m (- ,n ,n0)))
           (declare (type fixnum ,i ,m))
           ,@body)))))


(declaim (ftype (function (symbol) fixnum) type-byte-length))
(defun type-byte-length (val-type)
  (ecase val-type
    (short-float 2)
    (single-float 4)
    (double-float 8)
    (long-float 16)))
