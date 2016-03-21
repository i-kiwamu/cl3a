(in-package :cl-user)
(defpackage cl3a.utilities
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:export :+L1-size+
           :different-length-warn
           :ifloor
           :ifloor-sqrt
           :min-factor
           :dotimes-unroll
           :type-byte-length))
(in-package :cl3a.utilities)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +L1-size+ (the fixnum 32768))
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
    (if (= m 0) (/ x y) (/ x0 y))))


(declaim (ftype (function (integer integer &rest integer) integer)
                ifloor-sqrt))
(defun ifloor-sqrt (x y0 &rest ys)
  "Take integer part of floor function"
  (declare (type integer x y0)
           (type (proper-list integer) ys))
  (let* ((y (reduce #'* ys :initial-value y0))
         (m (mod x y))
         (x0 (- x m)))
    (declare (type integer y m x0))
    (if (= m 0) (isqrt (/ x y)) (isqrt (/ x0 y)))))


(declaim (ftype (function (integer integer &rest integer) integer)
                min-factor))
(defun min-factor (x y0 &rest ys)
  (declare (type integer x y0)
           (type (proper-list integer) ys))
  (let* ((y (reduce #'* ys :initial-value y0))
         (m (mod x y)))
    (declare (type integer y m))
    (if (= m 0) x (- x m))))


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
                         ,@(loop :repeat +unroll+
                              :append (append body `((incf ,i))))
                         (decf ,i))))
                  (declare (type fixnum ,maxi))
                  (when (< ,maxi ,n)
                    (do ((,i ,maxi (1+ ,i)))
                        ((>= ,i ,n))
                      ,@body))))))))


(declaim (ftype (function (symbol) fixnum) type-byte-length))
(defun type-byte-length (val-type)
  (ecase val-type
    (short-float 2)
    (single-float 4)
    (double-float 8)
    (long-float 16)))
