(in-package :cl-user)
(defpackage cl3a.utilities
  (:use :cl :alexandria)
  (:export :+L1-size+
           :+L2-size+
           :+associativity+
           :+unroll+
           :different-length-warn
           :ifloor
           :min-factor
           :dotimes-unroll
           :dotimes-interval
           :type-byte-length
           :block-size))
(in-package :cl3a.utilities)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +L1-size+ (the fixnum 32768))
  (defconstant +L2-size+ (the fixnum 262144))
  (defconstant +associativity+ (the fixnum 8))
  (defconstant +unroll+ (the fixnum 6)))


(defun different-length-warn (na nb)
  "Warn different lengths"
  (declare (type integer na nb))
  (warn (format nil "Length of two vectors were different (~D and ~D). Shorter one is used." na nb)))


(declaim (ftype (function (integer integer &rest integer) integer)
                ifloor))
(defun ifloor (x y0 &rest ys)
  "Take integer part of floor function"
  (declare (type integer x y0)
           (type list ys)
           (dynamic-extent ys))
  (let* ((y (reduce #'* ys :initial-value y0))
         (m (mod x y))
         (x0 (- x m)))
    (declare (type integer y m x0))
    (/ x0 y)))


(declaim (ftype (function (integer integer &rest integer) integer)
                min-factor))
(defun min-factor (x y0 &rest ys)
  (declare (type integer x y0)
           (type list ys)
           (dynamic-extent ys))
  (let* ((y (reduce #'* ys :initial-value y0))
         (m (mod x y)))
    (declare (type integer y m))
    (- x m)))


(defmacro dotimes-unroll ((i p n &optional (unroll +unroll+)) &body body)
  "Loop for i from p to n with unrolling of +unroll+"
  (with-gensyms (nu maxi)
    `(let ((,nu (min-factor (the fixnum (- ,n ,p)) ,unroll)))
       (declare (type fixnum ,nu))
       (let ((,maxi
              (do ((,i ,p (the fixnum (1+ ,i))))
                  ((>= (the fixnum ,i) ,nu) ,i)
                ,@(loop :repeat unroll
                     :append (append body `((incf ,i))))
                (decf ,i))))
         (declare (type fixnum ,maxi))
         ;; if n-p < unroll or n > n0
         (when (< ,maxi ,n)
           (do ((,i ,maxi (the fixnum (1+ ,i))))
               ((>= (the fixnum ,i) ,n))
             ,@body))))))


(defmacro dotimes-interval ((i m n) &body body)
  (with-gensyms (n0)
    `(let ((,n0 (min-factor ,n ,m)))
       (declare (type fixnum ,n0))
       (do ((,i 0 (the fixnum (+ ,i ,m))))
           ((>= (the fixnum ,i) ,n0))
         ,@body)
       (when (> (the fixnum ,n) ,n0)
         ;; execution only once
         (do ((,i ,n0 (the fixnum (1+ ,i))))
             ((>= (the fixnum ,i) (1+ ,n0)))
           ,@body))
       nil)))


(declaim (ftype (function (symbol) fixnum) type-byte-length))
(defun type-byte-length (val-type)
  (ecase val-type
    (short-float 2)
    (single-float 4)
    (double-float 8)
    (long-float 16)))


(declaim (ftype (function (integer) integer) block-size))
(defun block-size (n)
  "See Lam et al. 1991 The cache performance and optimizations of blocked algorithms"
  (declare (type integer n))
  (let ((n-half (ifloor n 2))
        (cache-size (ifloor +L2-size+ 4)))  ;; 1 word = 4 byte
    (declare (type integer n-half cache-size))
    (loop :while t
       :with max-width :of-type integer = (min n cache-size)
       :and addr :of-type integer = n-half
       :and di :of-type integer = 0
       :and dj :of-type integer = 1
       :and di0 :of-type integer = 1
       :do (progn
             (incf addr cache-size)
             (setf di (ifloor addr n))
             (setf dj (abs (- (mod addr n) n-half)))
             (setf di0 (min max-width dj)))
       (when (>= di di0)
         (return (min max-width di)))
       :do (setf max-width di0))))
