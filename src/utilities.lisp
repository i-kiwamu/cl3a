(in-package :cl-user)
(defpackage cl3a.utilities
  (:use :cl :alexandria)
  (:export :+cache-line+
           :+L1-size+
           :+L2-size+
           :+L3-size+
           :+associativity+
           :+unroll+
           :different-length-warn
           :ifloor
           :min-factor
           :dotimes-unroll
           :dotimes-interval
           :dotimes-interval-full
           :dotimes-interval2
           :dotimes-interval2-full
           :type-byte-length
           :copy-matrix
           :copy-matrix-transpose))
(in-package :cl3a.utilities)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cache-line+ (the fixnum 64))
  (defconstant +L1-size+ (the fixnum 32768))
  (defconstant +L2-size+ (the fixnum 262144))
  (defconstant +L3-size+ (the fixnum 4194304))
  (defconstant +associativity+ (the fixnum 8))
  (defconstant +unroll+ (the fixnum 4)))


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
  "calculate x0*y with the minimum m where x = x0*y + m, y = y0*y1*y2*..., and ys = (y1, y2, ...)"
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
  "loop for i from 0 to n with interval of m"
  (with-gensyms (n0)
    `(let ((,n0 (min-factor ,n ,m)))
       (declare (type fixnum ,n0))
       (do ((,i 0 (the fixnum (+ ,i ,m))))
           ((>= (the fixnum ,i) ,n0))
         ,@body)
       nil)))

(defmacro dotimes-interval-full ((i m n) &body body)
  "loop for i from 0 to n with interval of m"
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

(defmacro dotimes-interval2 ((i s n) (m m-val) &body body)
  "loop for i from s to n with interval of m"
  (with-gensyms (n0 iend0)
    `(let* ((,m ,m-val)
            (,n0 (min-factor ,n ,m))
            (,iend0 (+ ,n0 ,s)))
       (declare (type fixnum ,m ,n0 ,iend0))
       (do ((,i ,s (the fixnum (+ ,i ,m))))
           ((>= (the fixnum ,i) ,iend0))
         ,@body)
       nil)))


(defmacro dotimes-interval2-full ((i s n) (m m-val) &body body)
  "loop for i from s to n with interval of m"
  (with-gensyms (n0 iend0)
    `(let* ((,m ,m-val)
            (,n0 (min-factor ,n ,m))
            (,iend0 (+ ,n0 ,s)))
       (declare (type fixnum ,m ,n0 ,iend0))
       (do ((,i ,s (the fixnum (+ ,i ,m))))
           ((>= (the fixnum ,i) ,iend0))
         ,@body)
       (when (> ,n ,n0)
         ;; execution only once
         (setf ,m (- ,n ,n0))
         (let ((,i ,iend0))
           ,@body))
       nil)))


(declaim (ftype (function (symbol) fixnum) type-byte-length))
(defun type-byte-length (val-type)
  "return the size of type"
  (ecase val-type
    (short-float 2)
    (single-float 4)
    (double-float 8)
    (long-float 16)))


(defmacro copy-matrix (ma si ni sj nj mb)
  "copy ma ni rows from si and nj cols from sj into mb"
  (with-gensyms (i j nra nca nrb ncb nr nc rma rmb)
    `(let* ((,nra (array-dimension ,ma 0))
            (,nca (array-dimension ,ma 1))
            (,nrb (array-dimension ,mb 0))
            (,ncb (array-dimension ,mb 1))
            (,nr (min ,ni ,nra ,nrb))
            (,nc (min ,nj ,nca ,ncb)))
       (declare (type fixnum ,nra ,nca ,nrb ,ncb ,nr ,nc))
       (dotimes (,i ,nr)
         (let ((,rma (array-row-major-index ,ma (+ ,i ,si) ,sj))
               (,rmb (array-row-major-index ,mb ,i 0)))
           (declare (type fixnum ,rma ,rmb))
           (dotimes (,j ,nc)
             (setf (row-major-aref ,mb ,rmb)
                   (row-major-aref ,ma ,rma))
             (incf ,rma)
             (incf ,rmb)))))))


(defmacro copy-matrix-transpose (ma si ni sj nj mb)
  (with-gensyms (i j nrb ncb nr nc)
    `(let* ((,nrb (array-dimension ,mb 0))
            (,ncb (array-dimension ,mb 1))
            (,nr (min ,ni ,ncb))
            (,nc (min ,nj ,nrb)))
       (declare (type fixnum ,nrb ,ncb ,nr ,nc))
       (dotimes (,i ,nr)
         (dotimes (,j ,nc)
           (setf (aref ,mb ,j ,i)
                 (aref ,ma (+ ,i ,si) (+ ,j ,sj))))))))
