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
           :dotimes-unroll2
           :dotimes-unroll2-interval
           :dotimes-interval
           :dotimes-interval2
           :dotimes-interval3
           :type-byte-length
           :copy-matrix/generic
           :copy-matrix/double-float
           :copy-matrix/long-float
           :copy-matrix-transpose/double-float
           :let-mat-dotimes-cinterval2
           :let-mat-dotimes-rinterval2))
(in-package :cl3a.utilities)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cache-line+ (the fixnum 64))
  (defconstant +L1-size+ (the fixnum 32768))
  (defconstant +L2-size+ (the fixnum 262144))
  (defconstant +L3-size+ (the fixnum 4194304))
  (defconstant +associativity+ (the fixnum 8))
  (defconstant +unroll+ (the fixnum 8)))


(defun different-length-warn (na nb)
  "Warn different lengths"
  (declare (type fixnum na nb))
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
  (with-gensyms (n0)
    `(let ((,n0 (+ (min-factor (- ,n ,p) ,unroll) ,p)))
       (declare (type fixnum ,n0))
         (do ((,i ,p (1+ ,i)))
             ((>= ,i ,n0))
           ,@(loop :repeat (1- unroll)
                :append (append body `((incf ,i))))
           ,@body)
         (do ((,i ,n0 (1+ ,i)))
             ((>= ,i ,n))
           ,@body))))


(defmacro dotimes-unroll2 ((i p n &optional (unroll +unroll+)) &body body)
  "Loop for i from p to n with unrolling of +unroll+ without updating i"
  (with-gensyms (n0)
    `(let ((,n0 (+ (min-factor (- ,n ,p) ,unroll) ,p)))
       (declare (type fixnum ,n0))
       (do ((,i ,p (+ ,i ,unroll)))
             ((>= ,i ,n0))
         ,@(loop :repeat unroll :append body))
       (do ((,i ,n0 (1+ ,i)))
           ((>= ,i ,n))
         ,@body))))

(defmacro dotimes-unroll2-interval
    ((i p n m &optional (unroll +unroll+)) (&body body1) (&body body2))
  "Loop for i from p to n with unrolling of +unroll+ without updating i"
  (with-gensyms (n0)
    `(let ((,n0 (+ (min-factor (- ,n ,p) ,unroll ,m) ,p)))
       (declare (type fixnum ,n0))
       (do ((,i ,p (the fixnum (+ ,i (* ,unroll ,m)))))
             ((>= ,i ,n0))
         ,@(loop :repeat unroll :append body1))
       (do ((,i ,n0 (1+ ,i)))
           ((>= ,i ,n))
         ,@body2))))


(defmacro dotimes-interval ((i m n) &body body)
  "loop for i from 0 to n with interval of m"
  (with-gensyms (n0)
    `(let ((,n0 (min-factor ,n ,m)))
       (declare (type fixnum ,n0))
       (do ((,i 0 (the fixnum (+ ,i ,m))))
           ((>= (the fixnum ,i) ,n0))
         ,@body)
       (do ((,i ,n0 (the fixnum (1+ ,i))))
           ((>= (the fixnum ,i) (1+ ,n0)))
         ,@body))))


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
       (when (> ,n ,n0)
         ;; execution only once
         (setf ,m (- ,n ,n0))
         (let ((,i ,iend0))
           ,@body)))))


(defmacro dotimes-interval3 ((i s n) (m m-val) (&body body1) (&body body2))
  "loop for i from s to n with interval of m with different body"
  (with-gensyms (n0 iend0)
    `(let* ((,m ,m-val)
            (,n0 (min-factor ,n ,m))
            (,iend0 (+ ,n0 ,s)))
       (declare (type fixnum ,m ,n0 ,iend0))
       (do ((,i ,s (the fixnum (+ ,i ,m))))
           ((>= (the fixnum ,i) ,iend0))
         ,@body1)
       (when (> ,n ,n0)
         ;; execution only once
         (setf ,m (- ,n ,n0))
         (let ((,i ,iend0))
           ,@body2)))))


(declaim (ftype (function (symbol) fixnum) type-byte-length))
(defun type-byte-length (val-type)
  "return the size of type"
  (ecase val-type
    (short-float 2)
    (single-float 4)
    (double-float 8)
    (long-float 16)))


(defmacro copy-matrix/generic (ma si ni sj nj mb)
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


(defmacro copy-matrix-ker (nc ma rma mb rmb)
  (with-gensyms (j)
    `(dotimes-unroll2 (,j 0 ,nc)
       (setf (row-major-aref ,mb ,rmb)
             (row-major-aref ,ma ,rma))
       (incf ,rma)
       (incf ,rmb))))


(declaim (ftype (function ((simple-array double-float (* *))
                           fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))))
                copy-matrix/double-float))
(defun copy-matrix/double-float (ma si ni sj nj mb)
  (declare (type (simple-array double-float (* *)) ma mb)
           (type fixnum si ni sj nj))
  (flet ((copy (nc ma rma mb rmb)
           (declare (optimize (speed 3) (safety 0))
                    (type fixnum nc rma rmb)
                    (type (simple-array double-float (* *)) ma mb))
           (copy-matrix-ker nc ma rma mb rmb)))
    (let ((nr (min ni
                   (array-dimension ma 0)
                   (array-dimension mb 0)))
          (nc (min nj
                   (array-dimension ma 1)
                   (array-dimension mb 1))))
      (declare (type fixnum nr nc))
      (dotimes (i nr)
        (let ((rma (array-row-major-index ma (+ i si) sj))
              (rmb (array-row-major-index mb i 0)))
          (declare (type fixnum rma rmb))
          (copy nc ma rma mb rmb))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           fixnum fixnum fixnum fixnum
                           (simple-array long-float (* *))))
                copy-matrix/long-float))
(defun copy-matrix/long-float (ma si ni sj nj mb)
  (declare (type (simple-array long-float (* *)) ma mb)
           (type fixnum si ni sj nj))
  (flet ((copy (nc ma rma mb rmb)
           (declare (optimize (speed 3) (safety 0))
                    (type fixnum nc rma rmb)
                    (type (simple-array long-float (* *)) ma mb))
           (copy-matrix-ker nc ma rma mb rmb)))
    (let ((nr (min ni
                   (array-dimension ma 0)
                   (array-dimension mb 0)))
          (nc (min nj
                   (array-dimension ma 1)
                   (array-dimension mb 1))))
      (declare (type fixnum nr nc))
      (dotimes (i nr)
        (let ((rma (array-row-major-index ma (+ i si) sj))
              (rmb (array-row-major-index mb i 0)))
          (declare (type fixnum rma rmb))
          (copy nc ma rma mb rmb))))))


(defmacro copy-matrix-transpose-ker (ma si nr sj j mb)
  (with-gensyms (i)
    `(dotimes-unroll (,i 0 ,nr)
       (setf (aref ,mb ,j ,i)
             (aref ,ma (+ ,i ,si) (+ ,j ,sj))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))))
                copy-matrix-transpose/double-float))
(defun copy-matrix-transpose/double-float (ma si ni sj nj mb)
  (declare (type (simple-array double-float (* *)) ma mb)
           (type fixnum si ni sj nj))
  (flet ((copy (ma si nr sj j mb)
           (declare (optimize (speed 3) (safety 0))
                    (type fixnum si nr sj j)
                    (type (simple-array double-float (* *))))
           (copy-matrix-transpose-ker ma si nr sj j mb)))
  (let* ((nrb (array-dimension mb 0))
         (ncb (array-dimension mb 1))
         (nr (min ni ncb))
         (nc (min nj nrb)))
    (declare (type fixnum nrb ncb nr nc))
    (dotimes (j nc)
      (copy ma si nr sj j mb)))))
