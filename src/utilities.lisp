(in-package :cl-user)
(defpackage cl3a.utilities
  (:use :cl :alexandria :cl3a.utilities_vop)
  (:export :int3 :make-int3 :int3-i :int3-k :int3-j
           :int3-p :setf-int3-i :setf-int3-k :setf-int3-j
           :+cache-line+
           :+L1-size+
           :+L2-size+
           :+L3-size+
           :+associativity+
           :+unroll+
           :different-length-warn
           :ifloor
           :min-factor
           :type-byte-length
           :copy-matrix-sd :copy-matrix-pd
           :copy-matrix-to-vector-pd
           :copy-matrix-transpose-sd))
(in-package :cl3a.utilities)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cache-line+ (the fixnum 64))
  (defconstant +L1-size+ (the fixnum 32768))
  (defconstant +L2-size+ (the fixnum 262144))
  (defconstant +L3-size+ (the fixnum 4194304))
  (defconstant +associativity+ (the fixnum 8))
  (defconstant +unroll+ (the fixnum 8)))


(defstruct (int3)
  (i 0 :type fixnum)
  (k 0 :type fixnum)
  (j 0 :type fixnum))


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


(declaim (ftype (function (symbol) fixnum) type-byte-length))
(defun type-byte-length (val-type)
  "return the size of type"
  (ecase val-type
    (short-float 2)
    (single-float 4)
    (double-float 8)
    (long-float 16)))


(declaim (ftype (function ((simple-array double-float (* *))
                           fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))))
                copy-matrix-sd))
(defun copy-matrix-sd (ma si ni sj nj mb)
  (declare (type (simple-array double-float (* *)) ma mb)
           (type fixnum si ni sj nj))
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
        (dotimes (j nc)
          (setf (row-major-aref mb rmb)
                (row-major-aref ma rma))
          (incf rma)
          (incf rmb))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))))
                copy-matrix-pd))
(defun copy-matrix-pd (ma si ni sj nj mb)
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array double-float (* *)) ma mb)
           (type fixnum si ni sj nj))
  (let* ((nr (min ni
                  (array-dimension ma 0)
                  (array-dimension mb 0)))
         (nc (min nj
                  (array-dimension ma 1)
                  (array-dimension mb 1)))
         (nc0 (min-factor nc 4)))
    (declare (type fixnum nr nc nc0))
    (dotimes (i nr)
      (declare (type fixnum i))
      (let ((rma (array-row-major-index ma (+ i si) sj))
            (rmb (array-row-major-index mb i 0)))
        (declare (type fixnum rma rmb))
        (do ((j 0 (+ j 4)))
            ((>= j nc0))
          (declare (type fixnum j))
          (sb-sys:%primitive
           aset4-pd (sb-kernel:%array-data-vector mb) rmb
                    (aref4-pd (sb-kernel:%array-data-vector ma) rma))
          (incf rma 4)
          (incf rmb 4))
        (do ((j nc0 (1+ j)))
            ((>= j nc))
          (declare (type fixnum j))
          (setf (row-major-aref mb rmb)
                (row-major-aref ma rma))
          (incf rma)
          (incf rmb))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           fixnum fixnum fixnum fixnum
                           (simple-array double-float (*))))
                copy-matrix-to-vector-pd))
(defun copy-matrix-to-vector-pd (ma si ni sj nj vb)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) ma)
           (type (simple-array double-float (*)) vb)
           (type fixnum si ni sj nj))
  (let* ((nr (min ni
                  (array-dimension ma 0)))
         (nc (min nj
                  (array-dimension ma 1)))
         (nc0 (min-factor nc 8)))
    (declare (type fixnum nr nc nc0))
    (dotimes (i nr)
      (declare (type fixnum i))
      (let ((rma-init (array-row-major-index ma (+ i si) sj))
            (rmb-init (* i nc)))
        (declare (type fixnum rma-init rmb-init))
        (copy-vector8-pd nc0
                         (sb-ext:array-storage-vector ma) rma-init
                         vb rmb-init)
        (do ((j nc0 (1+ j))
             (rma (+ rma-init nc0) (1+ rma))
             (rmb (+ rmb-init nc0) (1+ rmb)))
            ((>= j nc))
          (declare (type fixnum j rma rmb))
          (setf (aref vb rmb)
                (row-major-aref ma rma)))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))))
                copy-matrix-transpose-sd))
(defun copy-matrix-transpose-sd (ma si ni sj nj mb)
  (declare (type (simple-array double-float (* *)) ma mb)
           (type fixnum si ni sj nj))
  (let ((nr (min ni
                 (array-dimension ma 0)
                 (array-dimension mb 1)))
        (nc (min nj
                 (array-dimension ma 1)
                 (array-dimension mb 0))))
    (declare (type fixnum nr nc))
    (dotimes (j nc)
      (dotimes (i nr)
        (setf (aref mb j i)
              (aref ma (+ i si) (+ j sj)))))))

