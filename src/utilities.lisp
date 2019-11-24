(in-package :cl-user)
(defpackage cl3a.utilities
  (:use :cl :alexandria
        :cl3a.utilities_vop)
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
           :copy-matrix-to-vector-pd
           :copy-matrix-to-vector-transposed-pd))
(in-package :cl3a.utilities)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cache-line+ (the fixnum 64))
  (defconstant +L1-size+ (the fixnum 32768))
  (defconstant +L2-size+ (the fixnum 262144))
  (defconstant +L3-size+ (the fixnum 4194304))
  (defconstant +associativity+ (the fixnum 8))
  (defconstant +unroll+ (the fixnum 8)))


(defstruct (int3)
  (i 0 :type (unsigned-byte 32))
  (k 0 :type (unsigned-byte 32))
  (j 0 :type (unsigned-byte 32)))


(declaim (inline different-length-warn))
(defun different-length-warn (na nb)
  "Warn different lengths"
  (warn (format nil "Length of two vectors were different (~D and ~D). Shorter one is used." na nb)))


(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64) &rest (unsigned-byte 64))
                          (unsigned-byte 64))
                ifloor)
         (inline ifloor))
(defun ifloor (x y0 &rest ys)
  "Take integer part of floor function"
  (declare (type (unsigned-byte 64) x y0)
           (type list ys)
           (dynamic-extent ys))
  (let* ((y (reduce #'* ys :initial-value y0))
         (m (mod x y))
         (x0 (- x m)))
    (declare (type (unsigned-byte 64) y m x0))
    (/ x0 y)))


(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64) &rest (unsigned-byte 64))
                          (unsigned-byte 64))
                min-factor)
         (inline min-factor))
(defun min-factor (x y0 &rest ys)
  "calculate x0*y with the minimum m where x = x0*y + m, y = y0*y1*y2*..., and ys = (y1, y2, ...)"
  (declare (type (unsigned-byte 64) x y0)
           (type list ys)
           (dynamic-extent ys))
  (let* ((y (reduce #'* ys :initial-value y0))
         (m (mod x y)))
    (declare (type (unsigned-byte 64) y m))
    (- x m)))


(declaim (ftype (function (symbol)
                          (unsigned-byte 64))
                type-byte-length)
         (inline type-byte-length))
(defun type-byte-length (val-type)
  "return the size of type"
  (ecase val-type
    (short-float 2)
    (single-float 4)
    (double-float 8)
    (long-float 16)))


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
                           (simple-array double-float (*))))
                copy-matrix-to-vector-transposed-pd))
(defun copy-matrix-to-vector-transposed-pd (ma si ni sj nj vb)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) ma)
           (type (simple-array double-float (*)) vb)
           (type fixnum si ni sj nj))
  (let ((nr (min ni
                 (array-dimension ma 0)))
        (nc (min nj
                 (array-dimension ma 1))))
    (declare (type fixnum nr nc))
    (dotimes (j nc)
      (declare (type fixnum j))
      (let ((rma-init (array-row-major-index ma si (+ sj j)))
            (rmb-init (* j nr)))
        (declare (type fixnum rma-init rmb-init))
        (do ((i 0 (1+ i))
             (rma rma-init (+ rma (array-dimension ma 1)))
             (rmb rmb-init (1+ rmb)))
            ((>= i nr))
          (declare (type fixnum i rma rmb))
          (setf (aref vb rmb)
                (row-major-aref ma rma)))))))
