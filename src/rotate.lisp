(in-package :cl-user)
(defpackage cl3a.rotate
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:import-from :cl3a.utilities
                :+L2-size+
                :different-length-warn
                :type-byte-length
                :ifloor
                :min-factor
                :dotimes-unroll
                :dotimes-interval)
  (:export :drotate :lrotate))
(in-package :cl3a.rotate)


(defmacro rotate-ker (p n nvec va vb c s vc vd)
  "Rotate two vectors (va & vb) with cos and sin,
   and return new two vectors"
  (with-gensyms (iend i)
    `(let ((,iend (min ,nvec (the fixnum (+ ,p ,n)))))
       (declare (type fixnum ,iend))
       (dotimes-unroll (,i ,p ,iend)
         (setf (aref ,vc ,i) (+ (* ,c (aref ,va ,i))
                                (* ,s (aref ,vb ,i))))
         (setf (aref ,vd ,i) (- (* ,c (aref ,vb ,i))
                                (* ,s (aref ,va ,i))))))))


(declaim (inline drotate-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array double-float (*))
                           (simple-array double-float (*))
                           (double-float -1d0 1d0)
                           (double-float -1d0 1d0)
                           (simple-array double-float (*))
                           (simple-array double-float (*))))
                drotate-ker))
(defun drotate-ker (p n nvec va vb c s vc vd)
  "Rotate two double-float vectors (va & vb) with cos and sin,
   and return new two vectors"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nvec)
           (type (simple-array double-float (*)) va vb vc vd)
           (type double-float c s))
  (rotate-ker p n nvec va vb c s vc vd))
(declaim (notinline drotate-ker))


(declaim (inline lrotate-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array long-float (*))
                           (simple-array long-float (*))
                           (long-float -1l0 1l0) (long-float -1l0 1l0)
                           (simple-array long-float (*))
                           (simple-array long-float (*))))
                lrotate-ker))
(defun lrotate-ker (p n nvec va vb c s vc vd)
  "Rotate two long-float vectors (va & vb) with cos and sin,
   and return new two vectors"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nvec)
           (type (simple-array long-float (*)) va vb vc vd)
           (type long-float c s))
  (rotate-ker p n nvec va vb c s vc vd))
(declaim (notinline lrotate-ker))


(declaim (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*))
                           (double-float -1d0 1d0)
                           (double-float -1d0 1d0)
                           (simple-array double-float (*))
                           (simple-array double-float (*))))
                drotate))
(defun drotate (va vb c s vc vd)
  "Rotate two double-float vectors (va & vb) with cos and sin,
   and return new two vectors"
  (declare (optimize (speed 3))
           (inline drotate-ker)
           (type (simple-array double-float (*)) va vb vc vd)
           (type (double-float 0d0 1d0) c s))
  (let* ((na (length va))
         (nb (length vb))
         (nc (cond ((/= na nb) (different-length-warn na nb)
                               (min na nb))
                   (t na)))
         (tbl (type-byte-length 'double-float))
         (m (ifloor +L2-size+ tbl)))
    (declare (type fixnum na nb nc tbl m))
    (cond ((= m 0)
           (dotimes (i nc)
             (drotate-ker i m nc va vb c s vc vd)))
          (t
           (dotimes-interval (i m nc)
             (drotate-ker i m nc va vb c s vc vd))))))


(declaim (ftype (function ((simple-array long-float (*))
                           (simple-array long-float (*))
                           (long-float -1l0 1l0)
                           (long-float -1l0 1l0)
                           (simple-array long-float (*))
                           (simple-array long-float (*))))
                lrotate))
(defun lrotate (va vb c s vc vd)
  "Rotate two long-float vectors (va & vb) with cos and sin,
   and return new two vectors"
  (declare (optimize (speed 3))
           (inline lrotate-ker)
           (type (simple-array long-float (*)) va vb vc vd)
           (type (long-float 0l0 1l0) c s))
  (let* ((na (length va))
         (nb (length vb))
         (nc (cond ((/= na nb) (different-length-warn na nb)
                               (min na nb))
                   (t na)))
         (tbl (type-byte-length 'long-float))
         (m (ifloor +L2-size+ tbl)))
    (declare (type fixnum na nb nc tbl m))
    (cond ((= m 0)
           (dotimes (i nc)
             (lrotate-ker i m nc va vb c s vc vd)))
          (t
           (dotimes-interval (i m nc)
             (lrotate-ker i m nc va vb c s vc vd))))))
