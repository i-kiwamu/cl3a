(in-package :cl-user)
(defpackage cl3a.add-vector
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:import-from :cl3a.utilities
                :+L2-size+
                :different-length-warn
                :ifloor
                :min-factor
                :type-byte-length
                :dotimes-unroll
                :dotimes-interval)
  (:export :dv+v :lv+v :dv+v-block))
(in-package :cl3a.add-vector)


(defmacro v+v-ker (s n nc va vb a b vc)
  "Add two vectors between a*va and b*vb"
  (with-gensyms (iend i)
    `(let ((,iend (min ,nc (the fixnum (+ ,s ,n)))))
       (declare (type fixnum ,iend))
       (dotimes-unroll (,i ,s ,iend)
         (incf (aref ,vc ,i)
               (+ (* ,a (aref ,va ,i)) (* ,b (aref ,vb ,i))))))))


(declaim (inline dv+v-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array double-float (*))
                           (simple-array double-float (*))
                           double-float double-float
                           (simple-array double-float (*))))
                dv+v-ker))
(defun dv+v-ker (s n nc va vb a b vc)
  "Add two double-float vectors between a*va and b*vb"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum s n nc)
           (type double-float a b)
           (type (simple-array double-float (*)) va vb vc))
  (v+v-ker s n nc va vb a b vc))
(declaim (notinline dv+v-ker))


(declaim (inline lv+v-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array long-float (*))
                           (simple-array long-float (*))
                           long-float long-float
                           (simple-array long-float (*))))
                lv+v-ker))
(defun lv+v-ker (s n nc va vb a b vc)
  "Add two long-float vectors between va and vb"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum s n nc)
           (type long-float a b)
           (type (simple-array long-float (*)) va vb vc))
  (v+v-ker s n nc va vb a b vc))
(declaim (notinline lv+v-ker))


(declaim (ftype (function (double-float (simple-array double-float (*))
                           double-float (simple-array double-float (*))
                           (simple-array double-float (*))))
                dv+v))
(defun dv+v (a va b vb vc)
  "Add two double-float vectors va and vb"
  (declare (optimize (speed 3))
           (inline dv+v-ker)
           (type double-float a b)
           (type (simple-array double-float (*)) va vb vc))
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
             (dv+v-ker i 1 nc va vb a b vc)))
          (t
           (dotimes-interval (i m nc)
             (dv+v-ker i m nc va vb a b vc))))))


(declaim (ftype (function (double-float (simple-array double-float (*))
                           double-float (simple-array double-float (*))
                           (simple-array double-float (*)) fixnum))
                dv+v-block))
(defun dv+v-block (a va b vb vc m)
  "Add two double-float vectors va and vb"
  (declare (optimize (speed 3))
           (inline dv+v-ker)
           (type double-float a b)
           (type (simple-array double-float (*)) va vb vc)
           (type fixnum m))
  (let* ((na (length va))
         (nb (length vb))
         (nc (cond ((/= na nb) (different-length-warn na nb)
                               (min na nb))
                   (t na))))
    (declare (type fixnum na nb nc m))
    (cond ((= m 0)
           (dotimes (i nc)
             (dv+v-ker i 1 nc va vb a b vc)))
          (t
           (dotimes-interval (i m nc)
             (dv+v-ker i m nc va vb a b vc))))))


(declaim (ftype (function (long-float (simple-array long-float (*))
                           long-float (simple-array long-float (*))
                           (simple-array long-float (*))))
                lv+v))
(defun lv+v (a va b vb vc)
  "Add two long-float vectors va and vb"
  (declare (optimize (speed 3))
           (inline lv+v-ker)
           (type long-float a b)
           (type (simple-array long-float (*)) va vb vc))
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
             (lv+v-ker i 1 nc va vb a b vc)))
          (t
           (dotimes-interval (i m nc)
             (lv+v-ker i m nc va vb a b vc))))))
