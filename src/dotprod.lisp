(in-package :cl-user)
(defpackage cl3a.dotprod
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:import-from :cl3a.utilities
                :+L1-size+
                :different-length-warn
                :ifloor
                :min-factor
                :type-byte-length
                :dotimes-interval)
  (:export :dv*v :lv*v))
(in-package :cl3a.dotprod)


(defmacro v*v-ker (val-type s n nc va vb)
  "Dot production between vectors va and vb"
  (with-gensyms (nv iend iend0 res i maxi)
    `(let* ((,nv (min ,nc ,n))
            (,iend (min ,nc (the fixnum (+ ,s ,n))))
            (,iend0 (min-factor ,iend 5))
            (,res (coerce 0.0 ',val-type))
            (,maxi 0))
       (declare (type fixnum ,nv ,iend0 ,maxi)
                (type ,val-type ,res))
       ;; Do NOT use dotimes-unroll macro for speed
       (when (>= ,nv 5)
         (setf ,maxi
               (do ((,i ,s (1+ ,i)))
                   ((>= ,i ,iend0) ,i)
                 (incf ,res
                       (+ (* (aref ,va ,i) (aref ,vb ,i))
                          ,@(loop :repeat 4
                               :append `((* (aref ,va (incf ,i))
                                            (aref ,vb ,i)))))))))
       ;; if nv < 5 or maxi < iend, calculate the rest of elements
       (do ((,i ,maxi (1+ ,i)))
           ((>= ,i ,iend))
         (incf ,res (* (aref ,va ,i) (aref ,vb ,i))))
       ,res)))


(declaim (inline dv*v-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                dv*v-ker))
(defun dv*v-ker (s n nc va vb)
  "Dot product with two double-float vectors va and vb"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum s n nc)
           (type (simple-array double-float (*)) va vb))
  (v*v-ker double-float s n nc va vb))
(declaim (notinline dv*v-ker))


(declaim (inline lv*v-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array long-float (*))
                           (simple-array long-float (*)))
                          long-float)
                lv*v-ker))
(defun lv*v-ker (s n nc va vb)
  "Dot product with two long-float vectors va and vb"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum s n nc)
           (type (simple-array long-float (*)) va vb))
  (v*v-ker long-float s n nc va vb))
(declaim (notinline lv*v-ker))


(declaim (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                dv*v))
(defun dv*v (va vb)
  "Dot product with two double-float vectors va and vb"
  (declare (optimize (speed 3))
           (inline dv*v-ker)
           (type (simple-array double-float (*)) va vb))
  (let* ((na (length va))
         (nb (length vb))
         (nc (cond ((/= na nb) (different-length-warn na nb)
                               (min na nb))
                   (t na))))
    (declare (type fixnum na nb nc))
    (dv*v-ker 0 nc nc va vb)))


(declaim (ftype (function ((simple-array long-float (*))
                           (simple-array long-float (*)))
                          long-float)
                lv*v))
(defun lv*v (va vb)
  "Dot product with two double-float vectors va and vb"
  (declare (optimize (speed 3))
           (inline lv*v-ker)
           (type (simple-array long-float (*)) va vb))
  (let* ((na (length va))
         (nb (length vb))
         (nc (cond ((/= na nb) (different-length-warn na nb)
                               (min na nb))
                   (t na))))
    (declare (type fixnum na nb nc))
    (lv*v-ker 0 nc nc va vb)))
