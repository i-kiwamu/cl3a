(in-package :cl-user)
(defpackage cl3a.ddotprod
  (:use :cl :trivial-types)
  (:import-from :cl3a.utilities
                :min-factor
                :dvvs-calc-within-L1
                :different-length-warn)
  (:export :dv*v))
(in-package :cl3a.ddotprod)


(declaim (inline dv*v-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                dv*v-ker))
(defun dv*v-ker (p n nv va vb)
  "Dot production between vectors va and vb"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nv)
           (type (simple-array double-float (*)) va vb))
  (let* ((nvec (min nv (the fixnum (+ p n))))
         (n5 (min-factor nvec 5))
         (res 0d0))
    (declare (type fixnum nvec n5)
             (type double-float res))
    (cond
      ((or (>= p nv) (< p 0) (< nvec 0))
       (warn (format nil "Position ~D is out of range of vectors with length of ~D." p nvec))
       0d0)
      ((< nvec 5) (do ((i p (the fixnum (+ i 1))))
                      ((>= (the fixnum i) nvec) res)
                    (setf res (+ res
                                 (the double-float (* (aref va i)
                                                      (aref vb i)))))))
      (t (let ((maxi
                (do ((i p (+ i 5))
                     (i1 (the fixnum (+ p 1)) (the fixnum (+ i1 5)))
                     (i2 (the fixnum (+ p 2)) (the fixnum (+ i2 5)))
                     (i3 (the fixnum (+ p 3)) (the fixnum (+ i3 5)))
                     (i4 (the fixnum (+ p 4)) (the fixnum (+ i4 5))))
                    ((>= i n5) i)
                  (setf res (+ res
                               (* (aref va i)  (aref vb i))
                               (* (aref va i1) (aref vb i1))
                               (* (aref va i2) (aref vb i2))
                               (* (aref va i3) (aref vb i3))
                               (* (aref va i4) (aref vb i4)))))))
           (declare (type fixnum maxi))
           (when (< maxi nvec)
             (do ((i maxi (+ i 1)))
                 ((>= i nvec))
               (setf res (+ res
                            (the double-float (* (aref va i)
                                                 (aref vb i))))))))
         res))))


(declaim (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                dv*v))
(defun dv*v (va vb)
  "Dot product with two vectors va and vb"
  (declare (optimize (speed 3))
           (type (simple-array double-float (*)) va vb))
  (let* ((na (length va))
         (nb (length vb))
         (nv (if (/= na nb)
                 (progn (different-length-warn na nb)
                        (min na nb))
                 na))
         (res (dvvs-calc-within-L1 #'dv*v-ker nv va vb)))
    (declare (type fixnum na nb nv)
             (type (proper-list double-float) res))
    (apply #'+ res)))
