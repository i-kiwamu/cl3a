(in-package :cl-user)
(defpackage cl3a.ddotprod
  (:use :cl)
  (:import-from :cl3a.utilities
                :ifloor
                :dvv-calc-within-L1
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
  "Dot production between vectors a*va and b*vb"
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 3))
           (type fixnum p n nv)
           (type (simple-array double-float (*)) va vb))
  (let ((nvec (min nv n)))
    (cond
      ((or (> p nvec) (< p 0) (<= nvec 0))
       (warn (format nil "Position ~D is out of range of vectors." p)) 0d0)
      ((< nvec 5) (loop :for i :from p :below nvec
                        :sum (* (aref va i) (aref vb i))))
      (t (loop
            :with n5 :of-type fixnum = (* (ifloor nvec 5) 5)
            :for i :below n5 :by 5
            :with i1 :of-type fixnum = (+ i 1)
            :and i2 :of-type fixnum = (+ i 2)
            :and i3 :of-type fixnum = (+ i 3)
            :and i4 :of-type fixnum = (+ i 4)
            :with s0 :of-type double-float = (* (aref va i) (aref vb i))
            :and s1 :of-type double-float = (* (aref va i1) (aref vb i1))
            :and s2 :of-type double-float = (* (aref va i2) (aref vb i2))
            :and s3 :of-type double-float = (* (aref va i3) (aref vb i3))
            :and s4 :of-type double-float = (* (aref va i4) (aref vb i4))
            :sum (+ s0 s1 s2 s3 s4) :into res
            :finally
            (return (+ res
                       (loop :for ir :from n5 :below nvec
                          :sum (* (aref va ir) (aref vb ir))))))))))


(declaim (inline dv*v)
         (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                dv*v))
(defun dv*v (va vb)
  "Dot product with two vectors va and vb"
  (declare (optimize (speed 3) (debug 0) (safety 2) (compilation-speed 3))
           (type (simple-array double-float (*)) va vb))
  (let* ((na (the fixnum (length va)))
         (nb (the fixnum (length vb)))
         (nv (the fixnum (if (/= na nb)
                             (progn (different-length-warn na nb)
                                    (min na nb))
                             na))))
    (apply #'+ (dvv-calc-within-L1 #'dv*v-ker nv va vb))))
