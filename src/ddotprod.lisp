(in-package :cl-user)
(defpackage cl3a.ddotprod
  (:use :cl)
  (:import-from :cl3a.utilities
                :ifloor
                :dvv-calc-within-L1)
  (:export :ddotprod))
(in-package :cl3a.ddotprod)


(declaim (inline ddotprod-ker)
         (ftype (function (fixnum fixnum
                           (simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                ddotprod-ker))
(defun ddotprod-ker (p n a va b vb)
  "Dot production between vectors a*va and b*vb"
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 3))
           (type fixnum p n)
           (type double-float a b)
           (type (simple-array double-float (*)) va vb))
  (let ((nmin (the fixnum (min (length va) (length vb) n))))
    (cond
      ((or (> p nmin) (< p 0) (<= nmin 0)) 0d0)
      ((< nmin 5) (loop :for i :from p :below nmin
                        :sum (* a (aref va i) b (aref vb i))))
      (t (loop
            :with n5 :of-type fixnum = (* (ifloor nmin 5) 5)
            :for i :below n5 :by 5
            :with i1 :of-type fixnum = (+ i 1)
            :and i2 :of-type fixnum = (+ i 2)
            :and i3 :of-type fixnum = (+ i 3)
            :and i4 :of-type fixnum = (+ i 4)
            :with s0 :of-type double-float = (* a (aref va i) b (aref vb i))
            :and s1 :of-type double-float = (* a (aref va i1)
                                               b (aref vb i1))
            :and s2 :of-type double-float = (* a (aref va i2)
                                               b (aref vb i2))
            :and s3 :of-type double-float = (* a (aref va i3)
                                               b (aref vb i3))
            :and s4 :of-type double-float = (* a (aref va i4)
                                               b (aref vb i4))
            :sum (+ s0 s1 s2 s3 s4) :into res
            :finally
            (return (+ res
                       (loop :with nrest :of-type fixnum = (- nmin n5)
                             :for ir :from n5 :below nmin
                             :sum (* a (aref va ir) b (aref vb ir))))))))))


(declaim ; (inline ddotprod)
         (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                ddotprod))
(defun ddotprod (va vb)
  "Dot product with two vectors va and vb"
  (declare (optimize (speed 3) (debug 0) (safety 2) (compilation-speed 3))
           (type (simple-array double-float (*)) va vb))
  ; (apply #'+ (dvv-calc-within-L1 #'ddotprod-ker 1d0 va 1d0 vb)))
  (ddotprod-ker 0 (min (length va) (length vb)) 1d0 va 1d0 vb))
