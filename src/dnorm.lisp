(in-package :cl-user)
(defpackage cl3a.dnorm
  (:use :cl)
  (:import-from :cl3a.utilities
                :ifloor
                :dv-calc-within-L1
                :different-length-warn)
  (:export :dnorm))
(in-package :cl3a.dnorm)


(declaim (inline asquare)
         (ftype (function (double-float) (double-float 0d0))))
(defun asquare (x)
  "calculate square of x"
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (type double-float x))
  (let ((ax (abs x)))
    (declare (double-float ax))
    (* ax ax)))


(declaim (inline dnorm-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array double-float (*)))
                          (double-float 0d0))
                dnorm-ker))
(defun dnorm-ker (p n nv va)
  "Sqaure of euclidean norm of vector"
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (type fixnum p n nv)
           (type (simple-array double-float (*)) va))
  (let ((nvec (min nv n)))
    (declare (fixnum nvec))
    (cond
      ((= nvec 0) 0d0)
      ((or (>= p nv) (< p 0) (< nvec 0))
       (warn (format nil "Position ~D is out of range of vectors with length of ~D." p nvec)) 0d0)
      ((< nvec 5) (loop :for i :of-type fixnum :from p :below nvec
                        :sum (asquare (aref va i))))
      (t (loop
            :with n5 :of-type fixnum = (* (ifloor nvec 5) 5)
            :for i :of-type fixnum :below n5 :by 5
            :with i1 :of-type fixnum = (+ i 1)
            :and i2 :of-type fixnum = (+ i 2)
            :and i3 :of-type fixnum = (+ i 3)
            :and i4 :of-type fixnum = (+ i 4)
            :with s0 :of-type double-float = (asquare (aref va i))
            :and s1 :of-type double-float = (asquare (aref va i1))
            :and s2 :of-type double-float = (asquare (aref va i2))
            :and s3 :of-type double-float = (asquare (aref va i3))
            :and s4 :of-type double-float = (asquare (aref va i4))
            :sum (+ s0 s1 s2 s3 s4) :into res :of-type double-float
            :finally
            (return (+ res
                       (loop :for ir :of-type fixnum :from n5 :below nvec
                             :sum (asquare (aref va ir))))))))))


(declaim (inline dnorm)
         (ftype (function ((simple-array double-float (*)))
                          (double-float 0d0))
                dnorm))
(defun dnorm (va)
  "Euclidean norm of vector"
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (type (simple-array double-float (*)) va))
  (let* ((na (length va)))
    (declare (fixnum na))
    (sqrt (apply #'+ (dv-calc-within-L1 #'dnorm-ker na va)))))
