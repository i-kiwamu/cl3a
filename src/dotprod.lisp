(in-package :cl-user)
(defpackage dotprod
  (:use :common-lisp)
  (:import-from :trivial-benchmark #:with-timing)
  (:export #:perf))
(in-package :dotprod)


(defparameter *l1-size* (the fixnum 32768))


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
            :for i :below nmin :by 5
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
            :sum (+ s0 s1 s2 s3 s4))))))


;; (declaim (inline get-type-size))
;; (defun get-type-size (x)
;;   "return size of type (bytes)"
;;   (declare (optimize (speed 3) (debug 0) (safety 2) (compilation-speed 3)))
;;   (cond ((or (eql x 'fixnum) (eql x 'integer)) 4)
;;         ((eql x 'bignum) 8)
;;         ((eql x 'short-float) 2)
;;         ((or (eql x 'float) (eql x 'single-float)) 4)
;;         ((eql x 'double-float) 8)
;;         ((eql x 'long-float) 16)
;;         (t 8)))


(declaim (inline ifloor)
         (ftype (function (fixnum fixnum) fixnum) ifloor))
(defun ifloor (x y)
  "Take integer part of floor function"
  (declare (optimize (speed 3) (debug 0) (safety 2) (compilation-speed 3))
           (type fixnum x y))
  (multiple-value-bind (q) (floor (/ x y)) q))


(declaim (inline dvv-calc-within-L1)
         (ftype (function (double-float (simple-array double-float (*))
                           double-float (simple-array double-float (*)))
                          list) dvv-calc-within-L1))
(defun dvv-calc-within-L1 (fun a xa b xb)
  "Divide data into small parts (< L1 cache size), and calculate"
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 3))
           (type double-float a b)
           (type (simple-array double-float (*)) xa xb))
  (loop
     :with n :of-type fixnum = (min (length xa) (length xb))
     :with m :of-type fixnum = (ifloor *l1-size* 8)
     :with k :of-type fixnum = (* (ifloor n m) m)
     :for i :below k :by m
     :with mi :of-type fixnum = (the fixnum (+ i m))
     :collect (funcall fun i mi a xa b xb) :into res
     :finally (return
                (let ((ni (the fixnum (- n i))))
                  (append res (list (funcall fun i ni a xa b xb)))))))


(declaim ; (inline ddotprod)
         (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                ddotprod))
(defun ddotprod (va vb)
  "Dot product with two vectors va and vb"
  (declare (optimize (speed 3) (debug 0) (safety 2) (compilation-speed 3))
           (type (simple-array double-float (*)) va vb))
  (apply #'+
         (dvv-calc-within-L1 #'ddotprod-ker 1d0 va 1d0 vb)))



(defun perf (n m)
  "Performance measurement by m repeat with random vectors whose length is n"
  (declare (optimize (speed 3) (compilation-speed 3))
           (type fixnum n m))
  (let ((va (make-array (list n)
                        :element-type 'double-float))
        (vb (make-array (list n)
                        :element-type 'double-float)))
    (dotimes (i n)
      (setf (aref va i) (random 1d0))
      (setf (aref vb i) (random 1d0)))
    (time (dotimes (i m)
            (ddotprod va vb)))))
