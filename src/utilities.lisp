(in-package :cl-user)
(defpackage cl3a.utilities
  (:use :cl)
  (:export :*L1-size*
           :ifloor
           :dvv-calc-within-L1))
(in-package :cl3a.utilities)


(defparameter *L1-size* (the fixnum 32768))


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


;; (declaim (inline dvv-calc-within-L1)
;;          (ftype (function (fixnum
;;                            double-float (simple-array double-float (*))
;;                            double-float (simple-array double-float (*)))
;;                           list) dvv-calc-within-L1))
(defun dvv-calc-within-L1 (fun n a xa b xb)
  "Divide data into small parts (< L1 cache size), and calculate.
   fun should have arguments of 
   position (fixnum), length for calc (fixnum), vector length (fixnum),
   factor a (double-float), vector xa (simple-array double-float (*)),
   factor b (double-float), vector xb (simple-array double-float (*))"
  (declare ;(optimize (speed 3) (debug 0) (safety 0) (compilation-speed 3))
           (type double-float a b)
           (type fixnum n)
           (type (simple-array double-float (*)) xa xb))
  (loop
     :with m :of-type fixnum = (ifloor *L1-size* 8)
     :with k :of-type fixnum = (* (ifloor n m) m)
     :for i :below k :by m
     :with mi :of-type fixnum = (the fixnum (+ i m))
     :collect (funcall fun i mi n a xa b xb) :into res
     :finally (return
                (let ((ni (the fixnum (- n i))))
                  (append res (list (funcall fun i ni n a xa b xb)))))))


(declaim (inline different-length-warn))
(defun different-length-warn (na nb)
  "Warn different lengths"
  (declare ;(optimize (speed 3) (debug 0) (safety 0) (compilation-speed 3))
           (type fixnum na nb))
  (warn (format nil "Length of two vectors were different (~D and ~D). Shorter one is used." na nb)))

(define-condition different-length-error (error)
  ((text :initarg :text :reader text)))

