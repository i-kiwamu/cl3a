(in-package :cl-user)
(defpackage cl3a.utilities
  (:use :cl)
  (:export :ifloor
           :dvv-calc-within-L1
           :different-length-warn))
(in-package :cl3a.utilities)


(defparameter *L1-size* (the fixnum 32768))


;; (declaim (inline ifloor)
;;          (ftype (function (fixnum fixnum) fixnum) ifloor))
(defun ifloor (x y)
  "Take integer part of floor function"
  ;; (declare (optimize (speed 3) (debug 0) (safety 2) (compilation-speed 3))
  ;;          (type fixnum x y))
  (multiple-value-bind (q) (floor (/ x y))
    (declare (fixnum q))
    q))


(declaim (inline dvv-calc-within-L1)
         (ftype (function ((function (fixnum fixnum fixnum
                                      (simple-array double-float (*))
                                      (simple-array double-float (*)))
                                     double-float)
                           fixnum
                           (simple-array double-float (*))
                           (simple-array double-float (*)))
                          list) dvv-calc-within-L1))
(defun dvv-calc-within-L1 (fun n xa xb)
  "Divide data into small parts (< L1 cache size), and calculate.
   fun should have arguments of 
   position (fixnum), length for calc (fixnum), vector length (fixnum),
   vector xa (simple-array vtype (*)),
   vector xb (simple-array vtype (*))"
  (declare (optimize (speed 3) (compilation-speed 3))
           (type (function (fixnum fixnum fixnum
                            (simple-array double-float (*))
                            (simple-array double-float (*)))
                           double-float) fun)
           (type fixnum n)
           (type (simple-array double-float (*)) xa xb))
  (loop
     :with m :of-type fixnum = (ifloor *L1-size* 8)
     :with k :of-type fixnum = (* (ifloor n m) m)
     :for i :of-type fixnum :below k :by m
     :with mi :of-type fixnum = (the fixnum (1- (+ i m)))
     :collect (funcall fun i mi n xa xb) :into res
     :finally (return
                (let ((ni (the fixnum (- n i))))
                  (declare (fixnum ni))
                  (append res (list (funcall fun i ni n xa xb)))))))


(declaim (inline different-length-warn))
(defun different-length-warn (na nb)
  "Warn different lengths"
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 3))
           (type fixnum na nb))
  (warn (format nil "Length of two vectors were different (~D and ~D). Shorter one is used." na nb)))

(define-condition different-length-error (error)
  ((text :initarg :text :reader text)))

