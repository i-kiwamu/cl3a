(in-package :cl-user)
(defpackage naive-funcs
  (:use :cl :cl3a)
  (:nicknames naive)
  (:export :dv*v-naive
           :sv*v-naive
           :dnorm-naive
           :dv+v-naive
           :drotate-naive
           :dm*v-naive
           :dm*m-naive))
(in-package #:naive-funcs)


(declaim (ftype (function ((vec double-float)
                           (vec double-float))
                          double-float)
                dv*v-naive))
(defun dv*v-naive (va vb)
  (declare (type (vec double-float) va vb))
  (let ((nv (min (vec-length va) (vec-length vb)))
        (res 0d0))
    (declare (type fixnum nv))
    (dotimes (i nv)
      (incf res (* (vecref va i) (vecref vb i))))
    res))


(declaim (ftype (function ((simple-array single-float (*))
                           (simple-array single-float (*)))
                          single-float)
                sv*v-naive))
(defun sv*v-naive (va vb)
  (declare (type (simple-array single-float (*)) va vb))
  (let ((nv (min (length va) (length vb)))
        (res 0.0))
    (declare (type fixnum nv))
    (dotimes (i nv)
      (incf res (* (aref va i) (aref vb i))))
    res))


(declaim (ftype (function ((vec double-float))
                          double-float)
                dnorm-naive))
(defun dnorm-naive (va)
  (declare (type (vec double-float) va))
  (sqrt (dv*v-naive va va)))


(declaim (ftype (function (double-float (simple-array double-float (*))
                           double-float (simple-array double-float (*))
                           (simple-array double-float (*))))
                dv+v-naive))
(defun dv+v-naive (a va b vb vc)
  (declare (type (simple-array double-float (*)) va vb vc)
           (type double-float a b))
  (let* ((nv (min (length va) (length vb))))
    (dotimes (i nv)
      (setf (aref vc i) (+ (* a (aref va i))
                           (* b (aref vb i)))))))


(declaim (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*))
                           (double-float -1d0 1d0)
                           (double-float -1d0 1d0)
                           (simple-array double-float (*))
                           (simple-array double-float (*))))
                drotate-naive))
(defun drotate-naive (va vb c s vc vd)
  (declare (type (simple-array double-float (*)) va vb vc vd)
           (type (double-float -1d0 1d0) c s))
  (let* ((nv (min (length va) (length vb))))
    (declare (type fixnum nv))
    (dotimes (i nv)
      (let ((vai (aref va i))
            (vbi (aref vb i)))
        (declare (type double-float vai vbi))
        (setf (aref vc i) (+ (* c vai) (* s vbi)))
        (setf (aref vd i) (- (* c vbi) (* s vai)))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (*))
                           (simple-array double-float (*))))
                dm*v-naive))
(defun dm*v-naive (ma vb res)
  (declare (type (simple-array double-float (* *)) ma)
           (type (simple-array double-float (*)) vb res))
  (let* ((nr (array-dimension ma 0))
         (nc (array-dimension ma 1)))
    (dotimes (i nr)
      (let ((resi (aref res i)))
        (declare (type double-float resi))
        (dotimes (j nc)
          (incf resi (* (aref ma i j) (aref vb j))))
        (setf (aref res i) resi)))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m-naive))
(defun dm*m-naive (ma mb mc)
  (declare (type (simple-array double-float (* *)) ma mb mc))
  (let ((nra (array-dimension ma 0))
        (nv (min (array-dimension ma 1) (array-dimension mb 0)))
        (ncb (array-dimension mb 1)))
    (declare (type fixnum nra nv ncb))
    (dotimes (i nra)
      (dotimes (k nv)
        (let ((maik (aref ma i k)))
          (declare (type double-float maik))
          (dotimes (j ncb)
            (incf (aref mc i j) (* maik (aref mb k j)))))))))
