(in-package :cl-user)
(defpackage cl3a.add-vector
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dv+v :lv+v))
(in-package :cl3a.add-vector)


(defmacro v+v-ker (s n nc va vb a b vc)
  "Add two vectors between a*va and b*vb"
  (with-gensyms (iend i)
    `(let ((,iend (min ,nc (the fixnum (+ ,s ,n)))))
       (declare (type fixnum ,iend))
       (dotimes-unroll (,i ,s ,iend)
         (incf (aref ,vc ,i)
               (+ (* ,a (aref ,va ,i)) (* ,b (aref ,vb ,i))))))))


(defmacro v+v (val-type a va b vb vc)
  (with-gensyms (calc na nb nc tbl m i)
    `(flet ((,calc (s n nc va vb a b vc)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type fixnum s n nc)
                       (type ,val-type a b)
                       (type (simple-array ,val-type (*)) va vb vc))
              (v+v-ker s n nc va vb a b vc)))
       (declare (inline ,calc))
       (let* ((,na (length ,va))
              (,nb (length ,vb))
              (,nc (cond ((/= ,na ,nb) (different-length-warn ,na ,nb)
                                       (min ,na ,nb))
                         (t ,na)))
              (,tbl (type-byte-length ',val-type))
              (,m (ifloor +L2-size+ ,tbl)))
         (declare (type fixnum ,na ,nb ,nc ,tbl ,m))
         (cond ((= ,m 0)
                (dotimes (,i ,nc)
                  (,calc ,i 1 ,nc ,va ,vb ,a ,b ,vc)))
               (t
                (dotimes-interval (,i ,m ,nc)
                  (,calc ,i ,m ,nc ,va ,vb ,a ,b ,vc))))))))


(declaim (ftype (function (double-float (simple-array double-float (*))
                           double-float (simple-array double-float (*))
                           (simple-array double-float (*))))
                dv+v))
(defun dv+v (a va b vb vc)
  "Add two double-float vectors va and vb"
  (declare (type double-float a b)
           (type (simple-array double-float (*)) va vb vc))
  (v+v double-float a va b vb vc))


(declaim (ftype (function (long-float (simple-array long-float (*))
                           long-float (simple-array long-float (*))
                           (simple-array long-float (*))))
                lv+v))
(defun lv+v (a va b vb vc)
  "Add two long-float vectors va and vb"
  (declare (type long-float a b)
           (type (simple-array long-float (*)) va vb vc))
  (v+v long-float a va b vb vc))
