(in-package :cl-user)
(defpackage cl3a.dotprod
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dv*v :lv*v))
(in-package :cl3a.dotprod)


(defmacro v*v-ker (val-type s n nc va vb)
  "Dot production between vectors va and vb"
  (with-gensyms (nv iend iend0 res i maxi i1 i2 i3 i4)
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
               (do (;; (,i ,s (1+ ,i))
                    (,i ,s (+ ,i 5))
                    (,i1 (the fixnum (+ ,s 1)) (the fixnum (+ ,i1 5)))
                    (,i2 (the fixnum (+ ,s 2)) (the fixnum (+ ,i2 5)))
                    (,i3 (the fixnum (+ ,s 3)) (the fixnum (+ ,i3 5)))
                    (,i4 (the fixnum (+ ,s 4)) (the fixnum (+ ,i4 5))))
                   ((>= ,i ,iend0) ,i)
                 (incf ,res
                       (+ (* (aref ,va ,i) (aref ,vb ,i))
                          (* (aref ,va ,i1) (aref ,vb ,i1))
                          (* (aref ,va ,i2) (aref ,vb ,i2))
                          (* (aref ,va ,i3) (aref ,vb ,i3))
                          (* (aref ,va ,i4) (aref ,vb ,i4)))))))
       ;; if nv < 5 or maxi < iend, calculate the rest of elements
       (do ((,i ,maxi (1+ ,i)))
           ((>= ,i ,iend))
         (incf ,res (* (aref ,va ,i) (aref ,vb ,i))))
       ,res)))


(defmacro v*v (val-type va vb)
  (with-gensyms (calc na nb nc)
    `(flet ((,calc (s n nc va vb)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type fixnum s n nc)
                       (type (simple-array ,val-type (*)) va vb))
              (v*v-ker ,val-type s n nc va vb)))
       (declare (inline ,calc))
       (let* ((,na (length ,va))
              (,nb (length ,vb))
              (,nc (cond ((/= ,na ,nb) (different-length-warn ,na ,nb)
                                       (min ,na ,nb))
                         (t ,na))))
         (declare (type fixnum ,na ,nb ,nc))
         (,calc 0 ,nc ,nc ,va ,vb)))))

(declaim (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                dv*v))
(defun dv*v (va vb)
  "Dot product with two double-float vectors va and vb"
  (declare (type (simple-array double-float (*)) va vb))
  (v*v double-float va vb))


(declaim (ftype (function ((simple-array long-float (*))
                           (simple-array long-float (*)))
                          long-float)
                lv*v))
(defun lv*v (va vb)
  "Dot product with two double-float vectors va and vb"
  (declare (type (simple-array long-float (*)) va vb))
  (v*v long-float va vb))
