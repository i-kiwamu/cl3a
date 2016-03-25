(in-package :cl-user)
(defpackage cl3a.rotate
  (:use :cl :alexandria :cl3a.utilities)
  (:export :drotate :lrotate))
(in-package :cl3a.rotate)


(defmacro rotate-ker (p n nv va vb c s vc vd)
  "Rotate two vectors (va & vb) with cos and sin,
   and return new two vectors"
  (with-gensyms (iend i)
    `(let ((,iend (min ,nv (the fixnum (+ ,p ,n)))))
       (declare (type fixnum ,iend))
       (dotimes-unroll (,i ,p ,iend)
         (setf (aref ,vc ,i) (+ (* ,c (aref ,va ,i))
                                (* ,s (aref ,vb ,i))))
         (setf (aref ,vd ,i) (- (* ,c (aref ,vb ,i))
                                (* ,s (aref ,va ,i))))))))


(defmacro xrotate (val-type va vb c s vc vd)
  (with-gensyms (calc na nb nv tbl m i)
    `(flet ((,calc (p n nvec va vb c s vc vd)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type fixnum p n nvec)
                       (type (simple-array ,val-type (*)) va vb vc vd)
                       (type ,val-type c s))
              (rotate-ker p n nvec va vb c s vc vd)))
       (declare (inline ,calc))
       (let* ((,na (length ,va))
              (,nb (length ,vb))
              (,nv (cond ((/= ,na ,nb) (different-length-warn ,na ,nb)
                                       (min ,na ,nb))
                         (t ,na)))
              (,tbl (type-byte-length ',val-type))
              (,m (ifloor +L2-size+ ,tbl)))
         (declare (type fixnum ,na ,nb ,nv ,tbl ,m))
         (dotimes-interval (,i ,m ,nv)
           (,calc ,i ,m ,nv ,va ,vb ,c ,s ,vc ,vd))))))


(declaim (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*))
                           (double-float -1d0 1d0)
                           (double-float -1d0 1d0)
                           (simple-array double-float (*))
                           (simple-array double-float (*))))
                drotate))
(defun drotate (va vb c s vc vd)
  "Rotate two double-float vectors (va & vb) with cos and sin,
   and return new two vectors"
  (declare (type double-float c s)
           (type (simple-array double-float (*)) va vb vc vd))
  (xrotate double-float va vb c s vc vd))


(declaim (ftype (function ((simple-array long-float (*))
                           (simple-array long-float (*))
                           (long-float -1l0 1l0)
                           (long-float -1l0 1l0)
                           (simple-array long-float (*))
                           (simple-array long-float (*))))
                lrotate))
(defun lrotate (va vb c s vc vd)
  "Rotate two long-float vectors (va & vb) with cos and sin,
   and return new two vectors"
  (declare (type long-float c s)
           (type (simple-array long-float (*)) va vb vc vd))
  (xrotate long-float va vb c s vc vd))
