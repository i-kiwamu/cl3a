(in-package :cl-user)
(defpackage cl3a.dotprod
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dv*v :lv*v))
(in-package :cl3a.dotprod)


(defmacro v*v-ker (val-type s n nc va vb)
  "Dot production between vectors va and vb"
  (let ((i-list (make-gensym-list +unroll+ "i")))
    (with-gensyms (iend iend0 res maxi i)
      `(let* ((,iend (min ,nc (the fixnum (+ ,s ,n))))
              (,iend0 (min-factor ,iend +unroll+))
              (,res (coerce 0.0 ',val-type))
              (,maxi 0))
         (declare (type fixnum ,iend0 ,maxi)
                  (type ,val-type ,res))
         ;; Do NOT use dotimes-unroll macro for speed
         (setf ,maxi
               (do (,@(loop :for ui :below +unroll+
                         :for ii :in i-list
                         :append `((,ii (the fixnum (+ ,s ,ui))
                                        (the fixnum (+ ,ii +unroll+))))))
                   ((>= ,(nth 0 i-list) ,iend0) ,(nth 0 i-list))
                 (incf ,res
                       (+ ,@(loop :for ii :in i-list
                               :append `((* (aref ,va ,ii) (aref ,vb ,ii))))))))
         ;; if nv < unroll or maxi < iend, calculate the rest of elements
         (do ((,i ,maxi (1+ ,i)))
             ((>= ,i ,iend))
           (incf ,res (* (aref ,va ,i) (aref ,vb ,i))))
         ,res))))


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
  (declare (optimize (speed 3))
           (type (simple-array double-float (*)) va vb))
  (v*v double-float va vb))


(declaim (ftype (function ((simple-array long-float (*))
                           (simple-array long-float (*)))
                          long-float)
                lv*v))
(defun lv*v (va vb)
  "Dot product with two double-float vectors va and vb"
  (declare (optimize (speed 3))
           (type (simple-array long-float (*)) va vb))
  (v*v long-float va vb))
