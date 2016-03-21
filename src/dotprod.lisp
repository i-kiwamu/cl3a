(in-package :cl-user)
(defpackage cl3a.dotprod
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:import-from :cl3a.utilities
                :+L1-size+
                :ifloor
                :min-factor
                :type-byte-length
                :different-length-warn)
  (:export :dv*v :lv*v))
(in-package :cl3a.dotprod)


(defmacro v*v-ker (val-type p n nvec va vb)
  "Dot production between vectors va and vb"
  (with-gensyms (nv iend iend0 res i maxi i1 i2 i3 i4)
    `(let* ((,nv (min ,nvec ,n))
            (,iend (min ,nvec (the fixnum (+ ,p ,n))))
            (,iend0 (min-factor ,iend 5))
            (,res (coerce 0.0 ',val-type)))
       (declare (type fixnum ,nv ,iend0)
                (type ,val-type ,res))
       ;; Do NOT use dotimes-unroll macro for speed
       (cond
         ((< ,nv 5) (do ((,i ,p (1+ ,i)))
                        ((>= ,i ,iend))
                      (incf ,res (* (aref ,va ,i) (aref ,vb ,i)))))
         (t (let ((,maxi
                   (do ((,i ,p (+ ,i 5))
                        (,i1 (the fixnum (+ ,p 1)) (the fixnum (+ ,i1 5)))
                        (,i2 (the fixnum (+ ,p 2)) (the fixnum (+ ,i2 5)))
                        (,i3 (the fixnum (+ ,p 3)) (the fixnum (+ ,i3 5)))
                        (,i4 (the fixnum (+ ,p 4)) (the fixnum (+ ,i4 5))))
                       ((>= ,i ,iend0) ,i)
                     (incf ,res
                           (+ (* (aref ,va ,i)  (aref ,vb ,i))
                              (* (aref ,va ,i1) (aref ,vb ,i1))
                              (* (aref ,va ,i2) (aref ,vb ,i2))
                              (* (aref ,va ,i3) (aref ,vb ,i3))
                              (* (aref ,va ,i4) (aref ,vb ,i4)))))))
              (declare (type fixnum ,maxi))
              (when (< ,maxi ,iend)
                (do ((,i ,maxi (1+ ,i)))
                    ((>= ,i ,iend))
                  (incf ,res (* (aref ,va ,i) (aref ,vb ,i))))))))
       ,res)))


(declaim (inline dv*v-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                dv*v-ker))
(defun dv*v-ker (p n nvec va vb)
  "Dot product with two double-float vectors va and vb"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nvec)
           (type (simple-array double-float (*)) va vb))
  (v*v-ker double-float p n nvec va vb))


(declaim (inline lv*v-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array long-float (*))
                           (simple-array long-float (*)))
                          long-float)
                lv*v-ker))
(defun lv*v-ker (p n nvec va vb)
  "Dot product with two long-float vectors va and vb"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nvec)
           (type (simple-array long-float (*)) va vb))
  (v*v-ker long-float p n nvec va vb))


;; (defmacro v*v (val-type fun va vb)
;;   "Dot product within L1 cache size"
;;   (with-gensyms (na nb nvec tbl m k res i nk)
;;     `(let* ((,na (length ,va))
;;             (,nb (length ,vb))
;;             (,nvec (cond ((/= ,na ,nb) (different-length-warn ,na ,nb)
;;                                        (min ,na ,nb))
;;                         (t ,na)))
;;             (,tbl (type-byte-length ',val-type))
;;             (,m (ifloor +L1-size+ ,tbl))
;;             (,k (min-factor ,nvec ,m))
;;             (,res (coerce 0.0 ',val-type)))
;;        (declare (type fixnum ,na ,nb ,nvec ,tbl ,m ,k)
;;                 (type ,val-type ,res))
;;        (do ((,i 0 (the fixnum (+ ,i ,m))))
;;            ((>= (the fixnum ,i) ,k))
;;          (funcall ,fun ,i ,m ,nvec ,va ,vb ,res))
;;        (when (> ,nvec ,k)
;;          (let ((,nk (- ,nvec ,k)))
;;            (declare (type fixnum ,nk))
;;            (funcall ,fun ,k ,nk ,nvec ,va ,vb ,res)))
;;        ,res)))

(defmacro v*v (val-type fun va vb)
  (with-gensyms (na nb nvec res)
    `(let* ((,na (length ,va))
            (,nb (length ,vb))
            (,nvec (cond ((/= ,na ,nb) (different-length-warn ,na ,nb)
                                       (min ,na ,nb))
                         (t ,na)))
            (,res (coerce 0.0 ',val-type)))
    (declare (type fixnum ,na ,nb ,nvec)
             (type ,val-type ,res))
    (setf ,res (funcall ,fun 0 ,nvec ,nvec ,va ,vb))
    ,res)))


(declaim (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                dv*v))
(defun dv*v (va vb)
  "Dot product with two double-float vectors va and vb"
  (declare (optimize (speed 3))
           (type (simple-array double-float (*)) va vb))
  (v*v double-float #'dv*v-ker va vb))


(declaim (ftype (function ((simple-array long-float (*))
                           (simple-array long-float (*)))
                          long-float)
                lv*v))
(defun lv*v (va vb)
  "Dot product with two double-float vectors va and vb"
  (declare (optimize (speed 3))
           (type (simple-array long-float (*)) va vb))
  (v*v long-float #'lv*v-ker va vb))
