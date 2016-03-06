(in-package :cl-user)
(defpackage cl3a.add-vector
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:import-from :cl3a.utilities
                :min-factor
                :setf-array-range
                :dvvv-calc-within-L1
                :lvvv-calc-within-L1
                :different-length-warn)
  (:export :dv+v :lv+v))
(in-package :cl3a.add-vector)


(defmacro v+v-ker (val-type p n nv va vb a b)
  "Add two vectors between va and vb"
  (with-gensyms (nvec n5 res i maxi i1 i2 i3 i4)
    `(let* ((,nvec (min ,nv (the fixnum (+ ,p ,n))))
            (,n5 (min-factor ,nvec 5))
            (,res (make-array (list ,nvec) :element-type (quote ,val-type))))
       (declare (type fixnum ,nvec ,n5)
                (type (simple-array ,val-type (*)) ,res))
       (cond
         ((or (>= ,p ,nv) (< ,p 0) (< ,nvec 0))
          (warn (format nil "Position ~D is out of range of vectors with length of ~D." ,p ,nvec)))
         ((< ,nvec 5) (do ((,i ,p (+ ,i 1)))
                          ((>= ,i ,nvec))
                        (setf (aref ,res ,i) (+ (* ,a (aref ,va ,i))
                                                (* ,b (aref ,vb ,i))))))
         (t (let ((,maxi
                   (do ((,i ,p (+ ,i 5))
                        (,i1 (the fixnum (+ ,p 1)) (the fixnum (+ ,i1 5)))
                        (,i2 (the fixnum (+ ,p 2)) (the fixnum (+ ,i2 5)))
                        (,i3 (the fixnum (+ ,p 3)) (the fixnum (+ ,i3 5)))
                        (,i4 (the fixnum (+ ,p 4)) (the fixnum (+ ,i4 5))))
                       ((>= ,i ,n5) ,i)
                     (setf (aref ,res ,i)  (+ (* ,a (aref ,va ,i))
                                              (* ,b (aref ,vb ,i))))
                     (setf (aref ,res ,i1) (+ (* ,a (aref ,va ,i1))
                                              (* ,b (aref ,vb ,i1))))
                     (setf (aref ,res ,i2) (+ (* ,a (aref ,va ,i2))
                                              (* ,b (aref ,vb ,i2))))
                     (setf (aref ,res ,i3) (+ (* ,a (aref ,va ,i3))
                                              (* ,b (aref ,vb ,i3))))
                     (setf (aref ,res ,i4) (+ (* ,a (aref ,va ,i4))
                                              (* ,b (aref ,vb ,i4)))))))
              (declare (type fixnum ,maxi))
              (when (< ,maxi ,nvec)
                (do ((,i ,maxi (+ ,i 1)))
                    ((>= ,i ,nvec))
                  (setf (aref ,res ,i) (+ (* ,a (aref ,va ,i))
                                          (* ,b (aref ,vb ,i)))))))))
       ,res)))


(declaim (inline dv+v-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array double-float (*))
                           (simple-array double-float (*))
                           double-float double-float)
                          (simple-array double-float (*)))
                dv+v-ker))
(defun dv+v-ker (p n nv va vb a b)
  "Add two double-float vectors between va and vb"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nv)
           (type double-float a b)
           (type (simple-array double-float (*)) va vb))
  (v+v-ker double-float p n nv va vb a b))


(declaim (inline lv+v-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array long-float (*))
                           (simple-array long-float (*))
                           long-float long-float)
                          (simple-array long-float (*)))
                lv+v-ker))
(defun lv+v-ker (p n nv va vb a b)
  "Add two long-float vectors between va and vb"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nv)
           (type long-float a b)
           (type (simple-array long-float (*)) va vb))
  (v+v-ker long-float p n nv va vb a b))


(declaim (ftype (function (double-float (simple-array double-float (*))
                           double-float (simple-array double-float (*)))
                          (simple-array double-float (*)))
                dv+v))
(defun dv+v (a va b vb)
  "Dot product with two double-float vectors va and vb"
  (declare (optimize (speed 3))
           (type double-float a b)
           (type (simple-array double-float (*)) va vb))
  (let* ((na (length va))
         (nb (length vb))
         (nv (cond ((/= na nb) (different-length-warn na nb)
                               (min na nb))
                   (t na)))
         (res (dvvv-calc-within-L1 #'dv+v-ker nv va vb a b))
         (vc (make-array (list nv) :element-type 'double-float)))
    (declare (type fixnum na nb nv)
             (type (proper-list (simple-array double-float (*))) res))
    (let ((p 0))
      (declare (type fixnum p))
      (dolist (r res)
        (declare (type (simple-array double-float (*)) r))
        (setf-array-range vc p r)
        (incf p (length r))))
    vc))


(declaim (ftype (function (long-float (simple-array long-float (*))
                           long-float (simple-array long-float (*)))
                          (simple-array long-float (*)))
                dv+v))
(defun lv+v (a va b vb)
  "Dot product with two long-float vectors va and vb"
  (declare (optimize (speed 3))
           (type long-float a b)
           (type (simple-array long-float (*)) va vb))
  (let* ((na (length va))
         (nb (length vb))
         (nv (cond ((/= na nb) (different-length-warn na nb)
                               (min na nb))
                   (t na)))
         (res (dvvv-calc-within-L1 #'lv+v-ker nv va vb a b))
         (vc (make-array (list nv) :element-type 'long-float)))
    (declare (type fixnum na nb nv)
             (type (proper-list (simple-array long-float (*))) res))
    (let ((p 0))
      (declare (type fixnum p))
      (dolist (r res)
        (declare (type (simple-array long-float (*)) r))
        (setf-array-range vc p r)
        (incf p (length r))))
    vc))
