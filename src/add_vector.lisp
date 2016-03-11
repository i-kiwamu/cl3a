(in-package :cl-user)
(defpackage cl3a.add-vector
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:import-from :cl3a.utilities
                :min-factor
                :dotimes-unroll
                :dvvv-calc-within-L1
                :lvvv-calc-within-L1
                :different-length-warn)
  (:export :dv+v :lv+v))
(in-package :cl3a.add-vector)


(defmacro v+v-ker (val-type p n nv va vb a b)
  "Add two vectors between a*va and b*vb"
  (with-gensyms (nvec iend res i ip)
    `(let* ((,nvec (min ,nv ,n))
            (,iend (min ,nv (the fixnum (+ ,p ,n))))
            (,res (make-array (list ,nvec) :element-type ',val-type)))
       (declare (type fixnum ,nvec ,iend)
                (type (simple-array ,val-type (*)) ,res))
       (dotimes-unroll (,i ,p ,iend)
         (let ((,ip (- (the fixnum ,i) ,p)))
           (declare (type fixnum ,ip))
           (setf (aref ,res ,ip) (+ (* ,a (aref ,va ,i))
                                    (* ,b (aref ,vb ,i))))))
       ,res)))


(declaim (inline dv+v-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array double-float (*))
                           (simple-array double-float (*))
                           double-float double-float)
                          (simple-array double-float (*)))
                dv+v-ker))
(defun dv+v-ker (p n nv va vb a b)
  "Add two double-float vectors between a*va and b*vb"
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
         (vc (make-array (list nv) :element-type 'double-float))
         (p 0))
    (declare (type fixnum na nb nv p)
             (type (proper-list (simple-array double-float (*))) res)
             (type (simple-array double-float (*)) vc))
    (dolist (r res)
      (declare (type (simple-array double-float (*)) r))
      (let ((nr (length r)))
        (declare (type fixnum nr))
        (setf (subseq vc p (+ p nr)) r)
        (incf p nr)))
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
        (let ((nr (length r)))
          (declare (type fixnum nr))
          (setf (subseq vc p (+ p nr)) r)
          (incf p nr))))
    vc))
