(in-package :cl-user)
(defpackage cl3a.add-vector
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
                :dotimes-unroll
                :different-length-warn)
  (:export :dv+v :lv+v))
(in-package :cl3a.add-vector)


(defmacro v+v-ker (val-type p n nvec va vb a b)
  "Add two vectors between a*va and b*vb"
  (with-gensyms (nv iend res i ip)
    `(let* ((,nv (min ,nvec ,n))
            (,iend (min ,nvec (the fixnum (+ ,p ,n))))
            (,res (make-array ,nv :element-type ',val-type)))
       (declare (type fixnum ,nv ,iend)
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
(defun dv+v-ker (p n nvec va vb a b)
  "Add two double-float vectors between a*va and b*vb"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nvec)
           (type double-float a b)
           (type (simple-array double-float (*)) va vb))
  (v+v-ker double-float p n nvec va vb a b))


(declaim (inline lv+v-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array long-float (*))
                           (simple-array long-float (*))
                           long-float long-float)
                          (simple-array long-float (*)))
                lv+v-ker))
(defun lv+v-ker (p n nvec va vb a b)
  "Add two long-float vectors between va and vb"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nvec)
           (type long-float a b)
           (type (simple-array long-float (*)) va vb))
  (v+v-ker long-float p n nvec va vb a b))


(defmacro v+v (val-type fun a va b vb)
  "Add two vectors within L1 cache-size"
  (with-gensyms (na nb nvec tbl m k vc i res ir ii nk)
    `(let* ((,na (length ,va))
            (,nb (length ,vb))
            (,nvec (cond ((/= ,na ,nb) (different-length-warn ,na ,nb)
                                       (min ,na ,nb))
                         (t ,na)))
            (,tbl (type-byte-length ',val-type))
            (,m (ifloor +L1-size+ ,tbl))
            (,k (min-factor ,nvec ,m))
            (,vc (make-array ,nvec :element-type ',val-type)))
       (declare (type fixnum ,na ,nb ,nvec ,tbl ,m ,k)
                (type (simple-array ,val-type (*)) ,vc))
       (do ((,i 0 (the fixnum (+ ,i ,m))))
           ((>= (the fixnum ,i) ,k))
         (let ((,res (funcall ,fun ,i ,m ,nvec ,va ,vb ,a ,b)))
           (declare (type (simple-array ,val-type (*)) ,res))
           (dotimes (,ir ,m)
             (let ((,ii (+ ,ir ,i)))
               (declare (type fixnum ,ii))
               (setf (aref ,vc ,ii) (aref ,res ,ir))))))
       (when (> ,nvec ,k)
         (let* ((,nk (- ,nvec ,k))
                (,res (funcall ,fun ,k ,nk ,nvec ,va ,vb ,a ,b)))
           (declare (type fixnum ,nk)
                    (type (simple-array ,val-type (*)) ,res))
           (do ((,i ,k (1+ ,i))
                (,ir 0 (1+ ,ir)))
               ((>= ,i ,nvec))
             (setf (aref ,vc ,i) (aref ,res ,ir)))))
       ,vc)))


(declaim (ftype (function (double-float (simple-array double-float (*))
                           double-float (simple-array double-float (*)))
                          (simple-array double-float (*)))
                dv+v))
(defun dv+v (a va b vb)
  "Add two double-float vectors va and vb"
  (declare (optimize (speed 3))
           (type double-float a b)
           (type (simple-array double-float (*)) va vb))
  (v+v double-float #'dv+v-ker a va b vb))


(declaim (ftype (function (long-float (simple-array long-float (*))
                           long-float (simple-array long-float (*)))
                          (simple-array long-float (*)))
                dv+v))
(defun lv+v (a va b vb)
  "Add two long-float vectors va and vb"
  (declare (optimize (speed 3))
           (type long-float a b)
           (type (simple-array long-float (*)) va vb))
  (v+v long-float #'lv+v-ker a va b vb))
