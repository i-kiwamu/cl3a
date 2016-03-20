(in-package :cl-user)
(defpackage cl3a.rotate
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:import-from :cl3a.utilities
                :+L1-size+
                :type-byte-length
                :ifloor
                :min-factor
                :dotimes-unroll
                :different-length-warn)
  (:export :drotate :lrotate))
(in-package :cl3a.rotate)


(defmacro rotate-ker (val-type p n nvec va vb c s)
  "Rotate two vectors (va & vb) with cos and sin,
   and return new two vectors"
  (with-gensyms (nv iend res1 res2 i ip)
    `(let* ((,nv (min ,nvec ,n))
            (,iend (min ,nv (the fixnum (+ ,p ,n))))
            (,res1 (make-array ,nv :element-type ',val-type))
            (,res2 (make-array ,nv :element-type ',val-type)))
       (declare (type fixnum ,nv ,iend)
                (type (simple-array ,val-type (*)) ,res1 ,res2))
       (dotimes-unroll (,i ,p ,iend)
         (let ((,ip (- (the fixnum ,i) ,p)))
           (declare (type fixnum ,ip))
           (setf (aref ,res1 ,ip) (+ (* ,c (aref ,va ,i))
                                     (* ,s (aref ,vb ,i))))
           (setf (aref ,res2 ,ip) (- (* ,c (aref ,vb ,i))
                                     (* ,s (aref ,va ,i))))))
       (values ,res1 ,res2))))


(declaim (inline drotate-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array double-float (*))
                           (simple-array double-float (*))
                           (double-float -1d0 1d0) (double-float -1d0 1d0))
                          (values (simple-array double-float (*))
                                  (simple-array double-float (*))))
                drotate-ker))
(defun drotate-ker (p n nvec va vb c s)
  "Rotate two double-float vectors (va & vb) with cos and sin,
   and return new two vectors"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nvec)
           (type (simple-array double-float (*)) va vb)
           (type double-float c s))
  (rotate-ker double-float p n nvec va vb c s))


(declaim (inline lrotate-ker)
         (ftype (function (fixnum fixnum fixnum
                           (simple-array long-float (*))
                           (simple-array long-float (*))
                           (long-float -1l0 1l0) (long-float -1l0 1l0))
                          (values (simple-array long-float (*))
                                  (simple-array long-float (*))))
                lrotate-ker))
(defun lrotate-ker (p n nvec va vb c s)
  "Rotate two long-float vectors (va & vb) with cos and sin,
   and return new two vectors"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nvec)
           (type (simple-array long-float (*)) va vb)
           (type long-float c s))
  (rotate-ker long-float p n nvec va vb c s))


(defmacro vrotate (val-type fun va vb c s)
  "Rotate two vectors within L1 cache size"
  (with-gensyms (na nb nvec tbl m k vc vd res1 res2 i ii ir nk)
    `(let* ((,na (length ,va))
            (,nb (length ,vb))
            (,nvec (cond ((/= ,na ,nb) (different-length-warn ,na ,nb)
                                       (min ,na ,nb))
                         (t ,na)))
            (,tbl (type-byte-length ',val-type))
            (,m (ifloor +L1-size+ ,tbl))
            (,k (min-factor ,nvec ,m))
            (,vc (make-array ,nvec :element-type ',val-type))
            (,vd (make-array ,nvec :element-type ',val-type)))
       (declare (type fixnum ,na ,nb ,nvec ,tbl ,m ,k)
                (type (simple-array ,val-type (*)) ,vc ,vd))
       (do ((,i 0 (the fixnum (+ ,i ,m))))
           ((>= (the fixnum ,i) ,k))
         (multiple-value-bind (,res1 ,res2)
             (funcall ,fun ,i ,m ,nvec ,va ,vb ,c ,s)
           (declare (type (simple-array ,val-type (*)) ,res1 ,res2))
           (dotimes (,ir ,m)
             (let ((,ii (+ ,ir ,i)))
               (declare (type fixnum ,ii))
               (setf (aref ,vc ,ii) (aref ,res1 ,ir))
               (setf (aref ,vd ,ii) (aref ,res2 ,ir))))))
       (when (> ,nvec ,k)
         (let ((,nk (- ,nvec ,k)))
           (declare (type fixnum ,nk))
           (multiple-value-bind (,res1 ,res2)
               (funcall ,fun ,k ,nk ,nvec ,va ,vb ,c ,s)
             (do ((,i ,k (1+ ,i))
                  (,ir 0 (1+ ,ir)))
                 ((>= ,i ,nvec))
               (setf (aref ,vc ,i) (aref ,res1 ,ir))
               (setf (aref ,vd ,i) (aref ,res2 ,ir))))))
       (values ,vc ,vd))))


(declaim (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*))
                           (double-float -1d0 1d0) (double-float -1d0 1d0))
                          (values (simple-array double-float (*))
                                  (simple-array double-float (*))))
                drotate))
(defun drotate (va vb c s)
  "Rotate two double-float vectors (va & vb) with cos and sin,
   and return new two vectors"
  (declare (optimize (speed 3))
           (type (simple-array double-float (*)) va vb)
           (type (double-float 0d0 1d0) c s))
  (vrotate double-float #'drotate-ker va vb c s))


(declaim (ftype (function ((simple-array long-float (*))
                           (simple-array long-float (*))
                           (long-float -1l0 1l0) (long-float -1l0 1l0))
                          (values (simple-array long-float (*))
                                  (simple-array long-float (*))))
                lrotate))
(defun lrotate (va vb c s)
  "Rotate two long-float vectors (va & vb) with cos and sin,
   and return new two vectors"
  (declare (optimize (speed 3))
           (type (simple-array long-float (*)) va vb)
           (type (long-float 0l0 1l0) c s))
  (vrotate long-float #'lrotate-ker va vb c s))
