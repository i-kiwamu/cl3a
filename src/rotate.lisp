(in-package :cl-user)
(defpackage cl3a.rotate
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:import-from :cl3a.utilities
                :min-factor
                :dotimes-unroll
                :dvvv2-calc-within-L1
                :lvvv2-calc-within-L1
                :different-length-warn)
  (:export :drotate :lrotate))
(in-package :cl3a.rotate)


(defmacro rotate-ker (val-type p n nvec va vb c s)
  "Rotate two vectors (va & vb) with cos and sin,
   and return new two vectors"
  (with-gensyms (nv iend res1 res2 i ip)
    `(let* ((,nv (min ,nvec ,n))
            (,iend (min ,nv (the fixnum (+ ,p ,n))))
            (,res1 (make-array (list ,nv) :element-type ',val-type))
            (,res2 (make-array (list ,nv) :element-type ',val-type)))
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
  (let* ((na (length va))
         (nb (length vb))
         (nvec (cond ((/= na nb) (different-length-warn na nb)
                                 (min na nb))
                     (t na)))
         (vc (make-array (list nvec) :element-type 'double-float))
         (vd (make-array (list nvec) :element-type 'double-float))
         (p 0))
    (declare (type fixnum na nb nvec p)
             (type (simple-array double-float (*)) vc vd))
    (multiple-value-bind (res1 res2)
        (dvvv2-calc-within-L1 #'drotate-ker nvec va vb c s)
      (declare (type (proper-list (simple-array double-float (*)))
                     res1 res2))
      (let ((nres (length res1)))
        (declare (type fixnum nres))
        (dotimes (ir nres)
          (let* ((r1 (nth ir res1))
                 (r2 (nth ir res2))
                 (nr (length r1)))
            (declare (type (simple-array double-float (*)) r1 r2)
                     (type fixnum nr))
            (setf (subseq vc p (+ p nr)) r1)
            (setf (subseq vd p (+ p nr)) r2)
            (incf p nr)))))
    (values vc vd)))


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
  (let* ((na (length va))
         (nb (length vb))
         (nvec (cond ((/= na nb) (different-length-warn na nb)
                                 (min na nb))
                     (t na)))
         (vc (make-array (list nvec) :element-type 'long-float))
         (vd (make-array (list nvec) :element-type 'long-float))
         (p 0))
    (declare (type fixnum na nb nvec p)
             (type (simple-array long-float (*)) vc vd))
    (multiple-value-bind (res1 res2)
        (lvvv2-calc-within-L1 #'drotate-ker nvec va vb c s)
      (declare (type (proper-list (simple-array long-float (*))) res1 res2))
      (let ((nres (length res1)))
        (declare (type fixnum nres))
        (dotimes (ir nres)
          (let* ((r1 (nth ir res1))
                 (r2 (nth ir res2))
                 (nr (length r1)))
            (declare (type (simple-array long-float (*)) r1 r2)
                     (type fixnum nr))
            (setf (subseq vc p (+ p nr)) r1)
            (setf (subseq vd p (+ p nr)) r2)
            (incf p nr)))))
    (values vc vd)))
