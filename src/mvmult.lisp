(in-package :cl-user)
(defpackage cl3a.mvmult
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:import-from :cl3a.utilities
                :min-factor
                :dotimes-unroll
                :dmvv-calc-within-L1
                :lmvv-calc-within-L1
                :different-length-warn)
  (:export :dm*v :lm*v))
(in-package :cl3a.mvmult)


(defmacro m*v-ker (val-type p n nr nc ma vb)
  "Multiply matrix and vector"
  (with-gensyms (nvec iend res i ip j)
    `(let* ((,nvec (min ,nr ,n))
            (,iend (min ,nr (the fixnum (+ ,p ,n))))
            (,res (make-array (list ,nvec) :element-type ',val-type)))
       (declare (type fixnum ,nvec ,iend)
                (type (simple-array ,val-type (*)) ,res))
       (do ((,i ,p (1+ ,i)))
           ((>= ,i ,iend))
         (let ((,ip (- (the fixnum ,i) ,p)))
           (declare (type fixnum ,ip))
           (dotimes-unroll (,j 0 ,nc)
             (setf (aref ,res ,ip)
                   (* (aref ,ma ,i ,j) (aref ,vb ,j))))))
       ,res)))


(declaim (inline dm*v-ker)
         (ftype (function (fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (*)))
                          (simple-array double-float (*)))
                dm*v-ker))
(defun dm*v-ker (p n nr nc ma vb)
  "Multiply matrix and vector of double-float"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nr)
           (type (simple-array double-float (* *)) ma)
           (type (simple-array double-float (*)) vb))
  (m*v-ker double-float p n nr nc ma vb))


(declaim (inline lm*v-ker)
         (ftype (function (fixnum fixnum fixnum fixnum
                           (simple-array long-float (* *))
                           (simple-array long-float (*)))
                          (simple-array long-float (*)))
                lm*v-ker))
(defun lm*v-ker (p n nr nc ma vb)
  "Multiply matrix and vector of long-float"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum p n nr)
           (type (simple-array long-float (* *)) ma)
           (type (simple-array long-float (*)) vb))
  (m*v-ker long-float p n nr nc ma vb))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (*)))
                          (simple-array double-float (*)))
                dm*v))
(defun dm*v (ma vb)
  "Multiply matrix and vector of double-float"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type (simple-array double-float (* *)) ma)
           (type (simple-array double-float (*)) vb))
  (let* ((nr (array-dimension ma 0))
         (nc (array-dimension ma 1))
         (res (dmvv-calc-within-L1 #'dm*v-ker nr nc ma vb))
         (vc (make-array (list nr) :element-type 'double-float))
         (p 0))
    (declare (type fixnum nr nc p)
             (type (proper-list (simple-array double-float (*))) res)
             (type (simple-array double-float (*)) vc))
    (dolist (r res)
      (declare (type (simple-array double-float (*)) r))
      (let ((nir (length r)))
        (declare (type fixnum nir))
        (setf (subseq vc p (+ p nir)) r)
        (incf p nir)))
    vc))


(declaim (ftype (function ((simple-array long-float (* *))
                           (simple-array long-float (*)))
                          (simple-array long-float (*)))
                lm*v))
(defun lm*v (ma vb)
  "Multiply matrix and vector of long-float"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type (simple-array long-float (* *)) ma)
           (type (simple-array long-float (*)) vb))
  (let* ((nr (array-dimension ma 0))
         (nc (array-dimension ma 1))
         (res (lmvv-calc-within-L1 #'lm*v-ker nr nc ma vb))
         (vc (make-array (list nr) :element-type 'long-float))
         (p 0))
    (declare (type fixnum nr nc p)
             (type (proper-list (simple-array long-float (*))) res)
             (type (simple-array long-float (*)) vc))
    (dolist (r res)
      (declare (type (simple-array long-float (*)) r))
      (let ((nir (length r)))
        (declare (type fixnum nir))
        (setf (subseq vc p (+ p nir)) r)
        (incf p nir)))
    vc))
