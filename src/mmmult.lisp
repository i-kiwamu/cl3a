(in-package :cl-user)
(defpackage cl3a.mmmult
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:import-from :cl3a.utilities
                :+L1-size+
                :different-length-warn
                :type-byte-length
                :ifloor
                :min-factor
                :dotimes-unroll)
  (:export :dm*m :lm*m))
(in-package :cl3a.mmmult)


(defmacro m*m-ker (val-type si ni sj nj nra nca ncb ma mb mc)
  "Multiply matrix and matrix"
  (with-gensyms (iend jend i j k maik mb-index mc-index)
    `(let* ((,iend (min ,nra (the fixnum (+ ,si ,ni))))
            (,jend (min ,ncb (the fixnum (+ ,sj ,nj)))))
       (declare (type fixnum ,iend ,jend))
       (do ((,i ,si (1+ ,i)))
           ((>= ,i ,iend))
         (do ((,k 0 (1+ ,k)))
             ((>= ,k ,nca))
           (let ((,maik (aref ,ma ,i ,k))
                 (,mb-index (array-row-major-index ,mb ,k ,sj))
                 (,mc-index (array-row-major-index ,mc ,i ,sj)))
             (declare (type ,val-type ,maik)
                      (type fixnum ,mb-index ,mc-index))
             (dotimes-unroll (,j ,sj ,jend)
               (incf (row-major-aref ,mc ,mc-index)
                     (* ,maik (row-major-aref ,mb ,mb-index)))
               (incf ,mb-index)
               (incf ,mc-index))))))))


(declaim (inline dm*m-ker)
         (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m-ker))
(defun dm*m-ker (si ni sj nj nra nca ncb ma mb mc)
  "Multiply matrix and matrix of double-float"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum si ni sj nj nra nca ncb)
           (type (simple-array double-float (* *)) ma mb mc))
  (m*m-ker double-float si ni sj nj nra nca ncb ma mb mc))


(declaim (inline lm*m-ker)
         (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum fixnum
                           (simple-array long-float (* *))
                           (simple-array long-float (* *))
                           (simple-array long-float (* *))))
                lm*m-ker))
(defun lm*m-ker (si ni sj nj nra nca ncb ma mb mc)
  "Multiply matrix and matrix of long-float"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum si ni sj nj nra nca ncb)
           (type (simple-array long-float (* *)) ma mb mc))
  (m*m-ker long-float si ni sj nj nra nca ncb ma mb mc))


(defmacro m*m (val-type fun ma mb)
  (with-gensyms (nra nca nrb ncb nvec mc)
    `(let* ((,nra (array-dimension ,ma 0))
            (,nca (array-dimension ,ma 1))
            (,nrb (array-dimension ,mb 0))
            (,ncb (array-dimension ,mb 1))
            (,nvec (cond ((/= ,nca ,nrb)
                          (different-length-warn ,nca ,nrb)
                          (min ,nca ,nrb))
                         (t ,nca)))
            (,mc (make-array (list ,nra ,ncb) :element-type ',val-type)))
    (declare (type fixnum ,nra ,nca ,nrb ,ncb ,nvec)
             (type (simple-array ,val-type (* *)) ,mc))
    (funcall ,fun 0 ,nra 0 ,ncb ,nra ,nvec ,ncb ,ma ,mb ,mc)
    ,mc)))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *)))
                          (simple-array double-float (* *)))
                dm*m))
(defun dm*m (ma mb)
  "Multiply matrix and matrix of double-float"
  (declare (optimize (speed 3))
           (type (simple-array double-float (* *)) ma mb))
  (m*m double-float #'dm*m-ker ma mb))


(declaim (ftype (function ((simple-array long-float (* *))
                           (simple-array long-float (* *)))
                          (simple-array long-float (* *)))
                lm*m))
(defun lm*m (ma mb)
  "Multiply matrix and matrix of long-float"
  (declare (optimize (speed 3))
           (type (simple-array long-float (* *)) ma mb))
  (m*m long-float #'lm*m-ker ma mb))
