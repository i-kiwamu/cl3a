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
                :dotimes-unroll
                :dotimes-interval)
  (:export :dm*m :lm*m))
(in-package :cl3a.mmmult)


(defmacro m*m-ker (val-type si ni sj nj nra nca ncb ma mb mc)
  "Multiply matrix and matrix"
  (with-gensyms (iend jend i j k maik imb imc)
    `(let* ((,iend (min ,nra (the fixnum (+ ,si ,ni))))
            (,jend (min ,ncb (the fixnum (+ ,sj ,nj)))))
       (declare (type fixnum ,iend ,jend))
       (do ((,i ,si (1+ ,i)))
           ((>= ,i ,iend))
         (do ((,k 0 (1+ ,k)))
             ((>= ,k ,nca))
           (let ((,maik (aref ,ma ,i ,k))
                 (,imb (array-row-major-index ,mb ,k ,sj))
                 (,imc (array-row-major-index ,mc ,i ,sj)))
             (declare (type ,val-type ,maik)
                      (type fixnum ,imb ,imc))
             (do ((,j ,sj (1+ ,j)))
                 ((>= ,j ,jend))
               (incf (row-major-aref ,mc ,imc)
                     (* ,maik (row-major-aref ,mb ,imb)))
               (incf ,imb)
               (incf ,imc))))))))


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
(declaim (notinline dm*m-ker))


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
(declaim (notinline lm*m-ker))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m))
(defun dm*m (ma mb mc)
  "Multiply matrix and matrix of double-float"
  (declare (optimize (speed 3))
           (inline dm*m-ker)
           (type (simple-array double-float (* *)) ma mb mc))
  (let* ((nra (array-dimension ma 0))
         (nca (array-dimension ma 1))
         (nrb (array-dimension mb 0))
         (ncb (array-dimension mb 1))
         (nv (cond ((/= nca nrb)
                    (different-length-warn nca nrb)
                    (min nca nrb))
                   (t nca)))
         (tbl (type-byte-length 'double-float))
         (nt (min nra ncb))
         (m (ifloor +L1-size+ tbl nt nt)))
    (declare (type fixnum nra nca nrb ncb nv tbl nt m))
    (if (= m 0)
        (dotimes (i nra)
          (dotimes (j ncb)
            (dm*m-ker i 1 j 1 nra nv ncb ma mb mc)))
        (dotimes-interval (i m nra)
          (dotimes-interval (j m ncb)
            (dm*m-ker i m j m nra nv ncb ma mb mc))))))


(declaim (ftype (function ((simple-array long-float (* *))
                           (simple-array long-float (* *))
                           (simple-array long-float (* *))))
                lm*m))
(defun lm*m (ma mb mc)
  "Multiply matrix and matrix of long-float"
  (declare (optimize (speed 3))
           (inline lm*m-ker)
           (type (simple-array long-float (* *)) ma mb))
  (let* ((nra (array-dimension ma 0))
         (nca (array-dimension ma 1))
         (nrb (array-dimension mb 0))
         (ncb (array-dimension mb 1))
         (nv (cond ((/= nca nrb)
                    (different-length-warn nca nrb)
                    (min nca nrb))
                   (t nca)))
         (tbl (type-byte-length 'long-float))
         (nt (min nra ncb))
         (m (ifloor +L1-size+ tbl nt nt)))
    (declare (type fixnum nra nca nrb ncb nv tbl nt m))
    (if (= m 0)
        (dotimes (i nra)
          (dotimes (j ncb)
            (lm*m-ker i 1 j 1 nra nv ncb ma mb mc)))
        (dotimes-interval (i m nra)
          (dotimes-interval (j m ncb)
            (lm*m-ker i m j m nra nv ncb ma mb mc))))))
