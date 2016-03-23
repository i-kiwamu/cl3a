(in-package :cl-user)
(defpackage cl3a.mvmult
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:import-from :cl3a.utilities
                :+L2-size+
                :different-length-warn
                :type-byte-length
                :ifloor
                :min-factor
                :dotimes-unroll
                :dotimes-interval
                :block-size)
  (:export :dm*v :lm*v))
(in-package :cl3a.mvmult)


(defmacro m*v-ker (val-type si ni sj nj nr nc ma vb vc)
  "Multiply matrix and vector"
  (with-gensyms (iend jend i j ima ivb vci)
    `(let* (; (,nv (min ,nj ,nc))
            (,iend (min ,nr (the fixnum (+ ,si ,ni))))
            (,jend (min ,nc (the fixnum (+ ,sj ,nj)))))
       (declare (type fixnum ,iend ,jend))
       ;; (cond
       ;;   ((< ,nv 5)
          (do ((,i ,si (1+ ,i)))
              ((>= ,i ,iend))
            (let ((,ima (array-row-major-index ,ma ,i ,sj))
                  (,ivb (array-row-major-index ,vb ,sj))
                  (,vci (coerce 0.0 ',val-type)))
              (declare (type fixnum ,ima ,ivb)
                       (type ,val-type ,vci))
              (do ((,j ,sj (1+ ,j)))
                  ((>= ,j ,jend))
                (incf ,vci
                      (* (row-major-aref ,ma ,ima)
                         (row-major-aref ,vb ,ivb)))
                (incf ,ima)
                (incf ,ivb))
              (setf (aref ,vc ,i) ,vci))))
         ;; (t
         ;;  (do ((,i ,si (1+ ,i)))
         ;;      ((>= ,i ,iend))
         ;;    (let ((,ima (array-row-major-index ,ma ,i ,sj))
         ;;          (,ivb (array-row-major-index ,vb ,sj))
         ;;          (,vci (coerce 0.0 ',val-type))
         ;;          (,maxj 0))
         ;;      (declare (type fixnum ,ima ,ivb ,maxj)
         ;;               (type ,val-type ,vci))
         ;;      (setf ,maxj
         ;;            (do ((,j ,sj (+ ,j 5)))
         ;;                ((>= ,j ,jend) ,j)
         ;;              (incf ,vci
         ;;                    (+ (* (row-major-aref ,ma ,ima)
         ;;                          (row-major-aref ,vb ,ivb))
         ;;                       ,@(loop :repeat 4 :append
         ;;                            (append
         ;;                             `((* (row-major-aref ,ma (incf ,ima))
         ;;                                  (row-major-aref ,vb (incf ,ivb))))))))
         ;;              (incf ,ima)
         ;;              (incf ,ivb)))
         ;;      ;; if maxi < iend, calculate the rest of elements
         ;;      (do ((,j ,maxj (1+ ,j)))
         ;;          ((>= ,j ,jend))
         ;;        (incf ,vci
         ;;              (* (row-major-aref ,ma ,ima)
         ;;                 (row-major-aref ,vb ,ivb)))
         ;;        (incf ,ima)
         ;;        (incf ,ivb))
         ;;      (incf (aref ,vc ,i) ,vci))))))))
    ))


(declaim (inline dm*v-ker)
         (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (*))
                           (simple-array double-float (*))))
                dm*v-ker))
(defun dm*v-ker (si ni sj nj nr nc ma vb vc)
  "Multiply matrix and vector of double-float"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum si ni sj nj nr nc)
           (type (simple-array double-float (* *)) ma)
           (type (simple-array double-float (*)) vb vc))
  (m*v-ker double-float si ni sj nj nr nc ma vb vc))
(declaim (notinline dm*v-ker))


(declaim (inline lm*v-ker)
         (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum
                           (simple-array long-float (* *))
                           (simple-array long-float (*))
                           (simple-array long-float (*))))
                lm*v-ker))
(defun lm*v-ker (si ni sj nj nr nc ma vb vc)
  "Multiply matrix and vector of long-float"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum si ni sj nj nr nc)
           (type (simple-array long-float (* *)) ma)
           (type (simple-array long-float (*)) vb vc))
  (m*v-ker long-float si ni sj nj nr nc ma vb vc))
(declaim (notinline lm*v-ker))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (*))
                           (simple-array double-float (*))))
                dm*v))
(defun dm*v (ma vb vc)
  "Multiply matrix and vector of double-float"
  (declare (optimize (speed 3))
           (inline dm*v-ker)
           (type (simple-array double-float (* *)) ma)
           (type (simple-array double-float (*)) vb vc))
  (let* ((nra (array-dimension ma 0))
         (nca (array-dimension ma 1))
         (nb (length vb))
         (nj (cond ((/= nca nb) (different-length-warn nca nb)
                                (min nca nb))
                   (t nca)))
         (m (block-size (min nra nj))))
    (declare (type fixnum nra nca nb nj m))
    (cond ((= m 0)
           (dotimes (i nra)
             (dotimes (j nj)
               (dm*v-ker i 1 j 1 nra nca ma vb vc))))
          (t
           (dotimes-interval (i m nra)
             (dm*v-ker i m 0 nj nra nca ma vb vc))))))


(declaim (ftype (function ((simple-array long-float (* *))
                           (simple-array long-float (*))
                           (simple-array long-float (*))))
                lm*v))
(defun lm*v (ma vb vc)
  "Multiply matrix and vector of long-float"
  (declare (optimize (speed 3))
           (inline lm*v-ker)
           (type (simple-array long-float (* *)) ma)
           (type (simple-array long-float (*)) vb))
  (let* ((nra (array-dimension ma 0))
         (nca (array-dimension ma 1))
         (nb (length vb))
         (nj (cond ((/= nca nb) (different-length-warn nca nb)
                                (min nca nb))
                   (t nca)))
         (m (block-size (min nra nj))))
    (declare (type fixnum nra nca nb nj m))
    (cond ((= m 0)
           (dotimes (i nra)
             (dotimes (j nj)
               (lm*v-ker i 1 j 1 nra nca ma vb vc))))
          (t
           (dotimes-interval (i m nra)
             (lm*v-ker i m 0 nj nra nca ma vb vc))))))
