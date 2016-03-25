(in-package :cl-user)
(defpackage cl3a.mmmult
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dm*m :lm*m))
(in-package :cl3a.mmmult)


(defmacro m*m-ker (val-type si ni sk nk sj nj nra nv ncb ma mb mc)
  "Multiply matrix and matrix"
  (with-gensyms (iend jend jend0 kend maxj i j k maik imb imc)
    `(let* ((,iend (min ,nra (the fixnum (+ ,si ,ni))))
            (,jend (min ,ncb (the fixnum (+ ,sj ,nj))))
            (,jend0 (min-factor (- ,jend ,sj) +unroll+))
            (,kend (min ,nv (the fixnum (+ ,sk ,nk))))
            (,maxj 0))
       (declare (type fixnum ,iend ,jend ,jend0 ,kend ,maxj))
       (do ((,i ,si (1+ ,i)))
           ((>= ,i ,iend))
         (do ((,k ,sk (1+ ,k)))
             ((>= ,k ,kend))
           (let ((,maik (aref ,ma ,i ,k))
                 (,imb (array-row-major-index ,mb ,k ,sj))
                 (,imc (array-row-major-index ,mc ,i ,sj)))
             (declare (type ,val-type ,maik)
                      (type fixnum ,imb ,imc))
             (setf ,maxj
                   (do ((,j ,sj (+ ,j +unroll+)))
                       ((>= ,j ,jend0) ,j)
                     ,@(loop :repeat +unroll+
                          :with form = `((incf (row-major-aref ,mc ,imc)
                                               (* ,maik (row-major-aref ,mb ,imb)))
                                         (incf ,imb)
                                         (incf ,imc))
                          :append form)))
             (when (> ,jend ,maxj)
               (do ((,j ,maxj (1+ ,j)))
                   ((>= ,j ,jend))
                 (incf (row-major-aref ,mc ,imc)
                       (* ,maik (row-major-aref ,mb ,imb)))
                 (incf ,imb)
                 (incf ,imc)))))))))



(defmacro m*m (val-type ma mb mc)
  (with-gensyms (calc nra nca nrb ncb nv nt m i k)
    `(flet ((,calc (si ni sk nk sj nj nra nv ncb ma mb mc)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type fixnum si ni sk nk sj nj nra nv ncb)
                       (type (simple-array ,val-type (* *)) ma mb mc))
              (m*m-ker ,val-type si ni sk nk sj nj nra nv ncb ma mb mc)))
       (declare (inline ,calc))
       (let* ((,nra (array-dimension ,ma 0))
              (,nca (array-dimension ,ma 1))
              (,nrb (array-dimension ,mb 0))
              (,ncb (array-dimension ,mb 1))
              (,nv (cond ((/= ,nca ,nrb)
                          (different-length-warn ,nca ,nrb)
                          (min ,nca ,nrb))
                         (t ,nca)))
              (,nt (min ,nra ,ncb))
              (,m (block-size ,nt)))
         (declare (type fixnum ,nra ,nca ,nrb ,ncb ,nv ,nt ,m))
         (cond
           ((= ,m ,nra)
            (,calc 0 ,nra 0 ,nv 0 ,ncb ,nra ,nv ,ncb ,ma ,mb ,mc))
           ((= ,m ,nv)
            (dotimes-interval (,i ,m ,nra)
              (,calc ,i ,m 0 ,nv 0 ,ncb ,nra ,nv ,ncb ,ma ,mb ,mc)))
           (t
            (dotimes-interval (,i ,m ,nra)
              (dotimes-interval (,k ,m ,nv)
                (,calc ,i ,m ,k ,m 0 ,ncb ,nra ,nv ,ncb ,ma ,mb ,mc)))))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m))
(defun dm*m (ma mb mc)
  "Multiply matrix and matrix of double-float"
  (declare (type (simple-array double-float (* *)) ma mb mc))
  (m*m double-float ma mb mc))


(declaim (ftype (function ((simple-array long-float (* *))
                           (simple-array long-float (* *))
                           (simple-array long-float (* *))))
                lm*m))
(defun lm*m (ma mb mc)
  (declare (type (simple-array long-float (* *)) ma mb mc))
  (m*m long-float ma mb mc))
