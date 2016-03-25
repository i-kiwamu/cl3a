(in-package :cl-user)
(defpackage cl3a.mmmult
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dm*m :lm*m))
(in-package :cl3a.mmmult)


(defmacro m*m-ker (val-type si ni sk nk nra nv ncb ma mb mc)
  "Multiply matrix and matrix (unrolling version)"
  (with-gensyms (iend jend0 kend i j k maik imb imc)
    `(let* ((,iend (min ,nra (the fixnum (+ ,si ,ni))))
            (,jend0 (min-factor ,ncb +unroll+))
            (,kend (min ,nv (the fixnum (+ ,sk ,nk)))))
       (declare (type fixnum ,iend ,jend0 ,kend))
       (do ((,i ,si (1+ ,i)))
           ((>= ,i ,iend))
         (do ((,k ,sk (1+ ,k)))
             ((>= ,k ,kend))
           (let ((,maik (aref ,ma ,i ,k))
                 (,imb (array-row-major-index ,mb ,k 0))
                 (,imc (array-row-major-index ,mc ,i 0)))
             (declare (type ,val-type ,maik)
                      (type fixnum ,imb ,imc))
             (do ((,j 0 (+ ,j +unroll+)))
                 ((>= ,j ,jend0))
               ,@(loop :repeat +unroll+
                    :with form = `((incf (row-major-aref ,mc ,imc)
                                         (* ,maik (row-major-aref ,mb ,imb)))
                                   (incf ,imb)
                                   (incf ,imc))
                    :append form))
             ;; if ncb < +unroll+ or (mod ncb +unroll+) > 0
             (when (> ,ncb ,jend0)
               (do ((,j ,jend0 (1+ ,j)))
                   ((>= ,j ,ncb))
                 (incf (row-major-aref ,mc ,imc)
                       (* ,maik (row-major-aref ,mb ,imb)))
                 (incf ,imb)
                 (incf ,imc)))))))))



(defmacro m*m (val-type ma mb mc)
  (with-gensyms (calc nra nca nrb ncb nv nt m i k)
    `(flet ((,calc (si ni sk nk nra nv ncb ma mb mc)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type fixnum si ni sk nk nra nv ncb)
                       (type (simple-array ,val-type (* *)) ma mb mc))
              (m*m-ker ,val-type si ni sk nk nra nv ncb ma mb mc)))
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
            (,calc 0 ,nra 0 ,nv ,nra ,nv ,ncb ,ma ,mb ,mc))
           ((= ,m ,nv)
            (dotimes-interval (,i ,m ,nra)
              (,calc ,i ,m 0 ,nv ,nra ,nv ,ncb ,ma ,mb ,mc)))
           (t
            (dotimes-interval (,i ,m ,nra)
              (dotimes-interval (,k ,m ,nv)
                (,calc ,i ,m ,k ,m ,nra ,nv ,ncb ,ma ,mb ,mc)))))))))


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
