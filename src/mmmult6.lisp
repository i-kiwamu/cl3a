(in-package :cl-user)
(defpackage cl3a.mmmult6
  (:use :cl :alexandria :cl-simd :cl3a.utilities)
  (:export :dm*m :lm*m))
(in-package :cl3a.mmmult6)


(declaim (ftype (function (integer &key (:l1 boolean)) integer)
                tile-size))
(defun tile-size (n &key l1)
  "See Lam et al. 1991 The cache performance and optimizations of blocked algorithms"
  (declare (type integer n)
           (type boolean l1))
  (let* ((n-half (ifloor n 2))
         (cache-size (if l1
                         (ifloor +L1-size+ 8)
                         (ifloor +L2-size+ 8))))  ;; 1 word = 4 byte
    (declare (type integer n-half cache-size))
    (loop :while t
       :with max-width :of-type integer = (min n cache-size)
       :and addr :of-type integer = n-half
       :and di :of-type integer = 0
       :and dj :of-type integer = 1
       :and di0 :of-type integer = 1
       :do (progn
             (incf addr cache-size)
             (setf di (ifloor addr n))
             (setf dj (abs (- (mod addr n) n-half)))
             (setf di0 (min max-width dj)))
       (when (>= di di0)
         (return (min max-width di)))
       :do (setf max-width di0))))


(defmacro m*m-ker (val-type si ni sk nk nra nv ncb ma mb mc)
  "Multiply matrix and matrix (unrolling version)"
  (with-gensyms (iend jend0 kend i j k maik imb imc maxj)
    `(let* ((,iend (min ,nra (the fixnum (+ ,si ,ni))))
            (,jend0 (min-factor ,ncb +unroll+))
            (,kend (min ,nv (the fixnum (+ ,sk ,nk))))
            (,maxj 0))
       (declare (type fixnum ,iend ,jend0 ,kend ,maxj))
       (do ((,i ,si (1+ ,i)))
           ((>= ,i ,iend))
         (do ((,k ,sk (1+ ,k)))
             ((>= ,k ,kend))
           (let ((,maik (aref ,ma ,i ,k))
                 (,imb (array-row-major-index ,mb ,k 0))
                 (,imc (array-row-major-index ,mc ,i 0)))
             (declare (type ,val-type ,maik)
                      (type fixnum ,imb ,imc))
             (setf ,maxj
                   (do ((,j 0 (+ ,j +unroll+)))
                       ((>= ,j ,jend0) ,j)
                     ,@(loop :repeat +unroll+
                          :with form = `((incf (row-major-aref ,mc ,imc)
                                               (* ,maik (row-major-aref ,mb ,imb)))
                                         (incf ,imb)
                                         (incf ,imc))
                          :append form)))
             ;; if jend < +unroll+ or (mod jend +unroll+) > 0
             (do ((,j ,maxj (1+ ,j)))
                 ((>= ,j ,ncb))
               (incf (row-major-aref ,mc ,imc)
                     (* ,maik (row-major-aref ,mb ,imb)))
               (incf ,imb)
               (incf ,imc))))))))



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
              (,m (tile-size ,nt)))
         (declare (type fixnum ,nra ,nca ,nrb ,ncb ,nv ,nt ,m))
         (dotimes-interval (,i ,m ,nra)
           (dotimes-interval (,k ,m ,nv)
             (,calc ,i ,m ,k ,m ,nra ,nv ,ncb ,ma ,mb ,mc)))))))


(declaim (ftype (function ((sse-array double-float (* *))
                           (sse-array double-float (* *))
                           (sse-array double-float (* *))))
                dm*m))
(defun dm*m (ma mb mc)
  "Multiply matrix and matrix of double-float"
  (declare (optimize (speed 3) (safety 0))
           (type (sse-array double-float (* *)) ma mb mc))
  (m*m double-float ma mb mc))


(declaim (ftype (function ((sse-array long-float (* *))
                           (sse-array long-float (* *))
                           (sse-array long-float (* *))))
                lm*m))
(defun lm*m (ma mb mc)
  (declare (optimize (speed 3) (safety 0))
           (type (sse-array long-float (* *)) ma mb mc))
  (m*m long-float ma mb mc))
