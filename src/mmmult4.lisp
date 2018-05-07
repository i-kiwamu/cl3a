(in-package :cl-user)
(defpackage cl3a.mmmult4
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dm*m :lm*m))
(in-package :cl3a.mmmult4)


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


(defmacro m*m-ker (val-type sj nj sk nk nra nv ncb ma mb mc)
  "Multiply matrix and matrix (unrolling version)"
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   sj: block index for nc
  ;;   nj: block size for nc
  ;;   sk: block index for nv
  ;;   nk: block size for nv
  ;;   nra: number of row of ma
  ;;   nv: number of cols of ma & number of rows of mb
  ;;   ncb: number of cols of nc
  ;;   ma: input matrix with nra*nv
  ;;   mb: input matrix with nv*ncb
  ;;   mc: output matrix with nra*ncb
  (with-gensyms (iend jend0 kend i j k maik imb imc maxj)
    `(let* ((,iend ,nra)
            (,jend0 (min ,ncb
                         (the fixnum (min-factor (the fixnum (+ ,sj ,nj)) +unroll+))))
            (,kend (min ,nv (the fixnum (+ ,sk ,nk))))
            (,maxj 0))
       (declare (type fixnum ,iend ,jend0 ,kend ,maxj))
       (do ((,i 0 (1+ ,i)))
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
             ;; if jend < +unroll+ or (mod jend +unroll+) > 0
             (do ((,j ,maxj (1+ ,j)))
                 ((>= ,j ,ncb))
               (incf (row-major-aref ,mc ,imc)
                     (* ,maik (row-major-aref ,mb ,imb)))
               (incf ,imb)
               (incf ,imc))))))))



(defmacro m*m (val-type ma mb mc)
  (with-gensyms (calc copy nra nca nrb ncb nv nt m j k mnv mncb mb-sub)
    `(flet ((,calc (si ni sk nk nra nv ncb ma mb mc)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type fixnum si ni sk nk nra nv ncb)
                       (type (simple-array ,val-type (* *)) ma mb mc))
              (m*m-ker ,val-type si ni sk nk nra nv ncb ma mb mc))
            (,copy (ma si ni sj nj mb)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type (simple-array ,val-type (* *)) ma mb)
                       (type fixnum si ni sj nj))
              (copy-matrix ma si ni sj nj mb)))
       (declare (inline ,calc ,copy))
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
         (dotimes-interval (,k ,m ,nv)
           (dotimes-interval (,j ,m ,ncb)
             (let* ((,mnv (min ,m ,nv))
                    (,mncb (min ,m ,ncb))
                    (,mb-sub (make-array (list ,mnv ,mncb)
                                         :element-type ',val-type)))
               (declare (type fixnum ,mnv ,mncb)
                        (type (simple-array ,val-type (* *)) ,mb-sub))
               (,copy ,mb ,j ,m ,k ,m ,mb-sub)
               (,calc ,j ,m ,k ,m ,nra ,nv ,ncb ,ma ,mb-sub ,mc))))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m))
(defun dm*m (ma mb mc)
  "Multiply matrix and matrix of double-float"
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) ma mb mc))
  (m*m double-float ma mb mc))


(declaim (ftype (function ((simple-array long-float (* *))
                           (simple-array long-float (* *))
                           (simple-array long-float (* *))))
                lm*m))
(defun lm*m (ma mb mc)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array long-float (* *)) ma mb mc))
  (m*m long-float ma mb mc))
