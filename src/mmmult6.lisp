(in-package :cl-user)
(defpackage cl3a.mmmult6
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dm*m :tile-size))
(in-package :cl3a.mmmult6)


(declaim (ftype (function (integer &key (:l1 boolean)) integer)
                tile-size))
(defun tile-size (n &key l1)
  "See Lam et al. 1991 The cache performance and optimizations of blocked algorithms"
  (declare (type integer n)
           (type boolean l1))
  (let* ((n-half (ifloor n 2))
         (cache-size (if l1
                         (ifloor (* +L1-size+ +associativity+) 8)
                         (ifloor (* +L2-size+ +associativity+) 8))))  ;; 1 word = 4 byte
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
  (with-gensyms (iend jend0 kend i j k ii kk maik imb imc)
    `(let* ((,iend (min (the fixnum (- ,nra ,si)) ,ni))
            (,jend0 (min-factor ,ncb +unroll+))
            (,kend (min (the fixnum (- ,nv ,sk)) ,nk)))
       (declare (type fixnum ,iend ,jend0 ,kend))
       (dotimes (,i ,iend)
         (dotimes (,k ,kend)
           (let* ((,maik (aref ,ma 0 0))
                  (,ii (+ ,i ,si))  ; row i in ma-sub = row ii in ma
                  (,kk (+ ,k ,sk))  ; col k in ma-sub = col kk in ma
                  (,imb (array-row-major-index ,mb ,kk 0))
                  (,imc (array-row-major-index ,mc ,ii 0)))
             (declare (type ,val-type ,maik)
                      (type fixnum ,ii ,kk ,imb ,imc))
             (do ((,j 0 (+ ,j +unroll+)))
                 ((>= ,j ,jend0) ,j)
               ,@(loop :repeat +unroll+
                    :with form =
                    `((incf (row-major-aref ,mc ,imc)
                            (* ,maik (row-major-aref ,mb ,imb)))
                      (incf ,imb)
                      (incf ,imc))
                    :append form))
             ;; if jend < +unroll+ or (mod jend +unroll+) > 0
             (do ((,j ,jend0 (1+ ,j)))
                 ((>= ,j ,ncb))
               (incf (row-major-aref ,mc ,imc)
                     (* ,maik (row-major-aref ,mb ,imb)))
               (incf ,imb)
               (incf ,imc))))))))



(defmacro m*m (val-type ma mb mc)
  (with-gensyms (calc copy nra nca nrb ncb nv m1 m2 i k ma-sub)
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
              (,m1 (tile-size ,nra))
              (,m2 (tile-size ,ncb :l1 t)))
         (declare (type fixnum ,nra ,nca ,nrb ,ncb ,nv ,m1 ,m2))
         (dotimes-interval (,i ,m1 ,nra)
           (dotimes-interval (,k ,m2 ,nv)
             (let ((,ma-sub (make-array (list ,m1 ,m2)
                                        :element-type ',val-type)))
               (declare (type (simple-array ,val-type (* *)) ,ma-sub))
               (,copy ,ma ,i ,m1 ,k ,m2 ,ma-sub)
               (,calc 0 ,m1 0 ,m2 ,nra ,nv ,ncb ,ma-sub ,mb ,mc))))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m))
(defun dm*m (ma mb mc)
  "Multiply matrix and matrix of double-float"
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) ma mb mc))
  (m*m double-float ma mb mc))
