(in-package :cl-user)
(defpackage cl3a.mmmult7
  (:use :cl :sb-ext :sb-c :alexandria :cl3a.utilities :cl3a.mmmult7vop)
  (:export :dm*m :gemm1 :gemm3))
(in-package :cl3a.mmmult7)


(declaim (ftype (function ((simple-array double-float (*))
                           fixnum)
                          (simd-pack double-float))
                load2-sse-from-array)
         (inline load2-sse-from-array))
(defun load2-sse-from-array (matv i)
  (declare (type (simple-array double-float (*)) matv)
           (type fixnum i))
  (let ((x1 (row-major-aref matv i))
        (x2 (row-major-aref matv (1+ i))))
    (declare (type double-float x1 x2))
    (sb-kernel:%make-simd-pack-double x1 x2)))


(declaim (ftype (function ((simple-array double-float (*))
                           fixnum
                           (simd-pack double-float)))
                store2-sse-to-array)
         (inline store2-sse-to-array))
(defun store2-sse-to-array (matv i simd)
  (declare (type (simple-array double-float (*)) matv)
           (type fixnum i)
           (type (simd-pack double-float) simd))
  (multiple-value-bind (x1 x2)
      (sb-kernel:%simd-pack-doubles simd)
    (setf (row-major-aref matv i) x1)
    (incf i)
    (setf (row-major-aref matv i) x2)))


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
  (with-gensyms (iend jend0 kend i j k maik maiksse imb imc)
    `(let* ((,iend (min ,nra (the fixnum (+ ,si ,ni))))
            (,jend0 (min-factor ,ncb +unroll+ 2))  ; 2 for sse
            (,kend (min ,nv (the fixnum (+ ,sk ,nk)))))
       (declare (type fixnum ,iend ,jend0 ,kend))
       (do ((,i ,si (1+ ,i)))
           ((>= ,i ,iend))
         (do ((,k ,sk (1+ ,k)))
             ((>= ,k ,kend))
           (let* ((,maik (aref ,ma ,i ,k))
                  (,maiksse (sb-kernel:%make-simd-pack-double ,maik ,maik))
                  (,imb (array-row-major-index ,mb ,k 0))
                  (,imc (array-row-major-index ,mc ,i 0)))
             (declare (type ,val-type ,maik)
                      (type (simd-pack double-float) ,maiksse)
                      (type fixnum ,imb ,imc))
             (do ((,j 0 (the fixnum (+ ,j (* +unroll+ 2)))))
                 ((>= ,j ,jend0))
               ,@(loop :repeat +unroll+
                    :with form =
                    `((store2-sse-to-array (sb-kernel:%array-data-vector ,mc) ,imc
                       (f2+ (load2-sse-from-array (sb-kernel:%array-data-vector ,mc) ,imc)
                            (f2* ,maiksse
                                 (load2-sse-from-array (sb-kernel:%array-data-vector ,mb) ,imb))))
                      (incf ,imb 2)
                      (incf ,imc 2))
                    :append form))
             ;; if jend < +unroll+ or (mod jend +unroll+) > 0
             (do ((,j ,jend0 (+ ,j 2)))
                 ((>= ,j ,ncb))
               (cond
                 ((> (+ ,j 2) ,ncb)  ; if j is odd and loop is the last
                  (incf (row-major-aref ,mc ,imc)
                        (* ,maik (row-major-aref ,mb ,imb)))
                  (incf ,imb)
                  (incf ,imc))
                 (t
                  (store2-sse-to-array (sb-kernel:%array-data-vector ,mc) ,imc
                   (f2+ (load2-sse-from-array (sb-kernel:%array-data-vector ,mc) ,imc)
                        (f2* ,maiksse
                             (load2-sse-from-array (sb-kernel:%array-data-vector ,mb) ,imb))))
                  (incf ,imb 2)
                  (incf ,imc 2))))))))))



(defmacro m*m (val-type ma mb mc)
  ;; (with-gensyms (calc nra nca nrb ncb nv nt m i k)
  (with-gensyms (calc nra nca nrb ncb nv m1 m2 i k)
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
              ;; (,nt (min ,nra ,ncb))
              ;; (,m (tile-size ,nt))
              (,m1 (tile-size ,nra))
              (,m2 (tile-size ,ncb :l1 t)))
         ;; (declare (type fixnum ,nra ,nca ,nrb ,ncb ,nv ,nt ,m))
         ;; (dotimes-interval (,i ,m ,nra)
         ;;   (dotimes-interval (,k ,m ,nv)
         ;;     (,calc ,i ,m ,k ,m ,nra ,nv ,ncb ,ma ,mb ,mc)))))))
         (declare (type fixnum ,nra ,nca ,nrb ,ncb ,nv ,m1 ,m2))
         (dotimes-interval (,i ,m1 ,nra)
           (dotimes-interval (,k ,m2 ,nv)
             (,calc ,i ,m1 ,k ,m2 ,nra ,nv ,ncb ,ma ,mb ,mc)))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m))
(defun dm*m (ma mb mc)
  "Multiply matrix and matrix of double-float"
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) ma mb mc))
  (m*m double-float ma mb mc))
