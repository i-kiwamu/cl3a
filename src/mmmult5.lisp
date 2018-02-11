(in-package :cl-user)
(defpackage cl3a.mmmult5
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dm*m :lm*m))
(in-package :cl3a.mmmult5)


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


(defmacro m*m-ker (val-type si ni sj nj sk nk nra nv ncb ma mb mc)
  "Multiply matrix and matrix (unrolling version)"
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   si: block index for nr
  ;;   ni: block size for nr
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
  (with-gensyms (iend jend kend0 i j k mcij ima imb)
    `(let* ((,iend (min ,nra (the fixnum (+ ,si ,ni))))
            (,jend (min ,ncb (the fixnum (+ ,sj ,nj))))
            (,kend0 (min ,nv
                         (the fixnum (min-factor
                                      (the fixnum (+ ,sk ,nk))
                                      +unroll+)))))
       (declare (type fixnum ,iend ,jend ,kend0))
       ; (format t "(iend jend kend0) = (~A ~A ~A)~%" ,iend ,jend ,kend0)
       (do ((,i ,si (1+ ,i)))
           ((>= ,i ,iend))
         (do ((,j ,sj (1+ ,j)))
             ((>= ,j ,jend))
           (let ((,mcij (aref ,mc ,i ,j))
                 (,ima (array-row-major-index ,ma ,i ,sk))
                 (,imb (array-row-major-index ,mb ,sk ,j)))
             (declare (type ,val-type ,mcij)
                      (type fixnum ,ima ,imb))
             (do ((,k ,sk (+ ,k +unroll+)))
                 ((>= ,k ,kend0))
               ,@(loop :repeat +unroll+
                    :with form =
                    `((incf ,mcij
                            (* (row-major-aref ,ma ,ima)
                               (row-major-aref ,mb ,imb)))
                      (incf ,ima)
                      (incf ,imb)
                      ; (format t "unroll (i j k) = (~A ~A ~A)~%" ,i ,j ,k))
                      )
                    :append form))
             ;; if ncb0 < +unroll+ or (mod ncb0 +unroll+) > 0, do remain
             (do ((,k ,kend0 (1+ ,k)))
                 ((>= ,k ,nv))
               (incf ,mcij
                     (* (row-major-aref ,ma ,ima)
                        (row-major-aref ,mb ,imb)))
               (incf ,ima)
               (incf ,imb))
             (setf (aref ,mc ,i ,j) ,mcij)))))))


(defmacro m*m (val-type ma mb mc)
  (with-gensyms (calc copy nra nca nrb ncb nv nt m i j k mnra mncb ma-sub)
    `(flet ((,calc (si ni sj nj sk nk nra nv ncb ma mb mc)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type fixnum si ni sj nj sk nk nra nv ncb)
                       (type (simple-array ,val-type (* *)) ma mb mc))
              (m*m-ker ,val-type si ni sj nj sk nk nra nv ncb ma mb mc))
            (,copy (ma si ni sj nj mb)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type (simple-array ,val-type (* *)) ma mb)
                       (type fixnum si ni sj nj))
              (copy-matrix ma si ni sj nj mb)))
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
         (dotimes-interval (,j ,m ,ncb)
           (dotimes-interval (,i ,m ,nra)
             (let* ((,mncb (min ,m ,ncb))
                    (,mnra (min ,m ,nra))
                    (,ma-sub (make-array (list ,mnra ,mncb)
                                         :element-type ',val-type)))
               (declare (type fixnum ,mncb ,mnra)
                        (type (simple-array ,val-type (* *)) ,ma-sub))
               (,copy ,ma ,i ,m ,j ,m ,ma-sub)
               (dotimes-interval (,k ,m ,nv)
                 ; (format t "(si sj sk m) = (~A ~A ~A ~A)~%" ,i ,j ,k ,m)
                 (,calc ,i ,m ,j ,m ,k ,m ,nra ,nv ,ncb ,ma-sub ,mb ,mc)))))))))


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
