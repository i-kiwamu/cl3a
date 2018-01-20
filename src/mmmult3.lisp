(in-package :cl-user)
(defpackage cl3a.mmmult3
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dm*m :lm*m :tile-size))
(in-package :cl3a.mmmult3)


(declaim (ftype (function (symbol fixnum fixnum) fixnum) tile-size))
(defun tile-size (val-type n cache-size)
  "calculate the optimum tile size for matrix multiplication"
  ;; Tile size should satisfy: (type-byte)*3*(tile-size)^2 < cache-size
  ;; For safety, 1 type-byte is saved.
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   n: the matrix size (min nrow ncol)
  ;;   cache-size: size of cache (+cache-line+, +L1-size+, +L2-size+, or +L3-size+)
  (declare (type fixnum n cache-size)
           (type symbol val-type))
  (let ((bs (isqrt
             (ifloor cache-size
                     (the fixnum 3)
                     (type-byte-length val-type)))))
    (declare (type fixnum bs))
    (when (<= bs (the fixnum 1))
      (setf bs (ifloor cache-size (type-byte-length val-type))))
    (min bs n)))


(declaim (ftype (function (fixnum fixnum fixnum) fixnum) row-major-idx))
(defun row-major-idx (i j nr)
  "calculate the row-major index: (aref a i j) = (row-major-aref a (row-major-idx i j nr))"
  ;; Args
  ;;   i: row index
  ;;   j: column index
  ;;   nr: number of row
  (declare (type fixnum i j nr))
  (+ (* i nr) j))


(defmacro m*m-ker (val-type ib0 nrb0 nr kb0 nvb0 nv jb0 ncb0 nc ma mb mc)
  "kernel program of m*m"
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   ib0: block index for nr
  ;;   nrb0: block size for nr
  ;;   nr: number of rows of ma and mc
  ;;   kb0: block index for nv
  ;;   nvb0: block size for nv
  ;;   nv: number of cols of ma & number of rows of mb
  ;;   jb0: block index for nc
  ;;   ncb0: block size for nc
  ;;   nc: number of cols of mb and mc
  ;;   ma: input matrix with nr*nv
  ;;   mb: input matrix with nv*nc
  ;;   mc: output matrix with nr*nc
  (with-gensyms (j0 i j k maik rma rmb rmc)
    `(let ((,j0 (max ,jb0 (the fixnum (min-factor ,ncb0 +unroll+)))))
       (declare (type fixnum ,j0))
       (do ((,i ,ib0 (1+ ,i)))
           ((>= ,i (+ ,ib0 ,nrb0)))
         (declare (type fixnum ,i))
         (do ((,k ,kb0 (1+ ,k)))
             ((>= ,k (+ ,kb0 ,nvb0)))
           (declare (type fixnum ,k))
           (let* ((,rma (row-major-idx ,i ,k ,nr))
                  (,maik (row-major-aref ,ma ,rma)))
             (declare (type fixnum ,rma)
                      (type ,val-type ,maik))
             (do ((,j ,jb0 (the fixnum (+ ,j +unroll+))))
                 ((>= ,j ,j0))
               (declare (type fixnum ,j))
               (let ((,rmc (row-major-idx ,i ,j ,nr))
                     (,rmb (row-major-idx ,k ,j ,nv)))
                 (declare (type fixnum ,rmc ,rmb))
                 ,@(loop :repeat +unroll+
                      :with form = `((incf (row-major-aref ,mc ,rmc)
                                           (* ,maik
                                              (row-major-aref ,mb ,rmb)))
                                   ; (format t "unroll (i j k) = (~A ~A ~A)~%" ,i ,j ,k)
                                     (incf ,rmc)
                                     (incf ,rmb))
                    :append form)))
             ;; if ncb0 < +unroll+ or (mod ncb0 +unroll+) > 0, do remain
             (do ((,j ,j0 (1+ ,j)))
                 ((>= ,j ,nc))
               (declare (type fixnum ,j))
               (let ((,rmc (row-major-idx ,i ,j ,nr))
                     (,rmb (row-major-idx ,k ,j ,nv)))
                 (incf (row-major-aref ,mc ,rmc)
                       (* ,maik (row-major-aref ,mb ,rmb))))
               ; (format t "remain (i j k) = (~A ~A ~A)~%" ,i ,j ,k)
               )))))))


(defmacro m*m-tile-cache-line (val-type ib1 nrb1 nr kb1 nvb1 nv jb1 ncb1 nc ma mb mc)
  "m*m in a tile of cache line size"
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   ib1: block index for nr
  ;;   nrb1: block size for nr
  ;;   nr: number of rows of ma & mc
  ;;   kb1: block index for nv
  ;;   nvb1: block size for nv
  ;;   nv: number of cols of ma & number of rows of mb
  ;;   jb1: block index for nc
  ;;   ncb1: block size for nc
  ;;   nc: number of cols of mb & mc
  ;;   ma: input matrix with nr*nv
  ;;   mb: input matrix with nv*nc
  ;;   mc: output matrix with nr*nc
  (with-gensyms (calc nrb nvb ncb ib0 jb0 kb0 nrb0 nvb0 ncb0)
    `(flet ((,calc (ib nrb nr kb nvb nv jb ncb nc ma mb mc)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type fixnum ib nrb nr kb nvb nv jb ncb nc)
                       (type (simple-array ,val-type (* *)) ma mb mc))
              (m*m-ker ,val-type ib nrb nr kb nvb nv jb ncb nc ma mb mc)))
       (declare (inline ,calc))
       (let* ((,nrb (tile-size ',val-type ,nrb1 +L1-size+))
              (,nvb (tile-size ',val-type ,nvb1 +L1-size+))
              (,ncb (tile-size ',val-type ,ncb1 +L1-size+)))
         (declare (type fixnum ,nrb ,nvb ,ncb))
         (dotimes-interval2 (,ib0 ,ib1 ,nrb1) (,nrb0 ,nrb)
           (dotimes-interval2 (,kb0 ,kb1 ,nvb1) (,nvb0 ,nvb)
             (dotimes-interval2 (,jb0 ,jb1 ,ncb1) (,ncb0 ,ncb)
               ; (format t "(ib0 nrb0 kb0 nvb0 jb0 ncb0) = (~A ~A ~A ~A ~A ~A)~%" ,ib0 ,nrb0 ,kb0 ,nvb0 ,jb0 ,ncb0)
               (,calc ,ib0 ,nrb0 ,nr ,kb0 ,nvb0 ,nv ,jb0 ,ncb0 ,nc ,ma ,mb ,mc))))))))


(defmacro m*m-tile-L1 (val-type ib2 nrb2 nr kb2 nvb2 nv jb2 ncb2 nc ma mb mc)
  "m*m in a tile of L1 cache size"
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   ib2: block index for nr
  ;;   nrb2: block size for nr
  ;;   nr: number of rows of ma & mc
  ;;   kb2: block index for nv
  ;;   nvb2: block size for nv
  ;;   nv: number of cols of ma & number of rows of mb
  ;;   jb2: block index for nc
  ;;   ncb2: block size for nc
  ;;   nc: number of cols of mb & mc
  ;;   ma: input matrix with nr*nv
  ;;   mb: input matrix with nv*nc
  ;;   mc: output matrix with nr*nc
  (with-gensyms (nrb nvb ncb ib1 jb1 kb1 nrb1 nvb1 ncb1)
    `(let* ((,nrb (tile-size ',val-type ,nrb2 +L1-size+))
            (,nvb (tile-size ',val-type ,nvb2 +L1-size+))
            (,ncb (tile-size ',val-type ,ncb2 +L1-size+)))
       (declare (type fixnum ,nrb ,nvb ,ncb))
       (dotimes-interval2 (,ib1 ,ib2 ,nrb2) (,nrb1 ,nrb)
         (dotimes-interval2 (,kb1 ,kb2 ,nvb2) (,nvb1 ,nvb)
           (dotimes-interval2 (,jb1 ,jb2 ,ncb2) (,ncb1 ,ncb)
             ; (format t "(ib1 nrb1 kb1 nvb1 jb1 ncb1) = (~A ~A ~A ~A ~A ~A)~%" ,ib1 ,nrb1 ,kb1 ,nvb1 ,jb1 ,ncb1)
             (m*m-tile-cache-line ,val-type ,ib1 ,nrb1 ,nr ,kb1 ,nvb1 ,nv ,jb1 ,ncb1 ,nc ,ma ,mb ,mc)))))))


(defmacro m*m-tile-L2 (val-type nr nv nc ma mb mc)
  "m*m in a tile of L2 cache size"
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   nr: number of rows of ma & mc
  ;;   nv: number of cols of ma & number of rows of mb
  ;;   nc: number of cols of mb & mc
  ;;   ma: input matrix with nr*nv
  ;;   mb: input matrix with nv*nc
  ;;   mc: output matrix with nr*nc
  (with-gensyms (calc nrb nvb ncb ib2 jb2 kb2 nrb2 nvb2 ncb2)
    `(flet ((,calc (ib nrb nr kb nvb nv jb ncb nc ma mb mc)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type fixnum ib nrb nr kb nvb nv jb ncb nc)
                       (type (simple-array ,val-type (* *)) ma mb mc))
              (m*m-tile-cache-line ,val-type ib nrb nr kb nvb nv jb ncb nc ma mb mc)))
       (declare (inline ,calc))
       (let* ((,nrb (tile-size ',val-type ,nr +L2-size+))
              (,nvb (tile-size ',val-type ,nv +L2-size+))
              (,ncb (tile-size ',val-type ,nc +L2-size+)))
         (declare (type fixnum ,nrb ,nvb ,ncb))
         (dotimes-interval2 (,jb2 0 ,nc) (,ncb2 ,ncb)
           (dotimes-interval2 (,ib2 0 ,nr) (,nrb2 ,nrb)
             (dotimes-interval2 (,kb2 0 ,nv) (,nvb2 ,nvb)
               ; (format t "(ib2 nrb2 kb2 nvb2 jb2 ncb2) = (~A ~A ~A ~A ~A ~A)~%" ,ib2 ,nrb2 ,kb2 ,nvb2 ,jb2 ,ncb2)
               (,calc ,ib2 ,nrb2 ,nr ,kb2 ,nvb2 ,nv ,jb2 ,ncb2 ,nc ,ma ,mb ,mc))))))))
               ; (m*m-ker ,val-type ,ib2 ,nrb2 ,nr ,kb2 ,nvb2 ,nv ,jb2 ,ncb2 ,nc ,ma ,mb ,mc))))))))


(defmacro m*m (val-type ma mb mc)
  "main macro for m*m"
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   ma: input matrix with nr*nv
  ;;   mb: input matrix with nv*nc
  ;;   mc: output matrix with nr*nc
  (with-gensyms (nr nv nc)
    `(let ((,nr (min (array-dimension ma 0)
                     (array-dimension mc 0)))
           (,nv (min (array-dimension ma 1)
                     (array-dimension mb 0)))
           (,nc (min (array-dimension mb 1)
                     (array-dimension mc 1))))
       (declare (type fixnum ,nr ,nv ,nc))
       (m*m-tile-L2 ,val-type ,nr ,nv ,nc ,ma ,mb ,mc))))


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
  "Multiply matrix and matrix of long-float"
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array long-float (* *)) ma mb mc))
  (m*m long-float ma mb mc))
