(in-package :cl-user)
(defpackage cl3a.mmmult3
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dm*m :lm*m))
(in-package :cl3a.mmmult3)


(defun block-size (val-type n cache-size)
  "calculate the optimum block size for matrix multiplication"
  ;; Block size should satisfy: (type-byte)*3*(block-size)^2 < cache-size
  ;; For safety, 1 type-byte is saved.
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   n: the matrix size (min nrow ncol)
  ;;   cache-size: size of cache (+cache-line+, +L1-size+, +L2-size+, or +L3-size+)
  (declare (type integer n cache-size))
  (let ((bs (isqrt
             (ifloor cache-size 3 (type-byte-length val-type)))))
  ;; (let ((bs (ifloor cache-size (type-byte-length val-type))))
    (min bs n)))


(defmacro m*m-ker (val-type ib nrb nr kb nvb nv jb ncb nc ma mb mc)
  "kernel program of m*m"
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   ib: block index for nr
  ;;   nrb: block size for nr
  ;;   nr: number of rows of ma and mc
  ;;   kb: block index for nv
  ;;   nvb: block size for nv
  ;;   nv: number of cols of ma & number of rows of mb
  ;;   jb: block index for nc
  ;;   ncb: block size for nc
  ;;   nc: number of cols of mb and mc
  ;;   ma: input matrix with nr*nv
  ;;   mb: input matrix with nv*nc
  ;;   mc: output matrix with nr*nc
  (with-gensyms (j0 i j k maik)
    `(let ((,j0 (min-factor ,ncb +unroll+)))
       (declare (type fixnum ,j0))
       (do ((,i ,ib (1+ ,i)))
           ((>= ,i (+ ,ib ,nrb)))
         (do ((,k ,kb (1+ ,k)))
             ((>= ,k (+ ,kb ,nvb)))
           (let ((,maik (row-major-aref ,ma (+ (* ,i ,nr) ,k))))
             (declare (type ,val-type ,maik))
             (do ((,j ,jb (+ ,j +unroll+)))
                 ((>= ,j ,j0) ,j)
               ,@(loop :repeat +unroll+
                    :with form = `((incf (row-major-aref
                                          ,mc (+ (* ,i ,nr) ,j))
                                         (* ,maik
                                            (row-major-aref
                                             ,mb (+ (* ,k ,nv) ,j))))
                                   (incf ,j))
                    :append form))
             ;; if ncb < +unroll+ or (mod ncb +unroll+) > 0, do remain
             (do ((,j ,j0 (1+ ,j)))
                 ((>= ,j ,nc))
               (incf (row-major-aref ,mc (+ (* ,i ,nr) ,j))
                     (* ,maik (row-major-aref ,mb (+ (* ,k ,nv) ,j)))))))))))


(defmacro m*m-tile-L1 (val-type nr nv nc ma mb mc)
  "m*m in a tile of cache line size"
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   nr: number of rows of ma & mc
  ;;   nv: number of cols of ma & number of rows of mb
  ;;   nc: number of cols of mb & mc
  ;;   ma: input matrix with nr*nv
  ;;   mb: input matrix with nv*nc
  ;;   mc: output matrix with nr*nc
  ;; (with-gensyms (nrb nvb ncb ma-sub mb-sub ib jb kb)
  (with-gensyms (nrb nvb ncb ib jb kb nb1 nb2 nb3)
    `(let* ((,nrb (the fixnum (block-size ',val-type ,nr +L1-size+)))
            (,nvb (the fixnum (block-size ',val-type ,nv +L1-size+)))
            (,ncb (the fixnum (block-size ',val-type ,nc +L1-size+))))
            ;; (,ma-sub (make-array (list ,nrb ,nvb)
            ;;                      :element-type ',val-type))
            ;; (,mb-sub (make-array (list ,nvb ,ncb)
            ;;                      :element-type ',val-type)))
       (declare (type fixnum ,nrb ,nvb ,ncb))
                ;; (type (simple-array ,val-type (* *)) ,ma-sub3 ,mb-sub3))
       (dotimes-interval2 (,ib 0 ,nr) (,nb1 ,nrb)
         (dotimes-interval2 (,kb 0 ,nv) (,nb2 ,nvb)
           ; (copy-matrix ,ma ,ib ,nrb ,kb ,nvb ,ma-sub)
           (dotimes-interval2 (,jb 0 ,nc) (,nb3 ,ncb)
              ; (copy-matrix ,mb ,kb ,nvb ,jb ,ncb ,mb-sub)
             (m*m-ker ,val-type ,ib ,nb1 ,nr ,kb ,nb2 ,nv ,jb ,nb3 ,nc ,ma ,mb ,mc)))))))


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
       (m*m-tile-L1 ,val-type ,nr ,nv ,nc ,ma ,mb ,mc))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m))
(defun dm*m (ma mb mc)
  "Multiply matrix and matrix of double-float"
  (declare (optimize (speed 3))
           (type (simple-array double-float (* *)) ma mb mc))
  (m*m double-float ma mb mc))


(declaim (ftype (function ((simple-array long-float (* *))
                           (simple-array long-float (* *))
                           (simple-array long-float (* *))))
                lm*m))
(defun lm*m (ma mb mc)
  "Multiply matrix and matrix of long-float"
  (declare (optimize (speed 3))
           (type (simple-array long-float (* *)) ma mb mc))
  (m*m long-float ma mb mc))
