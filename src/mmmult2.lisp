(in-package :cl-user)
(defpackage cl3a.mmmult2
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dm*m :lm*m))
(in-package :cl3a.mmmult2)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +mc+ (the fixnum 1024))
  (defparameter +kc+ (the fixnum 32))
  (defparameter +mr+ (the fixnum 4))
  (defparameter +nr+ (the fixnum 512)))


(defmacro m*m-ker (val-type si sj mmi mmj mi mj mk ma-sub mb-sub mc)
  "kernel program of m*m"
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   si: 
  (with-gensyms (ssi ssj mmj0 ii jj kk rmb rmc maik)
    `(let ((,ssi (rem ,si ,mi))
           (,ssj (rem ,sj ,mj))
           (,mmj0 (min-factor ,mmj +unroll+)))
       (declare (type fixnum ,ssi ,ssj ,mmj0))
       (dotimes (,ii ,mmi)
         (dotimes (,kk ,mk)
           (let ((,maik (aref ,ma-sub (+ ,ii ,ssi) ,kk))
                 (,rmb (array-row-major-index ,mb-sub ,kk ,ssj))
                 (,rmc (array-row-major-index ,mc (+ ,ii ,si) ,sj)))
             (declare (type ,val-type ,maik)
                      (type fixnum ,rmb ,rmc))
             (do ((,jj 0 (+ ,jj +unroll+)))
                 ((>= ,jj ,mmj0) ,jj)
               ,@(loop :repeat +unroll+
                    :with form = `((incf (row-major-aref ,mc ,rmc)
                                         (* ,maik (row-major-aref ,mb-sub ,rmb)))
                                   (incf ,rmb)
                                   (incf ,rmc))
                    :append form))
             ;; if mmj < +unroll+ or (mod mmj +unroll+) > 0
             (do ((,jj ,mmj0 (1+ ,jj)))
                 ((>= ,jj ,mmj))
               (incf (row-major-aref ,mc ,rmc)
                     (* ,maik (row-major-aref ,mb-sub ,rmb)))
               (incf ,rmb)
               (incf ,rmc))))))))


(defmacro m*m-panel (calc si sj mi mj mk ma-sub mb-sub mc)
  (with-gensyms (i j mmi mmj)
    `(dotimes-interval2 (,i ,si ,mi) (,mmi +mr+)
       (dotimes-interval2 (,j ,sj ,mj) (,mmj +nr+)
         (funcall ,calc ,i ,j ,mmi ,mmj ,mi ,mj ,mk ,ma-sub ,mb-sub ,mc)))))


(defmacro m*m-block (val-type nr nv nc ma mb mc)
  "block-calculation for m*m"
  ;; Args
  ;;   val-type: element type of array (double-float or long-float)
  ;;   nr: number of rows of ma & mc
  ;;   nv: number of cols of ma & number of rows of mb
  ;;   nc: number of cols of mb & mc
  ;;   ma: input matrix with nr*nv
  ;;   mb: input matrix with nv*nc
  ;;   mc: output matrix with nr*nc
  (with-gensyms (calc nrc nvc ma-sub mb-sub mi mk i k)
    `(flet ((,calc (i j mmi mmj mi mj mk ma-sub mb-sub mc)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type fixnum i j mmi mmj mi mj mk)
                       (type (simple-array ,val-type (* *)) ma-sub mb-sub mc))
              (m*m-ker ,val-type i j mmi mmj mi mj mk ma-sub mb-sub mc)))
       (declare (inline ,calc))
       (let* ((,nrc (min ,nr (the fixnum +mc+)))  ; block size of row
              (,nvc (min ,nv (the fixnum +kc+)))  ; block size of col
              (,ma-sub (make-array (list ,nrc ,nvc)
                                   :element-type ',val-type))  ; sub-matrix of ma with nrc*nvc
              (,mb-sub (make-array (list ,nvc ,nc)
                                   :element-type ',val-type)))  ; sub-matrix of mb with nvc*nc
              ;; (,mb-sub (make-array (list ,nc ,nvc)
              ;;                      :element-type ',val-type)))
         (declare (type fixnum ,nrc ,nvc)
                  (type (simple-array ,val-type (* *)) ,ma-sub ,mb-sub))
         (dotimes-interval2 (,k 0 ,nv) (,mk ,nvc)
           ;; (copy-matrix-transpose ,mb ,k ,mk 0 ,nc ,mb-sub)
           (copy-matrix ,mb ,k ,mk 0 ,nc ,mb-sub)
           (dotimes-interval2 (,i 0 ,nr) (,mi ,nrc)
             (copy-matrix ,ma ,i ,mi ,k ,mk ,ma-sub)
             (m*m-panel #',calc ,i 0 ,mi ,nc ,mk ,ma-sub ,mb-sub ,mc)))))))


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
       (m*m-block ,val-type ,nr ,nv ,nc ,ma ,mb ,mc))))


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
