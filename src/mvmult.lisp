(in-package :cl-user)
(defpackage cl3a.mvmult
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dm*v :lm*v))
(in-package :cl3a.mvmult)


(defmacro m*v-ker (val-type si ni nr nc ma vb vc)
  "Multiply matrix and vector"
  (with-gensyms (iend jend0 i j ima ima1 ivb ivb1 vci)
    `(let* ((,iend (min ,nr (the fixnum (+ ,si ,ni))))
            (,jend0 (min-factor ,nc 2)))  ;; unrolling = 2
       (declare (type fixnum ,iend ,jend0))
       (do ((,i ,si (1+ ,i)))
           ((>= ,i ,iend))
         (let ((,ima (array-row-major-index ,ma ,i 0))
               (,ima1 (array-row-major-index ,ma ,i 1))
               (,ivb (array-row-major-index ,vb 0))
               (,ivb1 (array-row-major-index ,vb 1))
               (,vci (coerce 0.0 ',val-type)))
           (declare (type fixnum ,ima ,ima1 ,ivb ,ivb1)
                    (type ,val-type ,vci))
           (do ((,j 0 (the fixnum (+ ,j 2))))
               ((>= (the fixnum ,j) ,jend0))
             (incf ,vci
                   (+ (* (row-major-aref ,ma ,ima)
                         (row-major-aref ,vb ,ivb))
                      (* (row-major-aref ,ma ,ima1)
                         (row-major-aref ,vb ,ivb1))))
             (incf ,ima 2)
             (incf ,ima1 2)
             (incf ,ivb 2)
             (incf ,ivb1 2))
           ;; if nc < 2 or nc is odd number
           (when (> ,nc ,jend0)
             (incf ,vci
                   (* (row-major-aref ,ma ,ima)
                      (row-major-aref ,vb ,ivb))))
           (incf (aref ,vc ,i) ,vci))))))


(defmacro m*v (val-type ma vb vc)
  (with-gensyms (calc nra nca nb nj m0 m i)
    `(flet ((,calc (si ni nr nc ma vb vc)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type fixnum si ni nr nc)
                       (type (simple-array ,val-type (* *)) ma)
                       (type (simple-array ,val-type (*)) vb vc))
              (m*v-ker ,val-type si ni nr nc ma vb vc)))
       (declare (inline ,calc))
       (let* ((,nra (array-dimension ,ma 0))
              (,nca (array-dimension ,ma 1))
              (,nb (length ,vb))
              (,nj (cond ((/= ,nca ,nb) (different-length-warn ,nca ,nb)
                                        (min ,nca ,nb))
                         (t ,nca)))
              (,m0 (block-size (isqrt ,nra)))
              (,m (* ,m0 ,m0)))
         (declare (type fixnum ,nra ,nca ,nb ,nj ,m0 ,m))
         (cond
           ((= ,m ,nra)
            (,calc 0 ,nra ,nra ,nj ,ma ,vb ,vc))
           (t
            (dotimes-interval (,i ,m ,nra)
              (,calc ,i ,m ,nra ,nj ,ma ,vb ,vc))))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (*))
                           (simple-array double-float (*))))
                dm*v))
(defun dm*v (ma vb vc)
  "Multiply matrix and vector of double-float"
  (declare (type (simple-array double-float (* *)) ma)
           (type (simple-array double-float (*)) vb vc))
  (m*v double-float ma vb vc))


(declaim (ftype (function ((simple-array long-float (* *))
                           (simple-array long-float (*))
                           (simple-array long-float (*))))
                lm*v))
(defun lm*v (ma vb vc)
  "Multiply matrix and vector of long-float"
  (declare (type (simple-array long-float (* *)) ma)
           (type (simple-array long-float (*)) vb vc))
  (m*v long-float ma vb vc))
