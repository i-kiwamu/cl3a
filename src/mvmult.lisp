(in-package :cl-user)
(defpackage cl3a.mvmult
  (:use :cl :alexandria :trivial-types)
  (:shadowing-import-from :trivial-types
                          :proper-list
                          :proper-list-p
                          :string-designator)
  (:import-from :cl3a.utilities
                :+L1-size+
                :different-length-warn
                :type-byte-length
                :ifloor
                :min-factor
                :dotimes-unroll)
  (:export :dm*v :lm*v))
(in-package :cl3a.mvmult)


(defmacro m*v-ker (val-type si ni sj nj nr nc ma vb vc)
  "Multiply matrix and vector"
  (with-gensyms (iend jend i j ma-index vci)
    `(let* ((,iend (min ,nr (the fixnum (+ ,si ,ni))))
            (,jend (min ,nc (the fixnum (+ ,sj ,nj)))))
       (declare (type fixnum ,iend ,jend))
       (do ((,i ,si (1+ ,i)))
           ((>= ,i ,iend))
         (let ((,ma-index (array-row-major-index ,ma ,i 0))
               (,vci (aref ,vc ,i)))
           (declare (type fixnum ,ma-index)
                    (type ,val-type ,vci))
           (do ((,j ,sj (1+ ,j)))
               ((>= ,j ,jend))
           ;; (dotimes-unroll (,j ,sj ,jend)  ;; slow in small nr & nc
             (incf ,vci (* (row-major-aref ,ma ,ma-index)
                           (row-major-aref ,vb ,j)))
             (incf ,ma-index))
           (setf (row-major-aref ,vc ,i) ,vci))))))



(declaim (inline dm*v-ker)
         (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (*))
                           (simple-array double-float (*))))
                dm*v-ker))
(defun dm*v-ker (si ni sj nj nr nc ma vb vc)
  "Multiply matrix and vector of double-float"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum si ni sj nj nr nc)
           (type (simple-array double-float (* *)) ma)
           (type (simple-array double-float (*)) vb vc))
  (m*v-ker double-float si ni sj nj nr nc ma vb vc))


(declaim (inline lm*v-ker)
         (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum
                           (simple-array long-float (* *))
                           (simple-array long-float (*))
                           (simple-array long-float (*))))
                lm*v-ker))
(defun lm*v-ker (si ni sj nj nr nc ma vb vc)
  "Multiply matrix and vector of long-float"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum si ni sj nj nr nc)
           (type (simple-array long-float (* *)) ma)
           (type (simple-array long-float (*)) vb vc))
  (m*v-ker long-float si ni sj nj nr nc ma vb vc))


(defmacro m*v (val-type fun ma vb)
  (with-gensyms (nr nc nb nvec vc)
    `(let* ((,nr (array-dimension ,ma 0))
            (,nc (array-dimension ,ma 1))
            (,nb (length ,vb))
            (,nvec (cond ((/= ,nc ,nb) (different-length-warn ,nc ,nb)
                                       (min ,nc ,nb))
                         (t ,nc)))
            (,vc (make-array ,nvec :element-type ',val-type)))
    (declare (type fixnum ,nr ,nc ,nb ,nvec)
             (type (simple-array ,val-type (*)) ,vc))
    (funcall ,fun 0 ,nr 0 ,nvec ,nr ,nc ,ma ,vb ,vc)
    ,vc)))


;; (defmacro m*v (val-type fun ma vb)
;;   "Multiply matrix and vector within L1 cache size"
;;   (with-gensyms (nr nc nb nvec tbl mj mi ki kj vc i j nk)
;;     `(let* ((,nr (array-dimension ,ma 0))
;;             (,nc (array-dimension ,ma 1))
;;             (,nb (length ,vb))
;;             (,nvec (cond ((/= ,nc ,nb) (different-length-warn ,nc ,nb)
;;                                        (min ,nc ,nb))
;;                          (t ,nc)))
;;             (,tbl (type-byte-length ',val-type))
;;             (,mj (ifloor +L1-size+ ,tbl))
;;             (,mi (ifloor +L1-size+ ,tbl ,nvec))
;;             (,kj (if (= ,mj 0) 1 (min-factor ,nvec ,mj)))
;;             (,ki (if (= ,mi 0) 1 (min-factor ,nr ,mi)))
;;             (,vc (make-array ,nr :element-type ',val-type)))
;;        (declare (type fixnum ,nr ,nc ,nb ,nvec ,mj ,mi ,kj ,ki)
;;                 (type (simple-array ,val-type (*)) ,vc))
;;        (do ((,i 0 (the fixnum (+ ,i ,mi))))
;;            ((>= (the fixnum ,i) ,ki))
;;          (do ((,j 0 (the fixnum (+ ,j ,mj))))
;;              ((>= (the fixnum ,j) ,kj))
;;            (funcall ,fun ,i ,mi ,j ,mj ,nr ,nc ,ma ,vb ,vc))
;;          (when (> ,nvec ,kj)
;;            (let ((,nk (- ,nvec ,kj)))
;;              (declare (type fixnum ,nk))
;;              (funcall ,fun ,i ,mi ,kj ,nk ,nr ,nc ,ma ,vb ,vc))))
;;        (when (> ,nr ,ki)
;;          (let ((,nk (- ,nr ,ki)))
;;            (declare (type fixnum ,nk))
;;            (funcall ,fun ,ki ,nk 0 ,nvec ,nr ,nc ,ma ,vb ,vc)))
;;        ,vc)))



(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (*)))
                          (simple-array double-float (*)))
                dm*v))
(defun dm*v (ma vb)
  "Multiply matrix and vector of double-float"
  (declare (optimize (speed 3))
           (type (simple-array double-float (* *)) ma)
           (type (simple-array double-float (*)) vb))
  (m*v double-float #'dm*v-ker ma vb))


(declaim (ftype (function ((simple-array long-float (* *))
                           (simple-array long-float (*)))
                          (simple-array long-float (*)))
                lm*v))
(defun lm*v (ma vb)
  "Multiply matrix and vector of long-float"
  (declare (optimize (speed 3))
           (type (simple-array long-float (* *)) ma)
           (type (simple-array long-float (*)) vb))
  (m*v long-float #'lm*v-ker ma vb))
