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


(defmacro m*v-ker (val-type si ni sj nj nr nc ma vb)
  "Multiply matrix and vector"
  (with-gensyms (nw nv iend jend jend0 res i isi j j1 j2 j3 j4 maxj)
    `(let* ((,nv (min ,nr ,ni))
            (,nw (min ,nc ,nj))
            (,iend (min ,nr (the fixnum (+ ,si ,ni))))
            (,jend (min ,nc (the fixnum (+ ,sj ,nj))))
            (,jend0 (min-factor ,jend 5))
            (,res (make-array ,nv :element-type ',val-type)))
       (declare (type fixnum ,nv ,nw ,iend ,jend ,jend0)
                (type (simple-array ,val-type (*)) ,res))
       (cond
         ((< ,nw 5)
          (do ((,i ,si (1+ ,i)))
              ((>= ,i ,iend))
            (let ((,isi (- (the fixnum ,i) ,si)))
              (declare (type fixnum ,isi))
              (do ((,j ,sj (1+ ,j)))
                  ((>= ,j ,jend))
                (incf (aref ,res ,isi)
                      (* (aref ,ma ,i ,j) (aref ,vb ,j)))))))
         (t (do ((,i ,si (1+ ,i)))
                ((>= ,i ,iend) ,i)
              (let* ((,isi (- (the fixnum ,i) ,si))
                     (,maxj
                      (do ((,j ,sj (+ ,j 5))
                           (,j1 (the fixnum (+ ,sj 1)) (the fixnum (+ ,j1 5)))
                           (,j2 (the fixnum (+ ,sj 2)) (the fixnum (+ ,j2 5)))
                           (,j3 (the fixnum (+ ,sj 3)) (the fixnum (+ ,j3 5)))
                           (,j4 (the fixnum (+ ,sj 4)) (the fixnum (+ ,j4 5))))
                          ((>= ,j ,jend0) ,j)
                        (incf (aref ,res ,isi)
                              (+ (* (aref ,ma ,i ,j) (aref ,vb ,j))
                                 (* (aref ,ma ,i ,j1) (aref ,vb ,j1))
                                 (* (aref ,ma ,i ,j2) (aref ,vb ,j2))
                                 (* (aref ,ma ,i ,j3) (aref ,vb ,j3))
                                 (* (aref ,ma ,i ,j4) (aref ,vb ,j4)))))))
                (declare (type fixnum ,isi ,maxj))
                (when (< ,maxj ,jend)
                  (do ((,j ,maxj (1+ ,j)))
                      ((>= ,j ,jend))
                    (incf (aref ,res ,isi)
                          (* (aref ,ma ,i ,j) (aref ,vb ,j)))))))))
       ,res)))


(declaim (inline dm*v-ker)
         (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (*)))
                          (simple-array double-float (*)))
                dm*v-ker))
(defun dm*v-ker (si ni sj nj nr nc ma vb)
  "Multiply matrix and vector of double-float"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum si ni sj nj nr nc)
           (type (simple-array double-float (* *)) ma)
           (type (simple-array double-float (*)) vb))
  (m*v-ker double-float si ni sj nj nr nc ma vb))


(declaim (inline lm*v-ker)
         (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum
                           (simple-array long-float (* *))
                           (simple-array long-float (*)))
                          (simple-array long-float (*)))
                lm*v-ker))
(defun lm*v-ker (si ni sj nj nr nc ma vb)
  "Multiply matrix and vector of long-float"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum si ni sj nj nr nc)
           (type (simple-array long-float (* *)) ma)
           (type (simple-array long-float (*)) vb))
  (m*v-ker long-float si ni sj nj nr nc ma vb))


(defmacro m*v (val-type fun ma vb)
  "Multiply matrix and vector within L1 cache size"
  (with-gensyms (nr nc nb nvec tbl mj mi k vc res i ii ir nk)
    `(let* ((,nr (array-dimension ,ma 0))
            (,nc (array-dimension ,ma 1))
            (,nb (length ,vb))
            (,nvec (cond ((/= ,nc ,nb)
                          (different-length-warn ,nc ,nb)
                          (min ,nc ,nb))
                         (t ,nc)))
            (,tbl (type-byte-length ',val-type))
            (,mj (ifloor +L1-size+ ,tbl))
            (,mi (if (> ,nvec ,mj) 1 (ifloor +L1-size+ ,nvec ,tbl)))
            (,k (min-factor ,nr ,mi))
            (,vc (make-array ,nr :element-type ',val-type)))
       (declare (type fixnum ,nr ,nc ,nb ,nvec ,tbl ,mj ,mi ,k)
                (type (simple-array ,val-type (*)) ,vc))
       (do ((,i 0 (the fixnum (+ ,i ,mi))))
           ((>= (the fixnum ,i) ,k))
         (let ((,res (funcall ,fun ,i ,mi 0 ,nvec ,nr ,nc ,ma ,vb)))
           (declare (type (simple-array ,val-type (*)) ,res))
           (dotimes (,ir ,mi)
             (let ((,ii (+ ,ir ,i)))
               (declare (type fixnum ,ii))
               (setf (aref ,vc ,ii) (aref ,res ,ir))))))
       (when (> ,nvec ,k)
         (let* ((,nk (- ,nvec ,k))
                (,res (funcall ,fun ,k ,nk 0 ,nvec ,nr ,nc ,ma ,vb)))
           (declare (type fixnum ,nk)
                    (type (simple-array ,val-type (*)) ,res))
           (do ((,i ,k (1+ ,i))
                (,ir 0 (1+ ,ir)))
               ((>= ,i ,nvec))
             (setf (aref ,vc ,i) (aref ,res ,ir)))))
       ,vc)))


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
