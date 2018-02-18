(in-package :cl-user)
(defpackage cl3a.mmmult_Goto
  (:use :cl :alexandria :cl3a.utilities)
  (:export :gemm1 :gemm3 :dm*m))
(in-package :cl3a.mmmult_Goto)


(declaim (ftype (function (fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           fixnum
                           (simple-array double-float (* *))))
                gebp-reg)
         (inline gebp-reg))
(defun gebp-reg (mr t1 t2 t3 Atd Bp jr Caux)
  "kernel program of GEBP: Caux += Atd * Bp[ib]"
  ;; Args
  ;;   mr: block size of row of A & C
  ;;   t1: length of row of A = length of row of C
  ;;   t2: length of column of A = length of row of B
  ;;   t3: length of column of B = length of column of C
  ;;   Atd: left input matrix A-tilde
  ;;   Bp: right input matrix B-packed
  ;;   jr: target calculation of column at B & C
  ;;   Caux: output sub-matrix
  (declare (optimize (speed 3) (safety 0))
           (type fixnum mr t1 t2 t3 jr)
           (type (simple-array double-float (* *)) Atd Bp Caux))
  (dotimes-interval2 (ir 0 t1) (t4 mr)
    (dotimes (i t4)
      (dotimes (j t3)
        (let ((cij (aref Caux (+ ir i) j))
              (ia (array-row-major-index Atd (+ ir i) 0))
              (ib (array-row-major-index Bp (+ jr j) 0)))
          (declare (type double-float cij)
                   (type fixnum ia ib))
          (dotimes-unroll2 (k 0 t2)
            (incf cij (* (row-major-aref Atd ia)
                         (row-major-aref Bp ib)))
            (incf ia)
            (incf ib))
          (incf (aref Caux (+ ir i) j) cij))))))


(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum
                           fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                gepp-blk1))
(defun gepp-blk1 (p mc t2 nr mr m n A Bp C)
  "Blocked GEPP"
  ;; Args
  ;;   p: index
  ;;   mc: nrow of Atd
  ;;   t2: kc
  ;;   nr: ncol of submatrix of Btilde
  ;;   mr: nrow of submatrix of Atilde
  ;;   m: nrow of A & C
  ;;   n: ncol of B & C
  ;;   A: left input matrix
  ;;   Bp: right input matrix of n*kc (transposed)
  ;;   C: output matrix
  (declare (optimize (speed 3) (safety 0))
           (type fixnum p mc t2 nr mr m n)
           (type (simple-array double-float (* *)) A Bp C))
  (dotimes-interval2 (ic 0 m) (t1 mc)
     (let ((Atd (make-array (list t1 t2)
                            :element-type 'double-float)))
       (declare (type (simple-array double-float (* *)) Atd))
       (copy-matrix/double-float A ic t1 p t2 Atd)
       (dotimes-interval2 (jr 0 n) (t3 nr)
         (let ((Caux (make-array (list t1 t3)
                                 :element-type 'double-float)))
           (declare (type (simple-array double-float (* *)) Caux))
           (gebp-reg mr t1 t2 t3 Atd Bp jr Caux)
           (dotimes (ii t1)
             (let ((rmc (array-row-major-index C (+ ic ii) jr))
                   (rmcaux (array-row-major-index Caux ii 0)))
               (declare (type fixnum rmc rmcaux))
               (dotimes-unroll2 (jj 0 t3)
                 (incf (row-major-aref C rmc)
                       (row-major-aref Caux rmcaux))
                 (incf rmc)
                 (incf rmcaux)))))))))


(declaim (ftype (function (fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                gemm1))
(defun gemm1 (mc kc nr mr A B C)
  "GEMM_VAR1"
  (declare (optimize (speed 3) (safety 0))
           (type fixnum mc kc nr mr)
           (type (simple-array double-float (* *)) A B C))
  (let* ((m (min (array-dimension A 0)
                 (array-dimension C 0)))
         (k (min (array-dimension A 1)
                 (array-dimension B 0)))
         (n (min (array-dimension B 1)
                 (array-dimension C 1)))
         (Bp (make-array (list n (min kc k))
                         :element-type 'double-float)))
    (declare (type fixnum m k n)
             (type (simple-array double-float (* *)) Bp))
    (dotimes-interval2 (p 0 k) (t2 kc)
      (copy-matrix-transpose/double-float B p t2 0 n Bp)
      (gepp-blk1 p mc t2 nr mr m n A Bp C))))


(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           fixnum
                           (simple-array double-float (* *))))
                gebp-reg2)
         (inline gebp-reg2))
(defun gebp-reg2 (i mr t1 t2 t3 Atd Baux jr C)
  "kernel program of GEBP: Ci[ic] += Atd * Baux"
  ;; Args
  ;;   i: row index
  ;;   mr: block size of row of A & C
  ;;   t1: length of row of A = length of row of C
  ;;   t2: length of column of A = length of row of B
  ;;   t3: length of column of B = length of column of C
  ;;   Atd: left input matrix A-tilde
  ;;   Baux: right input sub-matrix B
  ;;   jr: target calculation of column at B & C
  ;;   C: output packed-matrix
  (declare (optimize (speed 3) (safety 0))
           (type fixnum i mr t1 t2 t3 jr)
           (type (simple-array double-float (* *)) Atd Baux C))
  (dotimes-interval2 (ir 0 t1) (t4 mr)
    (dotimes (ii t4)
      (dotimes (jj t3)
        (let ((cij (aref C (the fixnum (+ i ir ii)) (+ jr jj)))
              (ia (array-row-major-index Atd (+ ir ii) 0))
              (ib (array-row-major-index Baux jj 0)))
          (declare (type double-float cij)
                   (type fixnum ia ib))
          (dotimes-unroll2 (p 0 t2)
            (incf cij (* (row-major-aref Atd ia)
                         (row-major-aref Baux ib)))
            (incf ia)
            (incf ib))
          (setf (aref C (the fixnum (+ i ir ii)) (+ jr jj)) cij))))))


(declaim (ftype (function (fixnum fixnum fixnum fixnum
                           fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                gepm-blk1))
(defun gepm-blk1 (i t1 kc nr mr k n A B C)
  "Blocked GEPM"
  ;; Args
  ;;   i: row index
  ;;   t1: mc
  ;;   kc: ncol of Atd
  ;;   mr: minimum panel size for GEBP
  ;;   k: ncol of A and nrow of B
  ;;   n: ncol of B and C
  ;;   A: left input matrix
  ;;   B: right input matrix
  ;;   C: output matrix
  (declare (optimize (speed 3) (safety 0))
           (type fixnum i t1 kc nr mr k n)
           (type (simple-array double-float (* *)) A B C))
  (dotimes-interval2 (pc 0 k) (t2 kc)
    (let ((Atd (make-array (list t1 t2)
                           :element-type 'double-float)))
      (declare (type (simple-array double-float (* *)) Atd))
      (copy-matrix/double-float A i t1 pc t2 Atd)
      (dotimes-interval2 (jr 0 n) (t3 nr)
        (let ((Baux (make-array (list t3 t2)
                                :element-type 'double-float)))
          (declare (type (simple-array double-float (* *)) Baux))
          (copy-matrix-transpose/double-float B pc t2 jr t3 Baux)
          (gebp-reg2 i mr t1 t2 t3 Atd Baux jr C))))))


(declaim (ftype (function (fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                gemm3))
(defun gemm3 (mc kc nr mr A B C)
  "GEMM_VAR3"
  (declare (optimize (speed 3) (safety 0))
           (type fixnum mc kc nr mr)
           (type (simple-array double-float (* *)) A B C))
  (let* ((m (min (array-dimension A 0)
                 (array-dimension C 0)))
         (k (min (array-dimension A 1)
                 (array-dimension B 0)))
         (n (min (array-dimension B 1)
                 (array-dimension C 1))))
    (declare (type fixnum m k n))
    (dotimes-interval2 (i 0 m) (t1 mc)
      (gepm-blk1 i t1 kc nr mr k n A B C))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m))
(defun dm*m (A B C)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) A B C))
  (gemm1 512 256 4 4 A B C))
  ;; (gemm1 1024 32 4 512 A B C))
  ;; (gemm1 12 8 4 2 A B C))
  ;; (gemm3 512 256 4 4 A B C))
  ;; (gemm3 1024 32 4 512 A B C))
  ;; (gemm3 12 8 4 2 A B C))

