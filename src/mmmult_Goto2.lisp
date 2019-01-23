(in-package :cl-user)
(defpackage cl3a.mmmult_Goto
  (:use :cl :alexandria :cl3a.utilities :cl3a.mmmult_Goto2_vop)
  (:export :dm*m))
(in-package :cl3a.mmmult_Goto2)


(declaim (ftype (function (fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           fixnum
                           (simple-array double-float (* *))))
                dgebp-reg))
(defun dgebp-reg (mr t1 t2 t3 Atd Bp jr Caux)
  "register-scale program of GEBP: Caux += Atd * Bp[ib]"
  ;; Args
  ;;   mr: block size of row of A & C
  ;;   t1: length of row of A = length of row of C
  ;;   t2: length of column of A = length of row of B
  ;;   t3: length of column of B = length of column of C
  ;;   Atd: left input matrix A-tilde
  ;;   Bp: right input matrix B-packed
  ;;   jr: target calculation of column at B & C
  ;;   Caux: output sub-matrix
  (do ((ir 0 (+ ir mr)))
      ((>= ir t1))
    (dotimes (i mr)
      (let ((rmc (array-row-major-index Caux (+ ir i) 0)))
        (declare (type fixnum rmc))
        (dotimes (j t3)
          (let ((cij (row-major-aref Caux rmc))
                (ia (array-row-major-index Atd (+ ir i) 0))
                (ib (array-row-major-index Bp (+ jr j) 0)))
            (declare (type double-float cij)
                     (type fixnum ia ib))
            (incf (row-major-aref Caux rmc)
                  (dgebp-reg-ker t2 Atd ia Bp ib cij))
            (incf rmc)))))))


(declaim (ftype (function (fixnum fixnum fixnum fixnum
                           fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dgepp-blk1))
(defun dgepp-blk1 (p mc t2 nr mr m n A Bp C)
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
  (dotimes-interval2 (ic 0 m) (t1 mc)
    (let ((Atd (make-array (list t1 t2)
                           :element-type 'double-float)))
      (declare (type (simple-array double-float (* *)) Atd))
      (copy-matrix/double-float A ic t1 p t2 Atd)
      (dotimes-interval2 (jr 0 n) (t3 nr)
        (let ((Caux (make-array (list t1 t3)
                                :element-type 'double-float)))
          (declare (type (simple-array double-float (* *)) Caux))
          (dgebp-reg mr t1 t2 t3 Atd Bp jr Caux)
          (dotimes (ii t1)
            (let ((rmc (array-row-major-index C (+ ic ii) jr))
                  (rmcaux (array-row-major-index Caux ii 0)))
              (declare (type fixnum rmc rmcaux))
              (dotimes (jj t3)
                (incf (row-major-aref C rmc)
                      (row-major-aref Caux rmcaux))
                (incf rmc)
                (incf rmcaux)))))))))


(declaim (ftype (function (fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dgemm1))
(defun dgemm1 (mc kc nr mr A B C)
  "GEMM_VAR1"
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
      (dgepp-blk1 p mc t2 nr mr m n A Bp C))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m))
(defun dm*m (A B C)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) A B C))
  (dgemm1 128 1024 8 16 A B C))
