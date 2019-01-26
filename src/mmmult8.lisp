(in-package :cl-user)
(defpackage cl3a.mmmult8
  (:use :cl :sb-ext :sb-c :alexandria :cl3a.utilities :cl3a.mmmult8_vop)
  (:export :dm*m))
(in-package :cl3a.mmmult8)


(declaim (ftype (function (integer
                           (simple-array double-float (* *)) integer
                           (simple-array double-float (* *)) integer)
                          double-float)
                sum-dgebp-reg-ker))
(defun sum-dgebp-reg-ker (t20 Atd ia Bp ib)
  (declare (optimize (speed 3) (safety 0))
           (inline sum-dgebp-reg-ker)
           (type integer t20 ia ib)
           (type (simple-array double-float (* *)) Atd Bp))
  (multiple-value-bind (r0 r1 r2 r3)
      (%simd-pack-256-doubles
       (dgebp-reg-ker t20
                      (sb-kernel:%array-data-vector Atd) ia
                      (sb-kernel:%array-data-vector Bp) ib))
    (+ r0 r1 r2 r3)))


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
  ;;   t1: mc or m1
  ;;   t2: kc or k1
  ;;   t3: nr or n1
  ;;   Atd: left input matrix A-tilde (t1 * t2)
  ;;   Bp: right input matrix B-packed (n * kc, transposed)
  ;;   jr: target calculation of column at B & C
  ;;   Caux: output sub-matrix (t1 * t3)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum mr t1 t2 t3 jr)
           (type (simple-array double-float (* *)) Atd Bp Caux))
  (let ((t20 (min-factor t2 8)))
    (declare (type fixnum t20))
    (do ((ir 0 (+ ir mr)))
        ((>= ir t1))
      (declare (type fixnum ir))
      (dotimes (i (min mr t1 (- t1 ir)))
        (let* ((iri (+ ir i))
               (rmc (array-row-major-index Caux iri 0)))
          (declare (type fixnum iri rmc))
          (dotimes (j t3)
            (let ((ia (array-row-major-index Atd iri 0))
                  (ib (array-row-major-index Bp (+ jr j) 0)))
              (declare (type fixnum ia ib))
              (incf (row-major-aref Caux rmc)
                    (+ (sum-dgebp-reg-ker t20 Atd ia Bp ib)
                       (loop :for p :of-type fixnum :from t20 :below t2
                             :sum (* (row-major-aref Atd (+ ia p))
                                     (row-major-aref Bp (+ ib p)))
                               :into c :of-type double-float
                             :finally (return c)))))
            (incf rmc)))))))


(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dgepp-blk1i))
(defun dgepp-blk1i (t1 t2 nr mr n ic Atd Bp C)
  ""
  ;; Args
  ;;   t1: mc or m1
  ;;   t2: kc or k1
  ;;   nr: ncol of submatrix of Btilde
  ;;   mr: nrow of submatrix of Atilde
  ;;   n: ncol of B & C
  ;;   ic: index from 0 to m
  ;;   Atd: left input matrix A-tilde (t1 * t2)
  ;;   Bp: right input matrix B-packed (n * kc, transposed)
  ;;   C: output matrix (k * n)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum t1 t2 nr mr n ic)
           (type (simple-array double-float (* *)) Atd Bp C))
  (flet ((incf-back-matrix (jr t1 t3 Caux C)
           (declare (optimize (speed 3) (safety 0))
                    (type fixnum jr t1 t3)
                    (type (simple-array double-float (* *)) Caux C))
           (dotimes (ii t1)
             (let ((rmc (array-row-major-index C (+ ic ii) jr))
                   (rmcaux (array-row-major-index Caux ii 0)))
               (declare (type fixnum rmc rmcaux))
               (dotimes (jj t3)
                 (incf (row-major-aref C rmc)
                       (row-major-aref Caux rmcaux))
                 (incf rmc)
                 (incf rmcaux))))))
    (declare (inline incf-back-matrix))
    (let* ((n0 (min-factor n nr))
           (n1 (- n n0)))
      (declare (type fixnum n0 n1))
      (unless (= n0 0)
        (do ((jr 0 (+ jr nr)))
            ((>= jr n0))
          (let ((Caux (make-array (list t1 nr)
                                  :element-type 'double-float)))
            (declare (type (simple-array double-float (* *)) Caux))
            (dgebp-reg mr t1 t2 nr Atd Bp jr Caux)
            (incf-back-matrix jr t1 nr Caux C))))
      (when (> n1 0)
        (let ((Caux (make-array (list t1 n1)
                                :element-type 'double-float)))
          (declare (type (simple-array double-float (* *)) Caux))
          (dgebp-reg mr t1 t2 n1 Atd Bp n0 Caux)
          (incf-back-matrix n0 t1 n1 Caux C))))))


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
  ;;   t2: kc or k1
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
  (let* ((m0 (min-factor m mc))
         (m1 (- m m0)))
    (declare (type fixnum m0 m1))
    (unless (= m0 0)
      (do ((ic 0 (+ ic mc)))
          ((>= ic m0))
        (let ((Atd (make-array (list mc t2)
                               :element-type 'double-float)))
          (declare (type (simple-array double-float (* *)) Atd))
          (copy-matrix-pd A ic mc p t2 Atd)
          (dgepp-blk1i mc t2 nr mr n ic Atd Bp C))))
    (when (> m1 0)
      (let ((Atd (make-array (list m1 t2)
                             :element-type 'double-float)))
        (declare (type (simple-array double-float (* *)) Atd))
        (copy-matrix-sd A m0 m1 p t2 Atd)
        (dgepp-blk1i m1 t2 nr mr n m0 Atd Bp C)))))


(declaim (ftype (function (fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dgemm1))
(defun dgemm1 (mc kc nr mr A B C)
  "GEMM_VAR1"
  (declare (optimize (speed 3) (safety 0))
           (type fixnum mc kc nr mr)
           (type (simple-array double-float (* *)) A B C))
  (let* ((m (min (array-dimension A 0)
                 (array-dimension C 0)))
         (k (min (array-dimension A 1)
                 (array-dimension B 0)))
         (k0 (min-factor k kc))
         (k1 (- k k0))
         (n (min (array-dimension B 1)
                 (array-dimension C 1)))
         (Bp (make-array (list n (min kc k))
                         :element-type 'double-float)))
    (declare (type fixnum m k k0 k1 n)
             (type (simple-array double-float (* *)) Bp))
    (unless (= k0 0)
      (do ((p 0 (+ p kc)))
          ((>= p k0))
        (declare (type fixnum p))
        (copy-matrix-transpose-sd B p kc 0 n Bp)
        (dgepp-blk1 p mc kc nr mr m n A Bp C)))
    (when (> k1 0)
      (copy-matrix-transpose-sd B k0 k1 0 n Bp)
      (dgepp-blk1 k0 mc k1 nr mr m n A Bp C))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m))
(defun dm*m (A B C)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) A B C))
  ;;      mc  kc   nr mr
  ;;      m1  k1   n1
  ;;      t1  t2   t3
  ; (dgemm1 128 1024 8 16 A B C))
  (dgemm1 128 1020 8 16 A B C))
