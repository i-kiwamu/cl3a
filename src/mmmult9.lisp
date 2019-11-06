(in-package :cl-user)
(defpackage cl3a.mmmult9
  (:use :cl :sb-ext :sb-c :alexandria :cl3a.utilities :cl3a.mmmult9_vop)
  (:export :dm*m))
(in-package :cl3a.mmmult9)


(declaim (ftype (function (fixnum
                           (simple-array double-float (* *)) fixnum
                           (simple-array double-float (* *)) fixnum
                           (simple-array double-float (* *)) fixnum))
                incf-dgebp))
(defun incf-dgebp (k A rma Bt rmb C rmc)
  (declare (optimize (speed 3) (safety 1))  ; safety should be 1 to avoid "Unhandled memory fault"
           (type fixnum k rma rmb rmc)
           (type (simple-array double-float (* *)) A Bt C))
  (let ((k0 (min-factor k 8)))
    (declare (type fixnum k0))
    (multiple-value-bind (r0 r1 r2 r3)
        (%simd-pack-256-doubles
         (dgebp-reg-ker k0
                        (sb-kernel:%array-data-vector A) rma
                        (sb-kernel:%array-data-vector Bt) rmb))
      (incf (row-major-aref C rmc)
            (+ r0 r1 r2 r3
               (loop :for p :of-type fixnum :from k0 :below k
                     :sum (* (row-major-aref A (+ rma p))
                             (row-major-aref Bt (+ rmb p)))
                       :into c :of-type double-float
                     :finally (return c))))))
  nil)


(declaim (ftype (function (fixnum
                           (simple-array double-float (* *)) fixnum
                           (simple-array double-float (* *)) fixnum
                           (simple-array double-float (* *)) fixnum))
                incf-dgebp4))
(defun incf-dgebp4 (k A rma Bt rmb C rmc)
  (declare ; (optimize (speed 3) (safety 0))
           (optimize (speed 3) (safety 1))  ; safety should be 1 to avoid "Unhandled memory fault"
           (type fixnum k rma rmb rmc)
           (type (simple-array double-float (* *)) A Bt C))
  (let* ((k0 (min-factor k 8))
         (rmb1 (+ rmb k))
         (rmb2 (+ rmb1 k))
         (rmb3 (+ rmb2 k))
         (rmc1 (1+ rmc))
         (rmc2 (1+ rmc1))
         (rmc3 (1+ rmc2)))
    (declare (type fixnum k0 rmb1 rmb2 rmb3 rmc1 rmc2 rmc3))
    (multiple-value-bind (r0 r1 r2 r3)
        (%simd-pack-256-doubles
         (dgebp-reg-ker4 k k0
                         (sb-kernel:%array-data-vector A) rma
                         (sb-kernel:%array-data-vector Bt) rmb))
      (incf (row-major-aref C rmc)
            (+ r0
               (loop :for p :of-type fixnum :from k0 :below k
                     :sum (* (row-major-aref A (+ rma p))
                             (row-major-aref Bt (+ rmb p)))
                       :into c :of-type double-float
                     :finally (return c))))
      (incf (row-major-aref C rmc1)
            (+ r2
               (loop :for p :of-type fixnum :from k0 :below k
                     :sum (* (row-major-aref A (+ rma p))
                             (row-major-aref Bt (+ rmb1 p)))
                       :into c :of-type double-float
                     :finally (return c))))
      (incf (row-major-aref C rmc2)
            (+ r1
               (loop :for p :of-type fixnum :from k0 :below k
                     :sum (* (row-major-aref A (+ rma p))
                             (row-major-aref Bt (+ rmb2 p)))
                       :into c :of-type double-float
                     :finally (return c))))
      (incf (row-major-aref C rmc3)
            (+ r3
               (loop :for p :of-type fixnum :from k0 :below k
                     :sum (* (row-major-aref A (+ rma p))
                             (row-major-aref Bt (+ rmb3 p)))
                       :into c :of-type double-float
                     :finally (return c))))))
  nil)


(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dgebp-reg))
(defun dgebp-reg (t1 k t3 mr ic jr A Bt C)
  "register-scale program of GEBP: C += A * Bt[ib]"
  ;; Args
  ;;   t1: mc or m1
  ;;   k: nrow of matrix A & Bt
  ;;   t3: nr or n1
  ;;   mr: block size of row of A & C
  ;;   ic: target calculation of row at A & C
  ;;   jr: target calculation of column at B & C
  ;;   A: left input matrix A (m * k)
  ;;   Bt: right input matrix B (n * k, transposed)
  ;;   C: output matrix (m * n)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum mr t1 k t3 mr ic jr)
           (type (simple-array double-float (* *)) A Bt C))
  (let ((t10 (min-factor t1 mr))
        (t30 (min-factor t3 4)))
    (declare (type fixnum t10 t30))
    (do ((ir 0 (+ ir mr)))
        ((>= ir t10))
      (declare (type fixnum ir))
      (dotimes (i (min mr t1))
        (let ((rma (array-row-major-index A (+ ic ir i) 0)))
          (declare (type fixnum rma))
          (do ((j 0 (+ j 4)))
              ((>= j t30))
            (let ((rmb (array-row-major-index Bt (+ jr j) 0))
                  (rmc (array-row-major-index C (+ ic ir i) (+ jr j))))
              (declare (type fixnum rmb rmc))
              (incf-dgebp4 k A rma Bt rmb C rmc)))
          (do ((j t30 (1+ j)))
              ((>= j t3))
            (let ((rmb (array-row-major-index Bt (+ jr j) 0))
                  (rmc (array-row-major-index C (+ ic ir i) (+ jr j))))
              (declare (type fixnum rmb rmc))
              (incf-dgebp k A rma Bt rmb C rmc))))))
    (do ((ir t10 (1+ ir)))
        ((>= ir t1))
      (let ((rma (array-row-major-index A (+ ic ir) 0)))
        (declare (type fixnum rma))
        (dotimes (j t3)
          (let ((rmb (array-row-major-index Bt (+ jr j) 0))
                (rmc (array-row-major-index C (+ ic ir) (+ jr j))))
            (declare (type fixnum rmb rmc))
            (incf-dgebp k A rma Bt rmb C rmc)))))))


(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dgepp-blk1i))
(defun dgepp-blk1i (t1 k nr mr n ic A Bt C)
  ""
  ;; Args
  ;;   t1: mc or m1
  ;;   k: nrow of matrix A & Bt
  ;;   nr: ncol of submatrix of Btilde
  ;;   mr: nrow of submatrix of Atilde
  ;;   n: ncol of B & C
  ;;   ic: index from 0 to m
  ;;   A: left input matrix A
  ;;   Bt: right input matrix B (n * k, transposed)
  ;;   C: output matrix (k * n)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum t1 k nr mr n ic)
           (type (simple-array double-float (* *)) A Bt C))
  (let ((n0 (min-factor n nr)))
    (declare (type fixnum n0))
    (unless (= n0 0)
      (do ((jr 0 (+ jr nr)))
          ((>= jr n0))
        (declare (type fixnum jr))
        (dgebp-reg t1 k nr mr ic jr A Bt C)))
    (when (> n n0)
      (dgebp-reg t1 k (- n n0) mr ic n0 A Bt C))))


(declaim (ftype (function (fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dgemm1))
(defun dgemm1 (mc nr mr A B C)
  "GEMM_VAR1"
  (declare (optimize (speed 3) (safety 0))
           (type fixnum mc nr mr)
           (type (simple-array double-float (* *)) A B C))
  (let* ((m (min (array-dimension A 0)
                 (array-dimension C 0)))
         (m0 (min-factor m mc))
         (k (min (array-dimension A 1)
                 (array-dimension B 0)))
         (n (min (array-dimension B 1)
                 (array-dimension C 1)))
         (Bt (make-array (list n k)
                         :element-type 'double-float)))
    (declare (type fixnum m m0 k n)
             (type (simple-array double-float (* *)) Bt))  ;; consider dynamic-extent for Bt
    (copy-matrix-transpose-sd B 0 k 0 n Bt)
    (unless (= m0 0)
      (do ((ic 0 (+ ic mc)))
          ((>= ic m0))
        (declare (type fixnum ic))
        (dgepp-blk1i mc k nr mr n ic A Bt C)))
    (when (> m m0)
      (dgepp-blk1i (- m m0) k nr mr n m0 A Bt C))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m))
(defun dm*m (A B C)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) A B C))
  ;;      mc  nr mr
  ;;      m1  n1
  ;;      t1  t2
  ;; (dgemm1 128 8 16 A B C))
  (dgemm1 128 8 64 A B C))
