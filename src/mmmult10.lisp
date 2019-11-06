(in-package :cl-user)
(defpackage cl3a.mmmult10
  (:use :cl :sb-ext :sb-c :alexandria :cl3a.utilities :cl3a.mmmult10_vop)
  (:export :dm*m))
(in-package :cl3a.mmmult10)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +mc+ (the fixnum 512))
  (defconstant +pc+ (the fixnum 256))
  ;; (defconstant +mc+ (the fixnum 16))
  ;; (defconstant +pc+ (the fixnum 16))
  (defconstant +mpc+ (the fixnum (* +mc+ +pc+)))
  (defconstant +mr+ (the fixnum 4))
  (defconstant +nr+ (the fixnum 8))
  (defconstant +mnr+ (the fixnum (* +mr+ +nr+))))


(defparameter *Ampd* (make-array +mpc+ :element-type 'double-float))
(defparameter *Cmnd* (make-array +mnr+ :element-type 'double-float))
(defparameter *C1nd* (make-array +nr+ :element-type 'double-float))


(declaim (ftype (function ((simple-array double-float (* *))
                           int3 fixnum
                           (simple-array double-float (*))
                           int3))
                incf-Caux))
(defun incf-Caux (C pos i2 Caux sizes2)
  ;; Args
  ;;   C: output matrix
  ;;   pos: current position = (i1, k1, j1)
  ;;   i2: index from 0 to mc (position of Caux[0,0] = position of C[i1+i2,j1])
  ;;   Caux: result vector
  ;;   sizes2: original size of Caux (use i & j only)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) C)
           (type int3 pos sizes2)
           (type fixnum i2)
           (type (simple-array double-float (*)) Caux))
  (dotimes (i (int3-i sizes2))
    (let ((x (the fixnum (* i (int3-j sizes2))))
          (rmc (array-row-major-index C (the fixnum (+ (int3-i pos) i2 i)) (int3-j pos))))
      (declare (type fixnum x rmc))
      (dotimes (j (int3-j sizes2))
        (incf (row-major-aref C rmc) (aref Caux x))
        (setf (aref Caux x) 0d0)
        (incf x)
        (incf rmc)))))


(declaim (ftype (function (int3 int3
                           (simple-array double-float (*))
                           (simple-array double-float (* *))
                           (simple-array double-float (*))
                           int3 fixnum))
                dgebp-mr-nr))
(defun dgebp-mr-nr (sizes0 sizes1 Ampd B Caux pos i2)
  ;; Args
  ;;   sizes0: origianl sizes = (m, p, n))
  ;;   sizes1: sub sizes 1 = (+mc+ or m1r, +pc+ or p1r, n)
  ;;   sizes2: sub sizes 2 = (+mr+, +pc+ or p1r, +nr+)
  ;;   Ampd: contiguous vector (originally sub-matrix of A (size1) @ (i1, k1))
  ;;   Caux: contigous vector (originally sub-matrix of C (size2) @ (i1+i2, j1))
  ;;   pos: current position of Ampd, B, Caux = (i1, k1, j1)
  ;;   i2: index from 0 to mc
  ;; Positions
  ;;   i: A[i1+i2,:] = Ampd[i2,:], C[i1+i2,:] = Caux[0,:]
  ;;   k: A[:,k1] = Ampd[:,0]
  ;;   j: C[:,j1] = Caux[:,0]
  (declare (optimize (speed 3) (safety 1))
           (type int3 sizes0 sizes1 pos)
           (type (simple-array double-float (*)) Ampd Caux)
           (type (simple-array double-float (* *)) B)
           (type fixnum i2))
  (let* ((pc (int3-k sizes1))
         (n (int3-j sizes0))
         (rma (* i2 pc))
         (rmb (array-row-major-index B (int3-k pos) (int3-j pos)))
         (rmc 0))
    (declare (type fixnum pc n rma rmb rmc))
    (dgebp-mr-nr-ker pc n Ampd (sb-kernel:%array-data-vector B) Caux rma rmb rmc)))


(declaim (ftype (function (int3 int3
                           (simple-array double-float (*))
                           (simple-array double-float (* *))
                           (simple-array double-float (*))
                           int3 fixnum))
                dgebp-1-nr))
(defun dgebp-1-nr (sizes0 sizes1 Ampd B Caux pos i2)
  ;; Args
  ;;   sizes0: origianl sizes = (m, p, n))
  ;;   sizes1: sub sizes 1 = (+mc+ or m1r, +pc+ or p1r, n)
  ;;   sizes2: sub sizes 2 = (1, +pc+ or p1r, +nr+)
  ;;   Ampd: contiguous vector (originally sub-matrix of A (size1) @ (i1, k1))
  ;;   Caux: contigous vector (originally sub-matrix of C (size2) @ (i1+i2, j1))
  ;;   pos: current position of Ampd, B, Caux = (i1, k1, j1)
  ;;   i2: index from 0 to mc
  ;; Positions
  ;;   i: A[i1+i2,:] = Ampd[i2,:], C[i1+i2,:] = Caux[0,:]
  ;;   k: A[:,k1] = Ampd[:,0]
  ;;   j: C[:,j1] = Caux[:,0]
  (declare (optimize (speed 3) (safety 1))
           (type int3 sizes0 sizes1 pos)
           (type (simple-array double-float (*)) Ampd Caux)
           (type (simple-array double-float (* *)) B)
           (type fixnum i2))
  (let* ((pc (int3-k sizes1))
         (n (int3-j sizes0))
         (rma (* i2 pc))
         (rmb (array-row-major-index B (int3-k pos) (int3-j pos)))
         (rmc 0))
    (declare (type fixnum pc n rma rmb rmc))
    (dgebp-1-nr-ker pc n Ampd (sb-kernel:%array-data-vector B) Caux rma rmb rmc)))


(declaim (ftype (function (int3 int3
                           (simple-array double-float (*))
                           (simple-array double-float (* *))
                           (simple-array double-float (*))
                           int3 fixnum))
                dgebp-1-1))
(defun dgebp-1-1 (sizes1 sizes2 Ampd B Caux pos i2)
  ;; Args
  ;;   sizes0: origianl sizes = (m, p, n))
  ;;   sizes1: sub sizes 1 = (+mc+ or m1r, +pc+ or p1r, n)
  ;;   sizes2: sub sizes 2 = (mc, +pc+ or p1r, n1r)
  ;;   Ampd: contiguous vector (originally sub-matrix of A (size1) @ (i1, k1))
  ;;   Caux: contigous vector (originally sub-matrix of C (size2) @ (i1+i2, j1))
  ;;   pos: current position of Ampd, B, Caux = (i1, k1, j1)
  ;;   i2: index from 0 to mc
  ;; Positions
  ;;   i: A[i1+i2,:] = Ampd[i2,:], C[i1+i2,:] = Caux[0,:]
  ;;   k: A[:,k1] = Ampd[:,0]
  ;;   j: C[:,j1] = Caux[:,0]
  (declare (optimize (speed 3) (safety 0))
           (type int3 sizes1 sizes2 pos)
           (type (simple-array double-float (*)) Ampd Caux)
           (type (simple-array double-float (* *)) B)
           (type fixnum i2))
  (let ((rma (* i2 (int3-k sizes1))))
    (declare (type fixnum rma))
    (dotimes (k2 (int3-k sizes2))
      (declare (type fixnum k2))
      (let ((aik (aref Ampd rma))
            (rmb (array-row-major-index B (+ (int3-k pos) k2) (int3-j pos)))
            (rmc 0))
        (declare (type double-float aik)
                 (type fixnum rmb rmc))
        (dotimes (j2 (int3-j sizes2))
          (declare (type fixnum j2))
          (incf (aref Caux rmc)
                (* aik (row-major-aref B rmb)))
          (incf rmb)
          (incf rmc)))
      (incf rma)))
  nil)


(declaim (ftype (function (int3 int3
                           (simple-array double-float (*))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           int3))
                dgebp))
(defun dgebp (sizes0 sizes1 Ampd B C pos)
  ;; Args
  ;;   sizes0: origianl sizes = (m, p, n))
  ;;   sizes1: sub sizes 1 = (+mc+ or m1r, +pc+ or p1r, n)
  ;;   Ampd: contiguous vector (originally sub-matrix of A (size1) @ (i1, k1))
  ;;   pos: current position of Ampd, B, C = (i1, k1, 0)
  ;; Positions
  ;;   i: A[i1+i2,:] = Ampd[i2,:]
  ;;   k: A[:,k1] = Ampd[:,0]
  (declare (optimize (speed 3) (safety 0))
           (type int3 sizes0 sizes1 pos)
           (type (simple-array double-float (*)) Ampd)
           (type (simple-array double-float (* *)) B C))
  (let* ((mc (int3-i sizes1))
         (n (int3-j sizes1))
         (mc1 (min-factor mc +mr+))
         (n1 (min-factor n +nr+))
         (n1r (- n n1))
         (sizes2-00 (make-int3 :i +mr+ :k (int3-k sizes1) :j +nr+))
         (sizes2-10 (make-int3 :i 1 :k (int3-k sizes1) :j +nr+))
         (sizes2-11 (make-int3 :i mc :k (int3-k sizes1) :j n1r))
         (C11d (make-array (* mc n1r) :element-type 'double-float)))
    (declare (type fixnum mc n mc1 n1 n1r)
             (type int3 sizes2-00 sizes2-10 sizes2-11)
             (type (simple-array double-float (*)) C11d))
    (when (> n1 0)
      (do ((j1 (int3-j pos) (+ j1 +nr+)))
          ((>= j1 n1))
        (declare (type fixnum j1))
        (setf (int3-j pos) j1)
        (when (> mc 0)
          (do ((i2 0 (+ i2 +mr+)))
              ((>= i2 mc1))
            (declare (type fixnum i2))
            (let ((Caux (dgebp-mr-nr sizes0 sizes1 Ampd B *Cmnd* pos i2)))
              (declare (type (simple-array double-float (*)) Caux))
              ;; (print (format nil "mr-nr: (~A, ~A, ~A)" (+ (int3-i pos) i2) (int3-k pos) (int3-j pos)))
              (incf-Caux C pos i2 Caux sizes2-00))))
        (do ((i2 mc1 (1+ i2)))
            ((>= i2 mc))
          (declare (type fixnum i2))
          (let ((Caux (dgebp-1-nr sizes0 sizes1 Ampd B *C1nd* pos i2)))
            (declare (type (simple-array double-float (*)) Caux))
            ;; (print (format nil "1-nr: (~A, ~A, ~A)" (+ (int3-i pos) i2) (int3-k pos) (int3-j pos)))
            (incf-Caux C pos i2 Caux sizes2-10)))))
    (when (> n1r 0)
      (setf (int3-j pos) n1)
      (do ((i2 0 (1+ i2)))
          ((>= i2 mc))
        (declare (type fixnum i2))
        (dgebp-1-1 sizes1 sizes2-11 Ampd B C11d pos i2)
        ;; (print (format nil "1-1: (~A, ~A, ~A)" (+ (int3-i pos) i2) (int3-k pos) (int3-j pos)))
        (incf-Caux C pos i2 C11d sizes2-11)))
    (setf (int3-j pos) 0)))


(declaim (ftype (function (int3 int3
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           int3))
                dgepp))
(defun dgepp (sizes0 sizes1 A B C pos)
  ;; Args
  ;;   sizes0: original sizes = (m, p, n)
  ;;   sizes1: sub sizes 1 = (m, +pc+ or p1r, n)
  ;;   pos: current position of A, B, C = (0, k1, 0)
  (declare (optimize (speed 3) (safety 0))
           (type int3 sizes0 sizes1 pos)
           (type (simple-array double-float (* *)) A B C))
  (let* ((m (int3-i sizes0))
         (k1 (int3-k pos))
         (m1 (min-factor m +mc+))
         (m1r (- m m1))
         (pc (int3-k sizes1)))
    (declare (type fixnum m m1 m1r pc))
    (when (> m1 0)
      (do ((i1 0 (+ i1 +mc+)))
          ((>= i1 m1))
        (declare (type fixnum i1))
        (setf (int3-i pos) i1)
        (copy-matrix-to-vector-pd A i1 +mc+ k1 pc *Ampd*)
        (dgebp sizes0 sizes1 *Ampd* B C pos)))
    (when (> m1r 0)
      (let ((Ampd (make-array (* m1r pc) :element-type 'double-float)))
        (declare (type (simple-array double-float (*)) Ampd))
        (setf (int3-i pos) m1
              (int3-i sizes1) m1r)
        (copy-matrix-to-vector-pd A m1 m1r k1 pc Ampd)
        (dgebp sizes0 sizes1 Ampd B C pos)))
    (setf (int3-i pos) 0
          (int3-i sizes1) +mc+)))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m))
(defun dm*m (A B C)
  ;; C += A*B
  ;; Args
  ;;   A: left input matrix (m * p)
  ;;   B: right input matrix (p * n)
  ;;   C: output matrix (m * n)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) A B C))
  (let* ((m (min (array-dimension A 0)
                 (array-dimension C 0)))
         (p (min (array-dimension A 1)
                 (array-dimension B 0)))
         (n (min (array-dimension B 1)
                 (array-dimension C 1)))
         (p1 (min-factor p +pc+))
         (p1r (- p p1))
         (sizes0 (make-int3 :i m :k p :j n))
         (sizes1 (make-int3 :i +mc+ :k +pc+ :j n))
         (pos (make-int3 :i 0 :k 0 :j 0)))
    (declare (type fixnum m p n p1 p1r)
             (type int3 sizes0 sizes1 pos))
    (when (> p1 0)
      (do ((k1 0 (+ k1 +pc+)))
          ((>= k1 p1))
        (declare (type fixnum k1))
        (setf (int3-k pos) k1)
        (dgepp sizes0 sizes1 A B C pos)))
    (when (> p1r 0)
      (setf (int3-k pos) p1
            (int3-k sizes1) p1r)
      (dgepp sizes0 sizes1 A B C pos)))
  nil)