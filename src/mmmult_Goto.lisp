(in-package :cl-user)
(defpackage cl3a.mmmult_Goto
  (:use :cl :alexandria :cl3a.utilities :cl3a.transpose)
  (:export :gemm-kernel :gemm1 :dm*m))
(in-package :cl3a.mmmult_Goto)


(declaim (ftype (function ((simple-array double-float (* *))
                           fixnum fixnum fixnum fixnum)
                          (simple-array double-float (* *)))
                submatrix)
         (inline submatrix))
(defun submatrix (X si sj ni nj)
  "take sub-matrix of X from (si sj) with length of (ni nj)"
  ;; Args
  ;;   X: matrix
  ;;   si: start position of row
  ;;   sj: start position of column
  ;;   ni: length of row
  ;;   nj: length of column
  ;; Return
  ;;   sub-matrix
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) X)
           (type fixnum si sj ni nj))
  (let* ((ei (min (array-dimension X 0)
                  (+ si ni)))
         (ej (min (array-dimension X 1)
                  (+ sj nj)))
         (nr (- ei si))
         (nc (- ej sj))
         (res (make-array (list nr nc) :element-type 'double-float)))
    (declare (type fixnum ei ej nr nc)
             (type (simple-array double-float (* *)) res))
    (dotimes (i nr)
      (dotimes (j nc)
        (setf (aref res i j)
              (aref X (+ i si) (+ j sj)))))
    res))


(declaim (ftype (function ((simple-array double-float (* *)) fixnum)
                          (cons (simple-array double-float (* *)) t))
                hpack-matrix)
         (inline hpack-matrix))
(defun hpack-matrix (X width)
  "pack matrix X to horizontal direction with width"
  ;; Args
  ;;   X: matrix
  ;;   width: width interval to obtain the submatrix
  ;; Return
  ;;   (cons matrix matrix ...)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) X)
           (type fixnum width))
  (let ((nxr (array-dimension X 0))
        (nxc (array-dimension X 1))
        (res '()))
    (declare (type fixnum nxr nxc))
    (dotimes-interval2 (jj 0 nxc) (w width)
      (setf res
            (append res (list (submatrix X 0 jj nxr width)))))
    res))


(declaim (ftype (function ((simple-array double-float (* *)) fixnum)
                          (cons (simple-array double-float (* *)) t))
                vpack-matrix)
         (inline vpack-matrix))
(defun vpack-matrix (X height)
  "pack matrix X to vertical direction with height"
  ;; Args
  ;;   X: matrix
  ;;   height: height interval to obtain the submatrix
  ;; Return
  ;;   (cons matrix matrix ...)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) X)
           (type fixnum height))
  (let ((nxr (array-dimension X 0))
        (nxc (array-dimension X 1))
        (res '()))
    (declare (type fixnum nxr nxc))
    (dotimes-interval2 (ii 0 nxr) (h height)
      (setf res
            (append res (list (submatrix X ii 0 h nxc)))))
    res))



(declaim (ftype (function ((simple-array double-float (* *)) fixnum)
                          (cons (simple-array double-float (* *)) t))
                vpack-matrix-tpose)
         (inline vpack-matrix-tpose))
(defun vpack-matrix-tpose (X height)
  "slice matrix X to vertical direction by height with transposition"
  ;; Args
  ;;   X: matrix
  ;;   height: height interval to obtain the submatrix
  ;; Return
  ;;   (cons matrix matrix ...)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) X)
           (type fixnum height))
  (let ((nxr (array-dimension X 0))
        (nxc (array-dimension X 1))
        (res '()))
    (declare (type fixnum nxr nxc))
    (dotimes-interval2 (ii 0 nxr) (h height)
      (let ((subx (submatrix X ii 0 h nxc)))
        (declare (type (simple-array double-float (* *)) subx))
        (setf res
              (append res (list (tpose/double-float subx))))))
    res))


(declaim (ftype (function (fixnum fixnum fixnum
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                gebp-kernel)
         (inline gebp-kernel))
(defun gebp-kernel (m k n A B C)
  "kernel program of GEBP: C += A * B"
  ;; Args
  ;;   m: length of row of A = length of row of C
  ;;   k: length of column of A = length of row of B
  ;;   n: length of column of B = length of column of C
  ;;   A: left input matrix
  ;;   B: right input matrix
  ;;   C: output matrix
  (declare (optimize (speed 3) (safety 0))
           (type fixnum m k n)
           (type (simple-array double-float (* *)) A B C))
  (let* ((ms (min m (array-dimension A 0) (array-dimension C 0)))
         (ks (min k (array-dimension A 1) (array-dimension B 0)))
         (ns (min n (array-dimension B 1) (array-dimension C 1)))
         (n0 (min-factor ns 4))
         (maxj 0))
    (declare (type fixnum ms ks ns n0 maxj))
    (dotimes (i ms)
      (dotimes (p ks)
        (let ((maip (aref A i p))
              (ib (array-row-major-index B p 0))
              (ic (array-row-major-index C i 0)))
          (declare (type double-float maip)
                   (type fixnum ib ic))
          (setf maxj
                (do ((j 0 (+ j 4)))
                    ((>= j n0) j)
                  (incf (row-major-aref C ic)
                        (* maip (row-major-aref B ib)))
                  (incf ib)
                  (incf ic)
                  (incf (row-major-aref C ic)
                        (* maip (row-major-aref B ib)))
                  (incf ib)
                  (incf ic)
                  (incf (row-major-aref C ic)
                        (* maip (row-major-aref B ib)))
                  (incf ib)
                  (incf ic)
                  (incf (row-major-aref C ic)
                        (* maip (row-major-aref B ib)))
                  (incf ib)
                  (incf ic)))
          (do ((j maxj (1+ j)))
              ((>= j ns))
            (incf (row-major-aref C ic)
                  (* maip (row-major-aref B ib)))
            (incf ib)
            (incf ic)))))))


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
  (let ((k0 (min-factor t2 4))
        (maxk 0))
    (declare (type fixnum k0 maxk))
    (dotimes-interval2 (ir 0 t1) (t4 mr)
      (dotimes (i t4)
        (dotimes (j t3)
          (let ((cij (aref Caux (+ ir i) j))
                (ia (array-row-major-index Atd (+ ir i) 0))
                (ib (array-row-major-index Bp (+ jr j) 0)))
            (declare (type double-float cij)
                     (type fixnum ia ib))
            (setf maxk
                  (do ((k 0 (+ k 4)))
                      ((>= k k0) k)
                    (incf cij (* (row-major-aref Atd ia)
                                 (row-major-aref Bp ib)))
                    (incf ia)
                    (incf ib)
                    (incf cij (* (row-major-aref Atd ia)
                                 (row-major-aref Bp ib)))
                    (incf ia)
                    (incf ib)
                    (incf cij (* (row-major-aref Atd ia)
                                 (row-major-aref Bp ib)))
                    (incf ia)
                    (incf ib)
                    (incf cij (* (row-major-aref Atd ia)
                                 (row-major-aref Bp ib)))
                    (incf ia)
                    (incf ib)))
            (do ((k maxk (1+ k)))
                ((>= k t2))
              (incf cij (* (row-major-aref Atd ia)
                           (row-major-aref Bp ib)))
              (incf ia)
              (incf ib))
            (incf (aref Caux (+ ir i) j) cij)))))))


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
             (dotimes (jj t3)
               (incf (aref C (+ ic ii) (+ jr jj))
                     (aref Caux ii jj)))))))))


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


(declaim (ftype (function (fixnum fixnum
                           (cons (simple-array double-float (* *)) t)
                           (simple-array double-float (* *))
                           ;; (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (cons (simple-array double-float (* *)) t)))
                gepm-blk1))
;; (defun gepm-blk1 (i t1 mr Ai-tildes Ap B Baux C Ci-tildes)
(defun gepm-blk1 (i t1 Ai-tildes B Baux C Ci-tildes)
  "Blocked GEPM"
  ;; Args
  ;;   t1: mc
  ;;   mr: minimum panel size for GEBP
  ;;   Ai-tildes: (cons Ai0~ Ai1~ Ai2~ ...) where Aip~ is mc*kc
  ;;   Ap: Aip matrix of mr*kc
  ;;   B: right input matrix
  ;;   Baux: minimum input matrix of B of kc*nr
  ;;   C: output matrix
  ;;   Ci-tildes: (cons C0_~ C1_~ C2_~ ...) where Ci_~ is mc*nr
  (declare (optimize (speed 3) (safety 0))
           (type fixnum i t1)
           ;; (type (simple-array double-float (* *)) Ap B Baux C)
           (type (simple-array double-float (* *)) B Baux C)
           (type (cons (simple-array double-float (* *)) t) Ai-tildes Ci-tildes))
  (let ((k 0)
        (j 0))
    (declare (type fixnum k j))
    (dolist (Cit Ci-tildes)
      (declare (type (simple-array double-float (* *)) Cit))
      ;; (print "Cit dolist start!")
      (let* ((t3 (array-dimension Cit 1)))
        (declare (type fixnum t3))
        (dolist (Ait Ai-tildes)
          (declare (type (simple-array double-float (* *)) Ait))
          ;; (print "Ait dolist start!")
          (let ((t2 (array-dimension Ait 1)))
            (declare (type fixnum t2))
            (copy-matrix/double-float B k t2 j t3 Baux)
            ;; (dotimes-interval2 (r 0 t1) (t4 mr)
            ;;   (copy-matrix Ait r t4 0 t2 Ap)
            ;;   (gebp-kernel t4 t2 t3 Ap Baux Cit))
            (gebp-kernel t1 t2 t3 Ait Baux Cit)
            (incf k t2)))
        (dotimes (ii t1)
          (dotimes (jj t3)
            (incf (aref C (+ i ii) (+ j jj))
                  (aref Cit ii jj))))
        (incf j t3))
      (setf k 0))))


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
                 (array-dimension C 1)))
         (Ai (make-array (list (min mc m) k)
                         :element-type 'double-float))
         ;; (Ap (make-array (list mr (min kc k))
         ;;                 :element-type 'double-float))
         (Ci (make-array (list (min mc m) n)
                         :element-type 'double-float))
         (Baux (make-array (list kc nr) :element-type 'double-float)))
    (declare (type fixnum m k n)
             ;; (type (simple-array double-float (* *)) Ai Ap Ci Baux))
             (type (simple-array double-float (* *)) Ai Ci Baux))
    (dotimes-interval2 (i 0 m) (t1 mc)
      (copy-matrix/double-float A i t1 0 k Ai)
      (copy-matrix/double-float C i t1 0 n Ci)
      (let ((Ai-tildes (hpack-matrix Ai kc))
            (Ci-tildes (hpack-matrix Ci nr)))
        ;; (gepm-blk1 i t1 mr Ai-tildes Ap B Baux C Ci-tildes)))))
        (gepm-blk1 i t1 Ai-tildes B Baux C Ci-tildes)))))


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

