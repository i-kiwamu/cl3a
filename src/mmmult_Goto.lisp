(in-package :cl-user)
(defpackage cl3a.mmmult_Goto
  (:use :cl :alexandria :cl3a.utilities)
  (:export :gemm1 :gemm3 :dm*m))
(in-package :cl3a.mmmult_Goto)


(declaim (ftype (function ((simple-array double-float (*))
                           fixnum)
                          (sb-kernel:simd-pack double-float))
                load2-sse-from-array))
(defun load2-sse-from-array (matv i)
  (declare (type (simple-array double-float (*)) matv)
           (type fixnum i))
  (let ((x1 (row-major-aref matv i))
        (x2 (row-major-aref matv (1+ i))))
    (declare (type double-float x1 x2))
    (sb-kernel:%make-simd-pack-double x1 x2)))


(defmacro gebp-reg-ker (t2 Atd ia Bp ib cij)
  (with-gensyms (pp)
    `(progn
       (dotimes-unroll2 (,pp 0 ,t2)
         (incf ,cij
               (* (row-major-aref ,Atd ,ia)
                  (row-major-aref ,Bp ,ib)))
         (incf ,ia)
         (incf ,ib))
       ,cij)))


(defmacro gebp-reg (eltype mr t1 t2 t3 Atd Bp jr Caux)
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
  (with-gensyms (calc ir t4 i rmc j cij ia ib)
    `(flet ((,calc (,t2 ,Atd ,ia ,Bp ,ib ,cij)
              (declare (optimize (speed 3) (safety 0))
                       (type fixnum ,t2 ,ia ,ib)
                       (type (simple-array ,eltype (* *)) ,Atd ,Bp)
                       (type ,eltype ,cij))
              (gebp-reg-ker ,t2 ,Atd ,ia ,Bp ,ib ,cij)))
       (dotimes-interval2 (,ir 0 ,t1) (,t4 ,mr)
         (dotimes (,i ,t4)
           (let ((,rmc (array-row-major-index ,Caux (+ ,ir ,i) 0)))
             (declare (type fixnum ,rmc))
             (dotimes (,j ,t3)
               (let ((,cij (row-major-aref ,Caux ,rmc))
                     (,ia (array-row-major-index ,Atd (+ ,ir ,i) 0))
                     (,ib (array-row-major-index ,Bp (+ ,jr ,j) 0)))
                 (declare (type ,eltype ,cij)
                          (type fixnum ,ia ,ib))
                 (incf (row-major-aref ,Caux ,rmc)
                       (,calc ,t2 ,Atd ,ia ,Bp ,ib ,cij))
                 (incf ,rmc)))))))))

(macrolet
    ((define-gebp-reg (eltype)
       (let ((fname (symbolicate 'gebp-reg/ eltype)))
         `(progn
            (declaim (ftype (function
                             (fixnum fixnum fixnum fixnum
                              (simple-array ,eltype (* *))
                              (simple-array ,eltype (* *))
                              fixnum
                              (simple-array ,eltype (* *))))
                            ,fname))
            (defun ,fname (mr t1 t2 t3 Atd Bp jr Caux)
              (declare (optimize (speed 3) (safety 0))
                       (type fixnum mr t1 t2 t3 jr)
                       (type (simple-array ,eltype (* *)) Atd Bp Caux))
              (gebp-reg ,eltype mr t1 t2 t3 Atd Bp jr Caux))))))
  (define-gebp-reg double-float)
  (define-gebp-reg long-float))


(defmacro gepp-blk1 (eltype p mc t2 nr mr m n A Bp C)
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
  (let ((copy-matrix-eltype (symbolicate 'copy-matrix/ eltype))
        (gebp-reg-eltype (symbolicate 'gebp-reg/ eltype)))
    (with-gensyms (ic t1 Atd jr t3 Caux ii rmc rmcaux jj)
      `(dotimes-interval2 (,ic 0 ,m) (,t1 ,mc)
         (let ((,Atd (make-array (list ,t1 ,t2)
                                 :element-type ',eltype)))
           (declare (type (simple-array ,eltype (* *)) ,Atd))
           (,copy-matrix-eltype ,A ,ic ,t1 ,p ,t2 ,Atd)
           (dotimes-interval2 (,jr 0 ,n) (,t3 ,nr)
             (let ((,Caux (make-array (list ,t1 ,t3)
                                      :element-type ',eltype)))
               (declare (type (simple-array ,eltype (* *)) ,Caux))
               (,gebp-reg-eltype ,mr ,t1 ,t2 ,t3 ,Atd ,Bp ,jr ,Caux)
               (dotimes (,ii ,t1)
                 (let ((,rmc (array-row-major-index ,C (+ ,ic ,ii) ,jr))
                       (,rmcaux (array-row-major-index ,Caux ,ii 0)))
                   (declare (type fixnum ,rmc ,rmcaux))
                   (dotimes (,jj ,t3)
                     (incf (row-major-aref ,C ,rmc)
                           (row-major-aref ,Caux ,rmcaux))
                     (incf ,rmc)
                     (incf ,rmcaux)))))))))))

(macrolet
    ((define-gepp-blk1 (eltype)
       (let ((fname (symbolicate 'gepp-blk1/ eltype)))
         `(progn
            (declaim (ftype (function
                             (fixnum fixnum fixnum fixnum fixnum
                              fixnum fixnum
                              (simple-array ,eltype (* *))
                              (simple-array ,eltype (* *))
                              (simple-array ,eltype (* *))))
                            ,fname))
            (defun ,fname (p mc t2 nr mr m n A Bp C)
              (declare (optimize (speed 3) (safety 0))
                       (type fixnum p mc t2 nr mr m n)
                       (type (simple-array ,eltype (* *)) A Bp C))
              (gepp-blk1 ,eltype p mc t2 nr mr m n A Bp C))))))
  (define-gepp-blk1 double-float)
  (define-gepp-blk1 long-float))


(defmacro gemm1 (eltype mc kc nr mr A B C)
  "GEMM_VAR1"
  (let ((copy-tmatrix-eltype (symbolicate 'copy-matrix-transpose/ eltype))
        (gepp-blk1-eltype (symbolicate 'gepp-blk1/ eltype)))
    (with-gensyms (m k n Bp p t2)
      `(let* ((,m (min (array-dimension ,A 0)
                       (array-dimension ,C 0)))
              (,k (min (array-dimension ,A 1)
                       (array-dimension ,B 0)))
              (,n (min (array-dimension ,B 1)
                       (array-dimension ,C 1)))
              (,Bp (make-array (list ,n (min ,kc ,k))
                               :element-type ',eltype)))
         (declare (type fixnum ,m ,k ,n)
                  (type (simple-array ,eltype (* *)) ,Bp))
         (dotimes-interval2 (,p 0 ,k) (,t2 ,kc)
           (,copy-tmatrix-eltype ,B ,p ,t2 0 ,n ,Bp)
           (,gepp-blk1-eltype ,p ,mc ,t2 ,nr ,mr ,m ,n ,A ,Bp ,C))))))

(macrolet
    ((define-gemm1 (eltype)
       (let ((fname (symbolicate 'gemm1/ eltype)))
         `(progn
            (declaim (ftype (function
                             (fixnum fixnum fixnum fixnum
                              (simple-array ,eltype (* *))
                              (simple-array ,eltype (* *))
                              (simple-array ,eltype (* *))))
                            ,fname))
            (defun ,fname (mc kc nr mr A B C)
              (declare (optimize (speed 3) (safety 0))
                       (type fixnum mc kc nr mr)
                       (type (simple-array ,eltype (* *)) A B C))
              (gemm1 ,eltype mc kc nr mr A B C))))))
  (define-gemm1 double-float)
  (define-gemm1 long-float))



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
  (gemm1/double-float 128 1024 8 16 A B C))
  ;; (gemm1 1024 32 4 512 A B C))
  ;; (gemm1 256 512 8 2 A B C))
  ;; (gemm1 12 4 2 2 A B C))
  ;; (gemm3 512 256 4 4 A B C))
  ;; (gemm3 1024 32 4 512 A B C))
  ;; (gemm3 256 512 8 2 A B C))
  ;; (gemm3 12 8 4 2 A B C))

