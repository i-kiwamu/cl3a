(in-package :cl-user)
(defpackage cl3a.mmmult6
  (:use :cl :sb-ext :sb-c :alexandria :cl3a.utilities)
  (:export :dm*m :f2+ :f2* :load2-sse-from-array :store2-sse-to-array))
(in-package :cl3a.mmmult6)


(defknown f2+ ((simd-pack double-float)
               (simd-pack double-float))
    (simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)
(defknown f2* ((simd-pack double-float)
               (simd-pack double-float))
    (simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)
(defknown load2-sse-from-array ((simple-array double-float (*))
                                 fixnum)
    (simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)
(defknown store2-sse-to-array ((simple-array double-float (*))
                               fixnum
                               (simd-pack double-float))
    double-float
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(in-package :sb-vm)

(define-vop (cl3a.mmmult6::f2+)
  (:translate cl3a.mmmult6::f2+)
  (:policy :fast-safe)
  (:args (x :scs (double-sse-reg) :target res)
         (y :scs (double-sse-reg)))
  (:arg-types simd-pack-double simd-pack-double)
  (:results (res :scs (double-sse-reg)))
  (:result-types simd-pack-double)
  (:generator 4
    (cond ((location= res y)
           (inst addpd y x))
          (t
           (move res x)
           (inst addpd res y)))))

(define-vop (cl3a.mmmult6::f2*)
  (:translate cl3a.mmmult6::f2*)
  (:policy :fast-safe)
  (:args (x :scs (double-sse-reg) :target res)
         (y :scs (double-sse-reg)))
  (:arg-types simd-pack-double simd-pack-double)
  (:results (res :scs (double-sse-reg)))
  (:result-types simd-pack-double)
  (:generator 4
    (cond ((location= res y)
           (inst mulpd y x))
          (t
           (move res x)
           (inst mulpd res y)))))

(define-vop (cl3a.mmmult6::load2-sse-from-array)
  (:translate cl3a.mmmult6::load2-sse-from-array)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :target res)
         (index :scs (any-reg immediate)))
  (:arg-types simple-array-double-float tagged-num)
  (:temporary (:sc double-reg) tmp)
  (:results (res :scs (double-sse-reg) :from (:argument 0)))
  (:result-types simd-pack-double)
  (:generator 7
    (inst movsd res
          (make-ea-for-float-ref object index 0 8
                                 :scale (ash 1 (- word-shift
                                                  n-fixnum-tag-bits))))
    (inst movsd tmp 
          (make-ea-for-float-ref object index 1 8
                                 :scale (ash 1 (- word-shift
                                                  n-fixnum-tag-bits))))
    (inst unpcklpd res tmp)))

(define-vop (cl3a.mmmult6::store2-sse-to-array)
  (:translate cl3a.mmmult6::store2-sse-to-array)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (double-sse-reg)))
  (:arg-types simple-array-double-float tagged-num simd-pack-double)
  (:temporary (:sc double-reg) tmp)
  (:generator 20
    (inst xorpd tmp tmp)
    (move tmp value)
    (inst movsd
          (make-ea-for-float-ref object index 0 8
                                 :scale (ash 1 (- word-shift
                                                  n-fixnum-tag-bits)))
          tmp)
    (inst psrldq tmp 8)
    (inst movsd
          (make-ea-for-float-ref object index 1 8
                                 :scale (ash 1 (- word-shift
                                                  n-fixnum-tag-bits)))
          tmp)))

(in-package :cl3a.mmmult6)

(macrolet ((define-stub (name)
             `(defun ,name (x y)
                (,name x y))))
  (define-stub f2+)
  (define-stub f2*)
  (define-stub load2-sse-from-array))
(defun store2-sse-to-array (m i s)
  (store2-sse-to-array m i s))

(declaim (ftype (function (integer &key (:l1 boolean)) integer)
                tile-size))
(defun tile-size (n &key l1)
  "See Lam et al. 1991 The cache performance and optimizations of blocked algorithms"
  (declare (type integer n)
           (type boolean l1))
  (let* ((n-half (ifloor n 2))
         (cache-size (if l1
                         (ifloor (* +L1-size+ +associativity+) 8)
                         (ifloor (* +L2-size+ +associativity+) 8))))  ;; 1 word = 4 byte
    (declare (type integer n-half cache-size))
    (loop :while t
       :with max-width :of-type integer = (min n cache-size)
       :and addr :of-type integer = n-half
       :and di :of-type integer = 0
       :and dj :of-type integer = 1
       :and di0 :of-type integer = 1
       :do (progn
             (incf addr cache-size)
             (setf di (ifloor addr n))
             (setf dj (abs (- (mod addr n) n-half)))
             (setf di0 (min max-width dj)))
       (when (>= di di0)
         (return (min max-width di)))
       :do (setf max-width di0))))


(defmacro m*m-ker (val-type si ni sk nk nra nv ncb ma mb mc)
  "Multiply matrix and matrix (unrolling version)"
  (with-gensyms (iend jend0 kend i j k maik maiksse imb imc)
    `(let* ((,iend (min ,nra (the fixnum (+ ,si ,ni))))
            (,jend0 (min-factor ,ncb +unroll+ 2))  ; 2 for sse
            (,kend (min ,nv (the fixnum (+ ,sk ,nk)))))
       (declare (type fixnum ,iend ,jend0 ,kend))
       (do ((,i ,si (1+ ,i)))
           ((>= ,i ,iend))
         (do ((,k ,sk (1+ ,k)))
             ((>= ,k ,kend))
           (let* ((,maik (aref ,ma ,i ,k))
                  (,maiksse (%make-simd-pack-double ,maik ,maik))
                  (,imb (array-row-major-index ,mb ,k 0))
                  (,imc (array-row-major-index ,mc ,i 0)))
             (declare (type ,val-type ,maik)
                      (type simd-pack-double ,maiksse)
                      (type fixnum ,imb ,imc))
             (do ((,j 0 (+ ,j (the fixnum (* +unroll+ 2)))))
                 ((>= ,j ,jend0) ,j)
               ,@(loop :repeat +unroll+
                    :with form =
                    `((store2-sse-to-array
                       (sb-kernel:%array-data-vector ,mc) ,imc
                       (f2+ (load2-sse-from-array
                             (sb-kernel:%array-data-vector ,mc) ,imc)
                            (f2* ,maiksse
                                 (load2-sse-from-array
                                  (sb-kernel:%array-data-vector ,mb) ,imb))))
                      (incf ,imb 2)
                      (incf ,imc 2))
                    :append form))
             ;; if jend < +unroll+ or (mod jend +unroll+) > 0
             (do ((,j ,jend0 (+ ,j 2)))
                 ((>= ,j ,ncb))
               (cond
                 ((> (+ ,j 2) ,ncb)  ; if j is odd and loop is the last
                  (incf (row-major-aref ,mc ,imc)
                        (* ,maik (row-major-aref ,mb ,imb)))
                  (incf ,imb)
                  (incf ,imc))
                 (t
                  (store2-sse-to-array
                   (sb-kernel:%array-data-vector ,mc) ,imc
                   (f2+ (load2-sse-from-array
                         (sb-kernel:%array-data-vector ,mc) ,imc)
                        (f2* ,maiksse
                             (load2-sse-from-array
                              (sb-kernel:%array-data-vector ,mb) ,imb))))
                  (incf ,imb 2)
                  (incf ,imc 2))))))))))



(defmacro m*m (val-type ma mb mc)
  ;; (with-gensyms (calc nra nca nrb ncb nv nt m i k)
  (with-gensyms (calc nra nca nrb ncb nv m1 m2 i k)
    `(flet ((,calc (si ni sk nk nra nv ncb ma mb mc)
              (declare (optimize (speed 3) (debug 0) (safety 0))
                       (type fixnum si ni sk nk nra nv ncb)
                       (type (simple-array ,val-type (* *)) ma mb mc))
              (m*m-ker ,val-type si ni sk nk nra nv ncb ma mb mc)))
       (declare (inline ,calc))
       (let* ((,nra (array-dimension ,ma 0))
              (,nca (array-dimension ,ma 1))
              (,nrb (array-dimension ,mb 0))
              (,ncb (array-dimension ,mb 1))
              (,nv (cond ((/= ,nca ,nrb)
                          (different-length-warn ,nca ,nrb)
                          (min ,nca ,nrb))
                         (t ,nca)))
              ;; (,nt (min ,nra ,ncb))
              ;; (,m (tile-size ,nt))
              (,m1 (tile-size ,nra))
              (,m2 (tile-size ,ncb :l1 t)))
         ;; (declare (type fixnum ,nra ,nca ,nrb ,ncb ,nv ,nt ,m))
         ;; (dotimes-interval (,i ,m ,nra)
         ;;   (dotimes-interval (,k ,m ,nv)
         ;;     (,calc ,i ,m ,k ,m ,nra ,nv ,ncb ,ma ,mb ,mc)))))))
         (declare (type fixnum ,nra ,nca ,nrb ,ncb ,nv ,m1 ,m2))
         (dotimes-interval (,i ,m1 ,nra)
           (dotimes-interval (,k ,m2 ,nv)
             (,calc ,i ,m1 ,k ,m2 ,nra ,nv ,ncb ,ma ,mb ,mc)))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (* *))
                           (simple-array double-float (* *))))
                dm*m))
(defun dm*m (ma mb mc)
  "Multiply matrix and matrix of double-float"
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) ma mb mc))
  (m*m double-float ma mb mc))
