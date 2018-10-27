(in-package :cl-user)
(defpackage cl3a.mvmult
  ;; (:use :cl :sb-ext :sb-c :alexandria :cl3a.utilities)
  (:use :cl :sb-ext :sb-c :alexandria :cl3a.utilities :cl3a.dotprod_vop :cl3a.mvmult_vop)
  (:export :sm*v :dm*v))
(in-package :cl3a.mvmult)


;; (defmacro m*v-ker (val-type si ni nr nc ma vb vc)
;;   "Multiply matrix and vector"
;;   (let* (;; (unroll +unroll+)  ;; slow
;;          (unroll 3)
;;          (ima-list (make-gensym-list unroll "ima"))
;;          (ivb-list (make-gensym-list unroll "ivb")))
;;     (declare (type fixnum unroll))
;;     (with-gensyms (iend jend0 i j vci)
;;       `(let ((,iend (min ,nr (the fixnum (+ ,si ,ni))))
;;              (,jend0 (min-factor ,nc ,unroll)))
;;          (declare (type fixnum ,iend ,jend0))
;;          (do ((,i ,si (1+ ,i)))
;;              ((>= ,i ,iend))
;;            (let (,@(loop :for ui :below unroll
;;                       :for imai :in ima-list
;;                       :for ivbi :in ivb-list
;;                       :append `((,imai (array-row-major-index ,ma ,i ,ui))
;;                                 (,ivbi (array-row-major-index ,vb ,ui))))
;;                  (,vci (coerce 0.0 ',val-type)))
;;              (declare (type ,val-type ,vci))
;;              (do ((,j 0 (the fixnum (+ ,j ,unroll))))
;;                  ((>= (the fixnum ,j) ,jend0))
;;                (incf ,vci
;;                      (+ ,@(loop :for imai :in ima-list
;;                              :for ivbi :in ivb-list
;;                              :append `((* (row-major-aref ,ma ,imai)
;;                                           (row-major-aref ,vb ,ivbi))))))
;;                ,@(loop :for imai :in ima-list
;;                     :for ivbi :in ivb-list
;;                     :append `((incf (the fixnum ,imai) ,unroll)
;;                               (incf (the fixnum ,ivbi) ,unroll))))
;;              ;; if nc < unroll or (mod nc unroll) > 0
;;              (do ((,j ,jend0 (1+ ,j)))
;;                  ((>= ,j ,nc))
;;                (incf ,vci
;;                      (* (row-major-aref ,ma ,(nth 0 ima-list))
;;                         (row-major-aref ,vb ,(nth 0 ivb-list))))
;;                (incf ,(nth 0 ima-list))
;;                (incf ,(nth 0 ivb-list)))
;;              (incf (aref ,vc ,i) ,vci)))))))


;; (defmacro m*v (val-type ma vb vc)
;;   (with-gensyms (calc nra nca nb nj tbl m i)
;;     `(flet ((,calc (si ni nr nc ma vb vc)
;;               (declare (optimize (speed 3) (debug 0) (safety 0))
;;                        (type fixnum si ni nr nc)
;;                        (type (simple-array ,val-type (* *)) ma)
;;                        (type (simple-array ,val-type (*)) vb vc))
;;               (m*v-ker ,val-type si ni nr nc ma vb vc)))
;;        (declare (inline ,calc))
;;        (let* ((,nra (array-dimension ,ma 0))
;;               (,nca (array-dimension ,ma 1))
;;               (,nb (length ,vb))
;;               (,nj (cond ((/= ,nca ,nb) (different-length-warn ,nca ,nb)
;;                                         (min ,nca ,nb))
;;                          (t ,nca)))
;;               (,tbl (type-byte-length ',val-type))
;;               (,m (isqrt (ifloor +L2-size+ ,tbl))))
;;          (declare (type fixnum ,nra ,nca ,nb ,nj ,tbl ,m))
;;          (dotimes-interval (,i ,m ,nra)
;;            (,calc ,i ,m ,nra ,nj ,ma ,vb ,vc))))))


(declaim (ftype (function ((simple-array double-float (* *))
                           (simple-array double-float (*))
                           (simple-array double-float (*))))
                dm*v))
(defun dm*v (matA vb vc)
  "Multiply matrix and vector of double-float"
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) matA)
           (type (simple-array double-float (*)) vb vc))
  ;; (m*v double-float matA vb vc))
  (let* ((m (array-dimension matA 0))
         (k (array-dimension matA 1))
         (iend0 (min-factor m 2))
         (pend0 (min-factor k 4))
         (va0 (make-array pend0 :element-type 'double-float))
         (va1 (make-array pend0 :element-type 'double-float)))
    (declare (type fixnum m k iend0 pend0)
             (type (simple-array double-float (*)) va0 va1))
    (do ((i0 0 (+ i0 2))
         (i1 1 (+ i1 2)))
        ((>= i0 iend0))
      (declare (type fixnum i0 i1))
      (sb-sys:%primitive
       take-row-vector-pd (sb-kernel:%array-data matA)
                          (array-row-major-index matA i0 0) pend0 va0)
      (sb-sys:%primitive
       take-row-vector-pd (sb-kernel:%array-data matA)
                          (array-row-major-index matA i1 0) pend0 va1)
      ;; (let ((rmp0 (array-row-major-index matA i0 0))
      ;;       (rmp1 (array-row-major-index matA i1 0)))
      ;;   (declare (type fixnum rmp0 rmp1))
      ;;   (dotimes (p pend0)
      ;;     (setf (row-major-aref va0 p) (row-major-aref matA (+ rmp0 p)))
      ;;     (setf (row-major-aref va1 p) (row-major-aref matA (+ rmp1 p)))))
      (sb-sys:%primitive
       aset-pd vc i0
              (f2+-pd (mvi2x4-pd pend0 va0 va1 vb (aref-pd vc i0))
                      (loop :for p :of-type fixnum :from pend0 :below k
                         :sum (* (aref matA i0 p) (aref vb p))
                         :into a0 :of-type double-float
                         :sum (* (aref matA i1 p) (aref vb p))
                         :into a1 :of-type double-float
                         :finally
                         (return (sb-kernel:%make-simd-pack-double a0 a1))))))
    (when (oddp m)
      (incf (aref vc iend0)
            (loop :for p :of-type fixnum :below k
               :sum (* (aref matA iend0 p) (aref vb p))
               :into a :of-type double-float
               :finally (return a))))))


(declaim (ftype (function ((simple-array single-float (* *))
                           (simple-array single-float (*))
                           (simple-array single-float (*))))
                sm*v))
;; (defun sm*v (ma vb vc)
;;   "Multiply matrix and vector of long-float"
;;   (declare (optimize (speed 3))
;;            (type (simple-array long-float (* *)) ma)
;;            (type (simple-array long-float (*)) vb vc))
;;   (m*v long-float ma vb vc))
