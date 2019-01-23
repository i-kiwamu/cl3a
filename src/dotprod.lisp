(in-package :cl-user)
(defpackage cl3a.dotprod
  (:use :cl :sb-ext :sb-c :alexandria :cl3a.utilities :cl3a.utilities_vop :cl3a.dotprod_vop)
  (:export :sv*v :dv*v))
(in-package :cl3a.dotprod)


(declaim (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*)))
                          double-float)
                dv*v))
(defun dv*v (va vb)
  "Dot product with two double-float vectors va and vb"
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (*)) va vb))
  (let* ((n (min (length va) (length vb)))
         (n0 (min-factor n +unroll+))
         ;; (res-pd (setzero2-pd))
         (res-pd (setzero4-pd))
         (res 0d0))
    (declare (type fixnum n n0)
             ;; (type (simd-pack double-float) res-pd)
             (type (simd-pack-256 double-float) res-pd)
             (type double-float res))
    (do ((i 0 (+ i +unroll+)))
        ((>= i n0))
      (setf res-pd
            ;; (f2+-pd res-pd (dpi8-pd va vb i))))
            (f4+-pd res-pd (dpi8-avx2-pd va vb i))))
    ;; (multiple-value-bind (lo hi)
    ;;     (%simd-pack-doubles res-pd)
    ;;   (declare (type double-float lo hi))
    ;;   (incf res (+ lo hi)))
    (multiple-value-bind (r0 r1 r2 r3)
        (%simd-pack-256-doubles res-pd)
      (declare (type double-float r0 r1 r2 r3))
      (incf res (+ r0 r1 r2 r3)))
    (do ((i n0 (1+ i)))
        ((>= i n) res)
      (incf res (* (aref va i) (aref vb i))))))


(declaim (ftype (function ((simple-array single-float (*))
                           (simple-array single-float (*)))
                          single-float)
                sv*v))
(defun sv*v (va vb)
  "Dot product with two single-float vectors va and vb"
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array single-float (*)) va vb))
  (let* ((n (min (length va) (length vb)))
         (n0 (min-factor n +unroll+))
         ;; (res-ps (setzero4-ps))
         (res-ps (setzero8-ps))
         (res 0s0))
    (declare (type fixnum n n0)
             ;; (type (simd-pack single-float) res-ps)
             (type (simd-pack-256 single-float) res-ps)
             (type single-float res))
    (do ((i 0 (+ i +unroll+)))
        ((>= i n0))
      (setf res-ps
            ;; (f4+-ps res-ps (dpi8-ps va vb i))))
            (f8+-ps res-ps (dpi8-avx2-ps va vb i))))
    ;; (multiple-value-bind (r0 r1 r2 r3)
    ;;     (%simd-pack-singles res-ps)
    ;;   (declare (type single-float r0 r1 r2 r3))
    ;;   (incf res (+ r0 r1 r2 r3)))
    (multiple-value-bind (r0 r1 r2 r3 r4 r5 r6 r7)
        (%simd-pack-256-singles res-ps)
      (declare (type single-float r0 r1 r2 r3 r4 r5 r6 r7))
      (incf res (+ r0 r1 r2 r3 r4 r5 r6 r7)))
    (do ((i n0 (1+ i)))
        ((>= i n) res)
      (incf res (* (aref va i) (aref vb i))))))
