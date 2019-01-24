(in-package :cl-user)
(defpackage cl3a.rotate
  (:use :cl :alexandria :cl3a.utilities :cl3a.utilities_vop)
  (:export :srotate :drotate))
(in-package :cl3a.rotate)


(declaim (ftype (function ((simple-array double-float (*))
                           (simple-array double-float (*))
                           (double-float -1d0 1d0)
                           (double-float -1d0 1d0)
                           (simple-array double-float (*))
                           (simple-array double-float (*))))
                drotate))
(defun drotate (va vb c s vc vd)
  "Rotate two double-float vectors (va & vb) with cos and sin,
   and return new two vectors"
  (declare (optimize (speed 3) (safety 0))
           (type double-float c s)
           (type (simple-array double-float (*)) va vb vc vd))
  (let* ((na (length va))
         (nb (length vb))
         (nv (cond ((/= na nb) (different-length-warn na nb)
                               (min na nb))
                   (t na)))
         (nv0 (min-factor nv 4))
         (minus-s (* s -1d0)))
    (declare (type fixnum na nb nv nv0)
             (type double-float minus-s))
    (do ((i 0 (+ i 4)))
        ((>= i nv0))
      (sb-sys:%primitive
       aset4-pd vc i
                (f4+-pd (f4*-sd c (aref4-pd va i))
                        (f4*-sd s (aref4-pd vb i))))
      (sb-sys:%primitive
       aset4-pd vd i
                (f4+-pd (f4*-sd c (aref4-pd vb i))
                        (f4*-sd minus-s (aref4-pd va i)))))
    (do ((i nv0 (1+ i)))
        ((>= i nv))
      (setf (aref vc i)
            (+ (* c (aref va i))
               (* s (aref vb i))))
      (setf (aref vd i)
            (- (* c (aref vb i))
               (* s (aref va i)))))))


(declaim (ftype (function ((simple-array single-float (*))
                           (simple-array single-float (*))
                           (single-float -1.0 1.0)
                           (single-float -1.0 1.0)
                           (simple-array single-float (*))
                           (simple-array single-float (*))))
                srotate))
(defun srotate (va vb c s vc vd)
  "Rotate two single-float vectors (va & vb) with cos and sin,
   and return new two vectors"
  (declare (optimize (speed 3) (safety 0))
           (type single-float c s)
           (type (simple-array single-float (*)) va vb vc vd))
  (let* ((na (length va))
         (nb (length vb))
         (nv (cond ((/= na nb) (different-length-warn na nb)
                               (min na nb))
                   (t na)))
         (nv0 (min-factor nv 8))
         (minus-s (* s -1.0)))
    (declare (type fixnum na nb nv nv0)
             (type single-float minus-s))
    (do ((i 0 (+ i 8)))
        ((>= i nv0))
      (sb-sys:%primitive
       aset8-ps vc i
                (f8+-ps (f8*-ss c (aref8-ps va i))
                        (f8*-ss s (aref8-ps vb i))))
      (sb-sys:%primitive
       aset8-ps vd i
                (f8+-ps (f8*-ss c (aref8-ps vb i))
                        (f8*-ss minus-s (aref8-ps va i)))))
    (do ((i nv0 (1+ i)))
        ((>= i nv))
      (setf (aref vc i)
            (+ (* c (aref va i))
               (* s (aref vb i))))
      (setf (aref vd i)
            (- (* c (aref vb i))
               (* s (aref va i)))))))
