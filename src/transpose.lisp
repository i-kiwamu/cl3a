(in-package :cl-user)
(defpackage cl3a.transpose
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dtpose :ltpose))
(in-package :cl3a.transpose)


(defmacro tpose (val-type ma)
  (with-gensyms (nr nc nrc mat rc inext rma i next continue tmp)
    `(let* ((,nr (array-dimension ,ma 0))
            (,nc (array-dimension ,ma 1))
            (,nrc (array-total-size ,ma))
            (,mat (make-array (list ,nc ,nr) :element-type ',val-type)))
       (declare (type fixnum ,nr ,nc)
                (type (simple-array ,val-type (* *)) ,mat))
       ;; initialize
       (dotimes (,rma ,nrc)
         (setf (row-major-aref ,mat ,rma)
               (row-major-aref ,ma ,rma)))
       (labels
           ((,rc (n)
              (declare (type fixnum n))
              (let ((b (the fixnum (* (mod n ,nr) ,nc)))
                    (r (ifloor n ,nr)))
                (declare (type fixnum b r))
                (+ b r)))
            (,inext (i n i0)
              (declare (type fixnum i n i0))
              (let ((i2 (1+ i))
                    (n2 (,rc n)))
                (declare (type fixnum i2 n2))
                (if (<= n2 i0) (values i2 n2) (,inext i2 n2 i0)))))
         (dotimes (,rma ,nrc)
           (block ,continue
             (multiple-value-bind (,i ,next)
                 (,inext 0 ,rma ,rma)
               (when (or (< ,next ,rma) (= ,i 1))
                 (return-from ,continue))
               (setf ,next ,rma)
               (loop :with ,tmp :of-type ,val-type
                     = (row-major-aref ,mat ,rma)
                  :do
                  (setf ,i (,rc ,next))
                  (setf (row-major-aref ,mat ,next)
                        (if (= ,i ,rma) ,tmp (row-major-aref ,mat ,i)))
                  (setf ,next ,i)
                  :while (> ,next ,rma))))))
       ,mat)))


(declaim (ftype (function ((simple-array double-float (* *)))
                          (simple-array double-float (* *)))
                dtpose))
(defun dtpose (ma)
  "transpose matrix of double-float"
  (declare (type (simple-array double-float (* *)) ma))
  (tpose double-float ma))


(declaim (ftype (function ((simple-array long-float (* *)))
                          (simple-array long-float (* *)))
                ltpose))
(defun ltpose (ma)
  "transpose matrix of long-float"
  (declare (type (simple-array long-float (* *)) ma))
  (tpose long-float ma))
