(in-package :cl-user)
(defpackage cl3a.transpose
  (:use :cl :alexandria :cl3a.utilities)
  (:export :dtpose :ltpose))
(in-package :cl3a.transpose)


(defmacro tpose (val-type ma)
  (with-gensyms (nr nc mat start rc inext i next continue tmp)
    `(let ((,nr (array-dimension ,ma 0))
           (,nc (array-dimension ,ma 1)))
       (declare (type fixnum ,nr ,nc))
       (loop :with ,mat :of-type (simple-array ,val-type (* *))
          = (make-array (list ,nc ,nr) :element-type ',val-type)
          :for ,start fixnum :from 0 :below (* ,nr ,nc)
          :do (labels
                  ((,rc (n)
                     (declare (type fixnum n))
                      (let ((r (ifloor n ,nr))
                            (b (the fixnum (* (mod n ,nr) ,nc))))
                        (+ b r)))
                   (,inext (i n)
                     (declare (type fixnum i n))
                     (let ((i2 (1+ i))
                           (n2 (,rc n)))
                       (declare (type fixnum i2 n2))
                       (cond ((<= n2 ,start) (values i2 n2))
                             (t (,inext i2 n2))))))
                (multiple-value-bind (,i ,next)
                    (,inext 0 ,start)
                  (block ,continue
                    (when (or (< ,next ,start) (= ,i 1))
                      (setf (row-major-aref ,mat ,start)
                            (row-major-aref ,ma ,start))
                      (return-from ,continue))
                    (setf ,next ,start)
                    (loop :with ,tmp :of-type ,val-type
                       = (row-major-aref ,ma ,start)
                       :do
                       (setf ,i (,rc ,next))
                       (setf (row-major-aref ,mat ,next)
                             (cond ((= ,i ,start) ,tmp)
                                   (t (row-major-aref ,ma ,i))))
                       (setf ,next ,i)
                       :while (> ,next ,start)))))
          :finally (return ,mat)))))


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
