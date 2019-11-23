(in-package :cl-user)
(defpackage cl3a.typedef
  (:use :cl :sb-ext :sb-c :sb-vm)
  (:export :positive-byte
           :%vec :vec
           :make-vec-init :convert-simple-array-to-vec
           :eltype-of
           :%vecref :vecref
           :%mat :mat
           :make-mat-init :convert-simple-array-to-mat
           :%nrow :nrow
           :%ncol :ncol
           :%row-major-matref :row-major-matref
           :%matref :matref))
(in-package :cl3a.typedef)

(deftype positive-byte (bits)
  (if (eql bits '*)
      '(integer 1 *)
      `(integer 1 ,(1- (ash 1 bits)))))

(defun simple-float-vector-p (x)
  (or (typep x '(simple-array single-float (*)))
      (typep x '(simple-array double-float (*)))
      (typep x '(simple-array long-float (*)))))
(deftype simple-float-vector ()
  '(satisfies simple-float-vector-p))

(defun simple-float-matrix-p (x)
  (or (typep x '(simple-array single-float (* *)))
      (typep x '(simple-array double-float (* *)))
      (typep x '(simple-array long-float (* *)))))
(deftype simple-float-matrix ()
  '(satisfies simple-float-matrix-p))


;; definition of vec (vector)
(defstruct (%vec (:constructor %make-vec-dont-use (length eltype contents))
                 (:copier nil)
                 (:print-object print-vec))
  (length 1 :type (positive-byte 32)
            :read-only t)
  (eltype 'double-float :type (member single-float double-float long-float)
                        :read-only t)
  (contents (make-array 1 :element-type 'double-float)
            :type simple-float-vector))

(defun print-vec (x stream)
  (print-unreadable-object (x stream)
    (let ((len (%vec-length x)))
      (if (> len 20)
          (format stream
                  "Length: ~D | Element type: ~A~%  (~{~A~^ ~} ...)~%"
                  len (%vec-eltype x)
                  (loop :for i :from 0 :below 20 :collect (aref (%vec-contents x) i)))
          (format stream
                  "Length: ~D | Element type: ~A~%  (~{~A~^ ~})~%"
                  len (%vec-eltype x) (coerce (%vec-contents x) 'list))))))

(defun vec-single-float-p (x)
  (and (typep x '%vec)
       (eql (%vec-eltype x) 'single-float)))

(defun vec-double-float-p (x)
  (and (typep x '%vec)
       (eql (%vec-eltype x) 'double-float)))

(defun vec-long-float-p (x)
  (and (typep x '%vec)
       (eql (%vec-eltype x) 'long-float)))

(deftype vec (&optional eltype)
  (ecase eltype
    (single-float '(satisfies vec-single-float-p))
    (double-float '(satisfies vec-double-float-p))
    (long-float '(satisfies vec-long-float-p))
    (* '(satisfies %vec-p))))

(declaim (inline make-vec-init))
(defun make-vec-init (length eltype)
  (check-type length (positive-byte 32))
  (check-type eltype (member single-float double-float long-float))
  (%make-vec-dont-use length
                      eltype
                      (make-array length :element-type eltype)))

(declaim (inline convert-simple-array-to-vec))
(defun convert-simple-array-to-vec (sfvector)
  (check-type sfvector simple-float-vector)
  (%make-vec-dont-use (length sfvector)
                      (array-element-type sfvector)
                      (copy-seq sfvector)))

(declaim (inline vec-length))
(defun vec-length (x)
  (check-type x %vec)
  (%vec-length x))

(defmethod eltype-of ((x %vec))
  (%vec-eltype x))

(declaim (inline %vecref))
(defun %vecref (x i)
  (declare (type %vec x)
           (type (unsigned-byte 32) i))
  (aref (%vec-contents x) i))

(defun (setf %vecref) (value x i)
  (setf (aref (%vec-contents x) i) value))

(declaim (inline vecref))
(defun vecref (x i)
  (check-type x %vec)
  (check-type i (unsigned-byte 32))
  (%vecref x i))

(defun (setf vecref) (value x i)
  (setf (%vecref x i) value))


;; definition of mat (matrix)
(defstruct (%mat (:constructor %make-mat-dont-use (nrow ncol eltype contents))
                 (:copier nil)
                 (:print-object print-mat))
  (nrow 1 :type (positive-byte 32)
          :read-only t)
  (ncol 1 :type (positive-byte 32)
          :read-only t)
  (eltype 'double-float :type (member single-float double-float long-float)
                        :read-only t)
  (contents (make-array 1 :element-type 'double-float)
            :type simple-float-vector))

(defun print-mat (x stream)
  (print-unreadable-object (x stream)
    (let* ((nrow (%mat-nrow x))
           (ncol (%mat-ncol x))
           (nrow-show (min 5 nrow))
           (rmi 0))
      (format stream
              "Rows: ~D | Cols: ~D | Element type: ~A~%"
              nrow ncol (%mat-eltype x))
      (if (> ncol 10)
          (dotimes (n nrow-show)
            (format stream
                    "  (~{~A~^ ~} ...)~%"
                    (loop :for i :from rmi :below (+ rmi 10)
                       :collect (aref (%mat-contents x) i)))
            (incf rmi ncol))
          (dotimes (n nrow-show)
            (format stream
                    "  (~{~A~^ ~})~%"
                    (loop :for i :from rmi :below (+ rmi ncol)
                       :collect (aref (%mat-contents x) i)))
            (incf rmi ncol))))))

(defun mat-single-float-p (x)
  (and (typep x '%mat)
       (eql (%mat-eltype x) 'single-float)))

(defun mat-double-float-p (x)
  (and (typep x '%mat)
       (eql (%mat-eltype x) 'double-float)))

(defun mat-long-float-p (x)
  (and (typep x '%mat)
       (eql (%mat-eltype x) 'long-float)))

(deftype mat (&optional eltype)
  (ecase eltype
    (single-float '(satisfies mat-single-float-p))
    (double-float '(satisfies mat-double-float-p))
    (long-float '(satisfies mat-long-float-p))
    (* '(satisfies %mat-p))))


(declaim (inline make-mat-init))
(defun make-mat-init (nrow ncol eltype)
  (check-type nrow (positive-byte 32))
  (check-type ncol (positive-byte 32))
  (check-type eltype (member single-float double-float long-float))
  (%make-mat-dont-use nrow ncol eltype
                      (make-array (* nrow ncol) :element-type eltype)))

(declaim (inline convert-simple-array-to-mat))
(defun convert-simple-array-to-mat (sfmatrix)
  (check-type sfmatrix simple-float-matrix)
  (%make-mat-dont-use (array-dimension sfmatrix 0)
                      (array-dimension sfmatrix 1)
                      (array-element-type sfmatrix)
                      (copy-seq (array-storage-vector sfmatrix))))

(declaim (inline %nrow))
(defun %nrow (x)
  (declare (type %mat x))
  (%mat-nrow x))

(declaim (inline nrow))
(defun nrow (x)
  (check-type x %mat)
  (%nrow x))

(declaim (inline %ncol))
(defun %ncol (x)
  (declare (type %mat x))
  (%mat-ncol x))

(declaim (inline ncol))
(defun ncol (x)
  (check-type x %mat)
  (%ncol x))

(defmethod eltype-of ((x %mat))
  (%mat-eltype x))

(declaim (inline %row-major-matref))
(defun %row-major-matref (x index)
  (declare (type %mat x)
           (type (unsigned-byte 32) index))
  (aref (%mat-contents x) index))

(defun (setf %row-major-matref) (value x index)
  (setf (aref (%mat-contents x) index) value))

(declaim (inline row-major-matref))
(defun row-major-matref (x index)
  (check-type x %mat)
  (check-type index (unsigned-byte 32))
  (%row-major-matref x index))

(defun (setf row-major-matref) (value x index)
  (check-type value float)
  (check-type x %mat)
  (check-type index (unsigned-byte 32))
  (setf (%row-major-matref x index) value))

(declaim (inline %matref))
(defun %matref (x i j)
  (declare (type %mat x)
           (type (unsigned-byte 32) i j))
  (let ((index (+ j (* i (%mat-ncol x)))))
    (declare (type (unsigned-byte 32) index))
    (aref (%mat-contents x) index)))

(defun (setf %matref) (value x i j)
  (let ((index (+ j (* i (%mat-ncol x)))))
    (declare (type (unsigned-byte 32) index))
    (setf (aref (%mat-contents x) index) value)))

(declaim (inline matref))
(defun matref (x i j)
  (check-type x %mat)
  (check-type i (unsigned-byte 32))
  (check-type j (unsigned-byte 32))
  (%matref x i j))

(defun (setf matref) (value x i j)
  (check-type value float)
  (check-type x %mat)
  (check-type i (unsigned-byte 32))
  (check-type j (unsigned-byte 32))
  (setf (%matref x i j) value))
