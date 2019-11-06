(in-package :cl-user)
(defpackage cl3a-test
  (:use :cl :prove :cl3a :naive))
(in-package #:cl3a-test)


(defun equal-vector (va vb)
  (let* ((na (length va))
         (nb (length vb))
         (n (min na nb))
         (test (= na nb)))
    (declare (type integer na nb n)
             (type boolean test))
    (when test
      (block check
        (dotimes (i n)
          (let ((a (coerce (aref va i) 'single-float))
                (b (coerce (aref vb i) 'single-float)))
            (when (/= a b)
              (setf test nil)
              (return-from check))))))
    test))


(defun equal-matrix (ma mb)
  (let* ((nra (array-dimension ma 0))
         (nrb (array-dimension mb 0))
         (nca (array-dimension ma 1))
         (ncb (array-dimension mb 1))
         (nr (min nra nrb))
         (nc (min nca ncb))
         (test (and (= nra nrb) (= nca ncb))))
    (declare (type integer nra nrb nca ncb nr nc)
             (type boolean test))
    (when test
      (block check
        (dotimes (i nr)
          (dotimes (j nc)
            (let ((a (coerce (aref ma i j) 'single-float))
                  (b (coerce (aref mb i j) 'single-float)))
              (when (/= a b)
              ;; (when (> (abs (- a b)) 1d-6)
                (setf test nil)
                (return-from check)))))))
    test))


(defun ddotprod-test (n)
  (declare (type integer n))
  (let ((va (make-array n :element-type 'double-float
                        :initial-element 0d0))
        (vb (make-array n :element-type 'double-float
                        :initial-element 0d0)))
    (declare (type (simple-array double-float (*)) va vb))
    (dotimes (i n)
      (setf (aref va i) (random 1d0))
      (setf (aref vb i) (random 1d0)))
    (let ((res1 (coerce (dv*v va vb) 'single-float))
          (res2 (coerce (dv*v-naive va vb) 'single-float)))
      (diag "Test for dv*v")
      (is res1 res2))))


(defun dnorm-test (n)
  (declare (type integer n))
  (let ((va (make-array n :element-type 'double-float
                        :initial-element 0d0)))
    (declare (type (simple-array double-float (*)) va))
    (dotimes (i n)
      (setf (aref va i) (random 1d0)))
    (let ((res1 (coerce (dnorm va) 'single-float))
          (res2 (coerce (dnorm-naive va) 'single-float)))
      (diag "Test for dnorm")
      (is res1 res2))))


(defun dadd-test (n)
  (declare (type integer n))
  (let ((va (make-array n :element-type 'double-float
                        :initial-element 0d0))
        (vb (make-array n :element-type 'double-float
                        :initial-element 0d0))
        (a (random 1d0))
        (b (random 1d0))
        (res1 (make-array n :element-type 'double-float
                          :initial-element 0d0))
        (res2 (make-array n :element-type 'double-float
                          :initial-element 0d0)))
    (declare (type (simple-array double-float (*)) va vb res1 res2)
             (type double-float a b))
    (dotimes (i n)
      (setf (aref va i) (random 1d0))
      (setf (aref vb i) (random 1d0)))
    (dv+v a va b vb res1)
    (dv+v-naive a va b vb res2)
    (diag "Test for dv+v")
    (is res1 res2 :test #'equal-vector)))


(defun drotate-test (n)
  (declare (type integer n))
  (let* ((va (make-array n :element-type 'double-float
                         :initial-element 0d0))
         (vb (make-array n :element-type 'double-float
                         :initial-element 0d0))
         (theta (coerce (random (* 2d0 pi)) 'double-float))
         (c (cos theta))
         (s (sin theta))
         (res1c (make-array n :element-type 'double-float
                            :initial-element 0d0))
         (res1d (make-array n :element-type 'double-float
                            :initial-element 0d0))
         (res2c (make-array n :element-type 'double-float
                            :initial-element 0d0))
         (res2d (make-array n :element-type 'double-float
                            :initial-element 0d0)))
    (declare (type (simple-array double-float (*)) va vb res1c res1d res2c res2d)
             (type double-float theta)
             (type (double-float -1d0 1d0) c s))
    (dotimes (i n)
      (setf (aref va i) (random 1d0))
      (setf (aref vb i) (random 1d0)))
    (drotate va vb c s res1c res1d)
    (drotate-naive va vb c s res2c res2d)
    (subtest "Test for drotate"
      (is res1c res2c :test #'equal-vector)
      (is res1d res2d :test #'equal-vector))))


(defun dm*v-test (n)
  (declare (type integer n))
  (let ((ma (make-array (list n n) :element-type 'double-float
                        :initial-element 0d0))
        (vb (make-array n :element-type 'double-float
                        :initial-element 0d0))
        (res1 (make-array n :element-type 'double-float
                          :initial-element 0d0))
        (res2 (make-array n :element-type 'double-float
                          :initial-element 0d0)))
    (declare (type (simple-array double-float (* *)) ma)
             (type (simple-array double-float (*)) vb res1 res2))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref ma i j) (random 1d0)))
      (setf (aref vb i) (random 1d0)))
    (dm*v ma vb res1)
    (dm*v-naive ma vb res2)
    (diag "Test for dm*v")
    (is res1 res2 :test #'equal-vector)))


(defun dm*m-test (n)
  (declare (type integer n))
  (let ((ma (make-array (list n n) :element-type 'double-float
                        :initial-element 0d0))
        (mb (make-array (list n n) :element-type 'double-float
                        :initial-element 0d0))
        (res1 (make-array (list n n) :element-type 'double-float
                          :initial-element 0d0))
        (res2 (make-array (list n n) :element-type 'double-float
                          :initial-element 0d0)))
    (declare (type (simple-array double-float (* *)) ma mb res1 res2))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref ma i j) (random 1d0))
        (setf (aref mb i j) (random 1d0))))
    (dm*m ma mb res1)
    (dm*m-naive ma mb res2)
    (diag "Test for dm*m")
    (is res1 res2 :test #'equal-matrix)))


(plan 6)  ;; use prime number
(ddotprod-test 4999)
(dnorm-test 4999)
(dadd-test 4999)
(drotate-test 4999)
(dm*v-test 499)
(dm*m-test 521)
;; (dm*m-test 1031)
(finalize)
