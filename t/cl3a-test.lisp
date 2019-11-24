(in-package :cl-user)
(defpackage cl3a-test
  (:use :cl :rove :cl3a :naive))
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


(deftest dotprod-test
    (defun ddotprod-test (n)
      (declare (type integer n))
      (let ((va (make-vec-init n 'double-float))
            (vb (make-vec-init n 'double-float)))
        (declare (type (vec double-float) va vb))
        (dotimes (i n)
          (setf (vecref va i) (random 1d0))
          (setf (vecref vb i) (random 1d0)))
        (values
         (coerce (dv*v va vb) 'single-float)
         (coerce (dv*v-naive va vb) 'single-float))))
  (testing "dot product"
    (multiple-value-bind (res1 res2)
        (ddotprod-test 4999)
      (ok (= res1 res2)))))


(defun sdotprod-test (n)
  (declare (type integer n))
  (let ((va (make-array n :element-type 'single-float))
        (vb (make-array n :element-type 'single-float)))
    (declare (type (simple-array single-float (*)) va vb))
    (dotimes (i n)
      (setf (aref va i) (random 1.0))
      (setf (aref vb i) (random 1.0)))
    (values
     (sv*v va vb)
     (sv*v-naive va vb))))


(deftest norm-test
    (defun dnorm-test (n)
      (declare (type integer n))
      (let ((va (make-vec-init n 'double-float)))
        (declare (type (vec double-float) va))
        (dotimes (i n)
          (setf (vecref va i) (random 1d0)))
        (values
         (coerce (dnorm va) 'single-float)
         (coerce (dnorm-naive va) 'single-float))))
  (testing "normalized vector"
    (multiple-value-bind (res1 res2)
        (dnorm-test 4999)
      (ok (= res1 res2)))))


(deftest add-test
    (defun dadd-test (n)
      (declare (type integer n))
      (let ((va (make-array n :element-type 'double-float))
            (vb (make-array n :element-type 'double-float))
            (a (random 1d0))
            (b (random 1d0))
            (res1 (make-array n :element-type 'double-float))
            (res2 (make-array n :element-type 'double-float)))
        (declare (type (simple-array double-float (*)) va vb res1 res2)
                 (type double-float a b))
        (dotimes (i n)
          (setf (aref va i) (random 1d0))
          (setf (aref vb i) (random 1d0)))
        (values
         (dv+v a va b vb res1)
         (dv+v-naive a va b vb res2))))
  (testing "adding two vector"
    (multiple-value-bind (res1 res2)
        (dadd-test 4999)
      (ok (equal-vector res1 res2)))))


(deftest rotate-test
    (defun drotate-test (n)
      (declare (type integer n))
      (let* ((va (make-array n :element-type 'double-float))
             (vb (make-array n :element-type 'double-float))
             (theta (coerce (random (* 2d0 pi)) 'double-float))
             (c (cos theta))
             (s (sin theta))
             (res1c (make-array n :element-type 'double-float))
             (res1d (make-array n :element-type 'double-float))
             (res2c (make-array n :element-type 'double-float))
             (res2d (make-array n :element-type 'double-float)))
        (declare (type (simple-array double-float (*)) va vb res1c res1d res2c res2d)
                 (type double-float theta)
                 (type (double-float -1d0 1d0) c s))
        (dotimes (i n)
          (setf (aref va i) (random 1d0))
          (setf (aref vb i) (random 1d0)))
        (drotate va vb c s res1c res1d)
        (drotate-naive va vb c s res2c res2d)
        (values res1c res2c res1d res2d)))
  (testing "rotating two vectors"
    (multiple-value-bind (res1c res2c res1d res2d)
        (drotate-test 4999)
      (ok (equal-vector res1c res2c))
      (ok (equal-vector res1d res2d)))))


(deftest m*v-test
    (defun dm*v-test (n)
      (declare (type integer n))
      (let ((ma (make-array (list n n) :element-type 'double-float))
            (vb (make-array n :element-type 'double-float))
            (res1 (make-array n :element-type 'double-float))
            (res2 (make-array n :element-type 'double-float)))
        (declare (type (simple-array double-float (* *)) ma)
                 (type (simple-array double-float (*)) vb res1 res2))
        (dotimes (i n)
          (dotimes (j n)
            (setf (aref ma i j) (random 1d0)))
          (setf (aref vb i) (random 1d0)))
        (dm*v ma vb res1)
        (dm*v-naive ma vb res2)
        (values res1 res2)))
  (testing "matrix-vector multiplication"
    (multiple-value-bind (res1 res2)
        (dm*v-test 499)
      (ok (equal-vector res1 res2)))))


(deftest m*m-test
    (defun dm*m-test (n)
      (declare (type integer n))
      (let ((ma (make-array (list n n) :element-type 'double-float))
            (mb (make-array (list n n) :element-type 'double-float))
            (res1 (make-array (list n n) :element-type 'double-float))
            (res2 (make-array (list n n) :element-type 'double-float)))
        (declare (type (simple-array double-float (* *)) ma mb res1 res2))
        (dotimes (i n)
          (dotimes (j n)
            (setf (aref ma i j) (random 1d0))
            (setf (aref mb i j) (random 1d0))))
        (dm*m ma mb res1)
        (dm*m-naive ma mb res2)
        (values res1 res2)))
  (testing "matrix-matrix multiplication"
    (multiple-value-bind (res1 res2)
        (dm*m-test 3)
      (ok (equal-matrix res1 res2)))
    (multiple-value-bind (res1 res2)
        (dm*m-test 17)
      (ok (equal-matrix res1 res2)))
    (multiple-value-bind (res1 res2)
        (dm*m-test 1031)
      (ok (equal-matrix res1 res2)))))
