(in-package :cl-user)
(defpackage cl3a-test
  (:use :cl :prove :cl3a))
(in-package #:cl3a-test)


(plan 1)


(defun ddotprod-test ()
  (let ((va (make-array (list 6)
                        :element-type 'double-float
                        :initial-contents
                        '(-0.0811028018084892d0 -1.71204566380857d0
                          -0.0494652029771625d0 -0.0964655789216564d0
                          -0.854559547720318d0 -1.29799466342263d0)))
        (vb (make-array (list 6)
                        :element-type 'double-float
                        :initial-contents
                        '(-1.21286010812897d0 1.10526916110367d0
                          1.70130308426156d0 -0.583769438010967d0
                          1.17720685549422d0 -0.0536449035935699d0))))
    (coerce (cl3a:ddotprod va vb) 'single-float)))

(is (ddotprod-test) -2.75810912665216)


(finalize)
