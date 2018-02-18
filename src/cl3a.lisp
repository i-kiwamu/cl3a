(in-package :cl-user)
(defpackage :cl3a
  (:use :cl :cl3a.dotprod :cl3a.norm :cl3a.add-vector :cl3a.rotate
        :cl3a.mvmult :cl3a.mmmult7 :cl3a.transpose)
  (:export :dv*v :lv*v
           :dnorm :lnorm
           :dv+v :lv+v
           :drotate :lrotate
           :dm*v :lm*v
           :dm*m :gemm1 :gemm3
           :dtpose :ltpose))
(in-package :cl3a)
