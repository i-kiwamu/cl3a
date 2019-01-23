(in-package :cl-user)
(defpackage :cl3a
  (:use :cl :cl3a.dotprod :cl3a.norm :cl3a.add-vector :cl3a.rotate
        :cl3a.mvmult :cl3a.mmmult_Goto2 :cl3a.transpose)
  (:export :sv*v :dv*v
           :dnorm
           :dv+v
           :drotate
           :sm*v :dm*v
           :dm*m :gemm1 :gemm3
           :dtpose))
(in-package :cl3a)
