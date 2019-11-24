(in-package :cl-user)
(defpackage :cl3a
  (:use :cl :cl3a.typedef :cl3a.dotprod :cl3a.norm :cl3a.add-vector
        :cl3a.rotate :cl3a.mvmult :cl3a.mmmult10)
  (:export :vec
           :make-vec-init :convert-simple-array-to-vec
           :eltype-of
           :vec-length :vecref
           :mat
           :make-mat-init :convert-simple-array-to-mat
           :nrow :ncol
           :row-major-matref :matref
           :sv*v :dv*v
           :snorm :dnorm
           :dv+v
           :srotate :drotate
           :sm*v :dm*v
           :dm*m ; :gemm1 :gemm3
           :dtpose))
(in-package :cl3a)
