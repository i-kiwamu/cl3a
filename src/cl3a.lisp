(in-package :cl-user)
(defpackage :cl3a
  (:use :cl :cl3a.dotprod :cl3a.norm :cl3a.add-vector :cl3a.rotate
        :cl3a.mvmult :cl3a.mmmult)
  (:export :dv*v :lv*v
           :dnorm :lnorm
           :dv+v :lv+v
           :drotate :lrotate
           :dm*v :lm*v
           :dm*m :lm*m))
(in-package :cl3a)
