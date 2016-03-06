(in-package :cl-user)
(defpackage :cl3a
  (:use :cl)
  (:import-from :cl3a.dotprod
                :dv*v :lv*v)
  (:import-from :cl3a.norm
                :dnorm :lnorm)
  (:import-from :cl3a.add-vector
                :dv+v :lv+v)
  (:export :dv*v :lv*v
           :dnorm :lnorm
           :dv+v :lv+v))
(in-package :cl3a)
