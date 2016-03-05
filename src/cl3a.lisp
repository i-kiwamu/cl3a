(in-package :cl-user)
(defpackage :cl3a
  (:use :cl)
  (:import-from :cl3a.dotprod
                :dv*v)
  (:import-from :cl3a.norm
                :dnorm)
  (:export :dv*v :dnorm))
(in-package :cl3a)
