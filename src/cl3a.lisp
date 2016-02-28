(in-package :cl-user)
(defpackage :cl3a
  (:use :cl)
  (:import-from :cl3a.ddotprod
                :dv*v)
  (:import-from :cl3a.dnorm
                :dnorm)
  (:export :dv*v :dnorm))
(in-package :cl3a)
