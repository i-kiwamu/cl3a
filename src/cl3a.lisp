(in-package :cl-user)
(defpackage :cl3a
  (:use :cl)
  (:import-from :cl3a.ddotprod
                :dv*v)
  (:export :dv*v))
(in-package :cl3a)
