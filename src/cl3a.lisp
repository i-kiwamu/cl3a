(in-package :cl-user)
(defpackage :cl3a
  (:use :cl)
  (:import-from :cl3a.ddotprod
                :dv*v :dv*v-ker)
  (:export :dv*v))
(in-package :cl3a)
