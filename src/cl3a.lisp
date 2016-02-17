(in-package :cl-user)
(defpackage :cl3a
  (:use :cl)
  (:import-from :cl3a.ddotprod
                :ddotprod)
  (:export :ddotprod))
(in-package :cl3a)
