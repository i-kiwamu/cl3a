(in-package :cl-user)
(defpackage cl3a-asd
  (:use :cl :asdf))
(in-package :cl3a-asd)

(defsystem "cl3a"
    :description "Common Lisp Library of Linear Algebra"
    :version "0.1"
    :author "Kiwamu Ishikura"
    :license "GPL"
    :depends-on (:trivial-types)
    :components ((:module "src"
                  :components
                  ((:file "cl3a" :depends-on ("ddotprod" "dnorm"))
                   (:file "utilities")
                   (:file "ddotprod" :depends-on ("utilities"))
                   (:file "dnorm" :depends-on ("utilities" "ddotprod")))))
    :in-order-to ((test-op (load-op cl3a-test))))
