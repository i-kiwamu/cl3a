(in-package :cl-user)
(defpackage cl3a-asd
  (:use :cl :asdf))
(in-package :cl3a-asd)

(defsystem "cl3a"
    :description "Common Lisp Library of Linear Algebra"
    :version "0.1"
    :author "Kiwamu Ishikura"
    :license "GPL"
    :depends-on (:alexandria)
    :components ((:module "src"
                  :components
                  ((:file "cl3a"
                    :depends-on ("dotprod" "norm" "add_vector" "rotate"
                                 "mvmult" "mmmult_Goto" "transpose"))
                   (:file "transpose")
                   (:file "utilities")
                   (:file "dotprod"
                          :depends-on ("utilities"))
                   (:file "norm"
                          :depends-on ("utilities" "dotprod"))
                   (:file "add_vector"
                          :depends-on ("utilities"))
                   (:file "rotate"
                          :depends-on ("utilities"))
                   (:file "mvmult"
                          :depends-on ("utilities"))
                   (:file "mmmult7_vop")
                   (:file "mmmult_Goto"
                          :depends-on ("utilities" "mmmult7_vop")))))
    :in-order-to ((test-op (load-op cl3a-test))))
