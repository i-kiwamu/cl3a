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
                   (:file "dotprod_vop")
                   (:file "dotprod"
                          :depends-on ("utilities" "dotprod_vop"))
                   (:file "norm"
                          :depends-on ("utilities" "dotprod"))
                   (:file "add_vector"
                          :depends-on ("utilities"))
                   (:file "rotate"
                          :depends-on ("utilities"))
                   (:file "mvmult"
                          :depends-on ("utilities"))
                   ;; (:file "mmmult7_vop")
                   (:file "mmmult_Goto"
                          :depends-on ("utilities")))))
    :in-order-to ((test-op (test-op "cl3a/test"))))
