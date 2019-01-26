(defsystem "cl3a"
    :description "Common Lisp Library of Linear Algebra"
    :version "0.2"
    :author "Kiwamu Ishikura"
    :license "GPL"
    :depends-on (:alexandria)
    :components ((:module "src"
                  :components
                  ((:file "cl3a"
                    :depends-on ("dotprod" "norm" "add_vector" "rotate"
                                 "mvmult" "mmmult9" "transpose"))
                   (:file "transpose")
                   (:file "utilities_vop")
                   (:file "utilities"
                          :depends-on ("utilities_vop"))
                   (:file "dotprod_vop")
                   (:file "dotprod"
                          :depends-on ("utilities" "utilities_vop" "dotprod_vop"))
                   (:file "norm"
                          :depends-on ("utilities" "dotprod"))
                   (:file "add_vector"
                          :depends-on ("utilities" "utilities_vop"))
                   (:file "rotate"
                          :depends-on ("utilities" "utilities_vop"))
                   (:file "mvmult_vop")
                   (:file "mvmult"
                          :depends-on ("utilities" "utilities_vop" "mvmult_vop"))
                   (:file "mmmult9_vop")
                   (:file "mmmult9"
                          :depends-on ("utilities" "mmmult9_vop")))))
    :in-order-to ((test-op (load-op "cl3a-test"))))
