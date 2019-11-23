(defsystem "cl3a"
    :description "Common Lisp Library of Linear Algebra"
    :version "0.4"
    :author "Kiwamu Ishikura"
    :license "GPL"
    :depends-on ("alexandria")
    :components ((:module "src"
                  :components
                  ((:file "cl3a"
                          ;; :depends-on ("typedef" "dotprod" "norm" "add_vector"
                          :depends-on ("dotprod" "norm" "add_vector"
                                       "rotate" "mvmult" "mmmult10"))
                   ;; (:file "typedef")
                   (:file "utilities_vop")
                   (:file "utilities"
                          :depends-on ("utilities_vop"))
                          ;; :depends-on ("typedef" "utilities_vop"))
                   (:file "dotprod_vop")
                   (:file "dotprod"
                          :depends-on ("utilities" "utilities_vop" "dotprod_vop"))
                          ;; :depends-on ("typedef" "utilities" "utilities_vop" "dotprod_vop"))
                   (:file "norm"
                          :depends-on ("utilities" "dotprod"))
                          ;; :depends-on ("typedef" "utilities" "dotprod"))
                   (:file "add_vector"
                          :depends-on ("utilities" "utilities_vop"))
                          ;; :depends-on ("typedef" "utilities" "utilities_vop"))
                   (:file "rotate"
                          :depends-on ("utilities" "utilities_vop"))
                          ;; :depends-on ("typedef" "utilities" "utilities_vop"))
                   (:file "mvmult_vop")
                   (:file "mvmult"
                          :depends-on ("utilities" "utilities_vop" "mvmult_vop"))
                          ;; :depends-on ("typedef" "utilities" "utilities_vop" "mvmult_vop"))
                   (:file "mmmult10_vop")
                   (:file "mmmult10"
                          :depends-on ("utilities" "mmmult10_vop")))))
                          ;; :depends-on ("typedef" "utilities" "mmmult10_vop")))))
    :in-order-to ((test-op (test-op "cl3a/tests"))))


(defsystem "cl3a/tests"
    :description "test for cl3a"
    :depends-on ("cl3a" "rove")
    :components ((:module "t"
                  :components
                  ((:file "cl3a-test" :depends-on ("naive-funcs"))
                   (:file "naive-funcs"))))
    :perform (test-op (op c) (symbol-call :rove '#:run c)))
