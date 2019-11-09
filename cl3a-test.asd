(defsystem "cl3a-test"
    :description "test for cl3a"
    :depends-on (:cl3a :rove)
    :components ((:module "t"
                          :components
                          ((:test-file "cl3a-test"
                            :depends-on ("naive-funcs"))
                           (:file "naive-funcs"))))
    :perform (test-op (op c) (symbol-call :rove '#:run c)))
