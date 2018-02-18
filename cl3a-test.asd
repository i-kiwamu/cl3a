(in-package :cl-user)
(defpackage cl3a-test-asd
  (:use :cl :asdf :prove))
(in-package :cl3a-test-asd)

(defsystem cl3a-test
    :description "test for cl3a"
    :depends-on (:cl3a)
    :defsystem-depends-on (:prove-asdf)
    :components ((:module "t"
                          :components
                          ((:test-file "cl3a-test"
                            :depends-on ("naive-funcs"))
                           (:file "naive-funcs"))))
    :perform (test-op :after (op c)
                      (funcall (intern #.(string :run) :prove) c)
                      (asdf:clear-system c)))
