(in-package :cl-user)
(defpackage cl3a-test-asd
  (:use :cl :asdf :prove))
(in-package :cl3a-test-asd)

(defsystem cl3a-test
    :description "test for cl3a"
    :depends-on (:cl3a :prove :cl-slice)
    :components ((:module "t"
                          :components
                          ((:test-file "cl3a-test"
                            :depends-on ("naive-funcs"))
                           (:file "naive-funcs"))))
    :perform (test-op :after (op c)
                      (funcall (intern #.(string :run-test-system)
                                       :prove-asdf) c)
                      (asdf:clear-system c)))
