(in-package :cl-user)
(defpackage cl3a-test-asd
  (:use :cl :asdf :prove))
(in-package :cl3a-test-asd)

(defsystem cl3a-test
    :description "test for cl3a"
    :depends-on (:cl3a :prove)
    :components ((:module "t"
                          :components
                          ((:file "cl3a-test"))))
    :perform (load-op :after (op c) (asdf:clear-system c)))
