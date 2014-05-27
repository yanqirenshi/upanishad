#|
  This file is a part of upanishad project.
  Copyright (c) 2014 Satoshi Iwasaki (yanqirenshi@gmail.com)
|#

(in-package :cl-user)
(defpackage upanishad-test-asd
  (:use :cl :asdf))
(in-package :upanishad-test-asd)

(defsystem upanishad-test
  :author "yanqirenshi"
  :license "LLGPL"
  :depends-on (:upanishad
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "upanishad"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
