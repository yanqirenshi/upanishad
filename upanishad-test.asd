#|
This file is a part of upanishad project.
Copyright (c) 2014 Satoshi Iwasaki (yanqirenshi@gmail.com)
|#

(in-package :cl-user)
(defpackage upanishad-test-asd
  (:use :cl :asdf))
(in-package :upanishad-test-asd)

(defsystem :upanishad-test
  :name "UPANISHAD-TEST"
  :author "yanqirenshi"
  :version "0.1"
  :maintainer "yanqirenshi"
  :licence "Lesser Lisp General Public License"
  :description "Common Lisp Prevalence Test Package"
  :long-description "5am test suite for cl-prevalence"
  :components
  ((:module "t"
    :components ((:file "test-utility")
                 (:file "package" :depends-on ("test-utility"))
                 (:file "blob")
                 (:file "debug-pool")
                 (:module "managed-pool"
                  :components ((:file "transaction")
                               (:file "root-objects")
                               (:file "preference")
                               (:file "index-object")
                               (:file "id-counter")))
                 (:file "master-slave")
                 (:file "pool/pool.lisp")
                 (:file "printer")
                 (:file "utility")
                 (:module "ancestral"
                  :components ((:file "managed-pool")
                               (:file "master-slave")
                               (:file "pool")
                               (:file "serialization"))))))
  :depends-on (:upanishad :prove)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove.asdf) c)
                    (asdf:clear-system c)))
