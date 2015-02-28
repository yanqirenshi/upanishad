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
  ((:file "test/package")
   (:file "test/test-pool"          :depends-on ("test/package"))
   (:file "test/test-managed-pool"  :depends-on ("test/package"))
   (:file "test/test-master-slave"  :depends-on ("test/package"))
   (:file "test/test-serialization" :depends-on ("test/package")))
  :depends-on (:upanishad :fiveam))
