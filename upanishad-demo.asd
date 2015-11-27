#|
This file is a part of upanishad project.
Copyright (c) 2014 Satoshi Iwasaki (yanqirenshi@gmail.com)
|#

(in-package :cl-user)
(defpackage upanishad-demo-asd
  (:use :cl :asdf))
(in-package :upanishad-demo-asd)

(defsystem :upanishad-demo
  :name "UPANISHAD-DEMO"
  :author "yanqirenshi"
  :version "0.1"
  :maintainer "yanqirenshi"
  :licence "Lesser Lisp General Public License"
  :description "Common Lisp Prevalence Test Package"
  :long-description "5am test suite for cl-prevalence"
  :components
  ((:module "demo"
    :components ((:file "package")
                 (:file "demo1")
                 (:file "demo2"))))
  :depends-on (:upanishad))
