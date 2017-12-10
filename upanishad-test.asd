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
                 (:file "package")
                 (:module "index"       :components ((:file "slot-index")
                                                     (:file "slot-index-unique")
                                                     (:file "slot-index-multiple")))
                 (:module "memes"       :components ((:file "memes")))
                 (:module "replication" :components ((:file "master-slave")))
                 (:module "pool"        :components ((:file "basic")
                                                     (:file "indexes")
                                                     (:file "index")
                                                     (:file "memes")
                                                     (:file "meme")
                                                     (:file "id-counter")
                                                     (:file "index-object")
                                                     (:file "preference")
                                                     (:file "root-objects")
                                                     (:file "transaction")
                                                     (:file "serialize-backup")
                                                     (:file "serialize-files")
                                                     (:file "serialize-restore")
                                                     (:file "serialize-snapshot")))
                 (:module "utility"     :components ((:file "debug-pool")
                                                     (:file "printer")
                                                     (:file "utility")))
                 (:module "data-type"   :components ((:file "blob"))))))
  :depends-on (:upanishad :prove)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove.asdf) c)
                    (asdf:clear-system c)))
