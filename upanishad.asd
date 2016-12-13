(in-package :asdf)

(defsystem :upanishad
  :name "UPANISHAD"
  :author "Satoshi Iwasaki<yanqirenshi@gmail.com>"
  :version "0.1"
  :maintainer "Satoshi Iwasaki<yanqirenshi@gmail.com>"
  :licence "Lesser Lisp General Public License"
  :description "Common Lisp Prevalence Package"
  :long-description "Common Lisp Prevalence is an implementation of Object Prevalence for Common Lisp"
  :components
  ((:module "src"
    :serial t
    :components ((:file "meme")
                 (:module "index"
                  :components ((:file "package")
                               (:file "class")
                               (:file "generic-function")
                               (:file "utility")
                               (:file "slot-index")
                               (:file "slot-index-unique")
                               (:file "slot-index-multiple")))
                 (:file "memes")
                 (:module "pool"
                  :components ((:file "package")
                               (:file "utility")
                               (:file "generic-function")
                               (:file "class")
                               (:file "basic")
                               (:module "serialize"
                                :components ((:file "files")
                                             (:file "snapshot")
                                             (:file "backup")
                                             (:file "restore")))
                               (:file "guarded-pool")
                               (:file "etc")
                               ;; move from managed-pool
                               (:file "transaction")
                               (:file "id-counter")
                               (:file "preference")
                               (:file "indexes")
                               (:file "index")
                               (:file "memes")
                               (:file "meme")
                               (:module "old"
                                :components ((:file "index-objects-new")
                                             (:file "index-objects")
                                             (:file "root-objects")))))
                 (:file "master-slave")
                 (:file "blob")
                 (:file "printer")
                 (:file "debug-pool"))))
  :depends-on (:alexandria
               :cl-fad
               :s-xml
               :s-sysdeps
               :s-serialization
               :cl-ppcre)
  :in-order-to ((test-op (test-op upanishad-test))))
