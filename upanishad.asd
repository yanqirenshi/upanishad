(in-package :asdf)

(defsystem :upanishad
  :name "UPANISHAD"
  :author "Satoshi Iwasaki<yanqirenshi@gmail.com>"
  :version "1.0"
  :maintainer "Satoshi Iwasaki<yanqirenshi@gmail.com>"
  :licence "Lesser Lisp General Public License"
  :description "Common Lisp Prevalence Package"
  :long-description "Common Lisp Prevalence is an implementation of Object Prevalence for Common Lisp"
  :components
  ((:module "src"
    :serial t
    :components ((:module "meme"
                  :components ((:file "package")
                               (:file "class")))
                 (:module "index"
                  :components ((:file "package")
                               (:file "class")
                               (:file "generic-function")
                               (:file "utility")
                               (:file "slot-index")
                               (:file "slot-index-unique")
                               (:file "slot-index-multiple")))
                 (:module "memes"
                  :components ((:file "package")
                               (:file "class")
                               (:file "get-meme")
                               (:file "find-meme")
                               (:file "add-meme")
                               (:file "make-memes")
                               (:file "remove-meme")
                               (:file "memes")))

                 (:module "pool"
                  :components ((:file "package")
                               (:file "class")
                               (:file "generic-function")
                               (:file "basic")
                               (:module "slots" :components ((:file "slot-transaction-log")
                                                             (:file "slot-root-objects")
                                                             (:file "slot-index-objects")
                                                             (:file "slot-snapshot-pathnames")
                                                             (:file "slot-option")
                                                             (:file "slot-indexes")
                                                             (:file "slot-memes")))
                               (:module "serialize" :components ((:file "files")
                                                                 (:file "snapshot")
                                                                 (:file "backup")
                                                                 (:file "restore")))
                               (:module "transaction" :components ((:file "transaction")
                                                                   (:file "execute")))
                               (:file "printer")
                               (:file "query")
                               (:file "closing")
                               (:file "guarded-pool")
                               (:file "id-counter")
                               (:file "preference")
                               (:file "index")
                               (:file "meme")
                               (:file "etc")))
                 (:module "replication" :components ((:file "master-slave")))
                 (:module "utility"     :components ((:file "printer")
                                                     (:file "debug-pool")))
                 (:module "data-type"   :components ((:file "blob")))
                 (:file "package"))))
  :depends-on (:alexandria
               :cl-fad
               :s-xml
               :s-sysdeps
               :s-serialization
               :cl-ppcre)
  :in-order-to ((test-op (test-op upanishad-test))))
