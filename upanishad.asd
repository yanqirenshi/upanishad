;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id$
;;;;
;;;; The CL-PREVALENCE ASDF system definition
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

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
            ;;
            ;; (start)
            ;; package
            ;; [serialization]
            ;; generic-function
            ;; class
            ;; pool
            ;;   +-----------------+
            ;; managed-pool  master-slave
            ;; blob                |
            ;;   +-----------------+
            ;; printer
            ;; (end)
            ;;
    :components ((:file "package")
                 (:file "utility"          :depends-on ("package"))
                 (:file "generic-function" :depends-on ("utility"))
                 (:file "class"            :depends-on ("utility"))
                 (:file "pool"             :depends-on ("class"))
                 (:module "managed-pool"  :depends-on ("pool")
                  :components ((:file "transaction")
                               (:file "id-counter")
                               (:file "preference")
                               (:file "index-object" :depends-on ("transaction"))
                               (:file "root-objects" :depends-on ("index-object"))))
                 (:file "master-slave"     :depends-on ("pool"))
                 (:file "blob"             :depends-on ("managed-pool"))
                 (:file "printer"          :depends-on ("master-slave" "blob"))
                 (:file "debug-pool"       :depends-on ("printer")))))
  :depends-on (:alexandria
               :s-xml
               :s-sysdeps
               :s-serialization
               :cl-ppcre)
  :in-order-to ((test-op (test-op upanishad-test))))

