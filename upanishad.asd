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
    :serial t
    :components ((:file "package")
                 (:file "utility")
                 (:file "generic-function")
                 (:module "class" :depends-on ("utility")
                  :components ((:file "data")
                               (:file "pool")
                               (:file "transaction")))
                 (:module "index" :depends-on ("class")
                  :components ((:file "index")
                               (:file "utility")
                               (:file "slot-index")
                               (:file "slot-index-unique")
                               (:file "slot-index-multiple")))
                 (:module "pool" :depends-on ("index")
                  :components ((:file "basic")
                               (:file "snapshot-backup-restore")
                               (:file "guarded-pool")
                               (:file "etc")))
                 (:module "managed-pool"  :depends-on ("pool")
                  :components ((:file "transaction")
                               (:file "id-counter")
                               (:file "preference")
                               (:file "memes")
                               (:file "pool")
                               ;; old
                               (:file "index-objects-new")
                               (:file "index-objects")
                               (:file "root-objects")))
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
