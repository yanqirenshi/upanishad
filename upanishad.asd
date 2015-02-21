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
            ;;   |
            ;; package
            ;;   |
            ;; [serialization]
            ;;   |
            ;; class
            ;;   |
            ;; prevalence
            ;;   |
            ;;   +----------------------+
            ;;   |                      |
            ;; managed-prevalence  master-slave
            ;;   |                      |
            ;; blob                     |
            ;;   |                      |
            ;;   +----------------------+
            ;;   |
            ;; printer
            ;;   |
            ;; (end)
            ;;
            :components ((:file "package")
                         (:module "serialization"
                                  :components ((:file "serialization")
                                               (:file "xml"  :depends-on ("serialization"))
                                               (:file "sexp" :depends-on ("serialization")))
                                  :depends-on ("package"))
                         (:file "class"              :depends-on ("serialization"))
                         (:file "prevalence"         :depends-on ("class"))
                         (:file "managed-prevalence" :depends-on ("prevalence"))
                         (:file "master-slave"       :depends-on ("prevalence"))
                         (:file "blob"               :depends-on ("managed-prevalence"))
                         (:file "printer"            :depends-on ("master-slave" "blob")))))
  :depends-on (:alexandria
               :s-xml
               :s-sysdeps))

