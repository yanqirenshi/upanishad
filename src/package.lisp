;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id$
;;;;
;;;; Package definitions for the CL-PREVALENCE project
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(defpackage :s-serialization
  (:use :cl)
  (:export
   #:serializable-slots
   #:serialize-xml #:serialize-sexp
   #:deserialize-xml #:deserialize-sexp
   #:make-serialization-state
   #:reset-known-slots)
  (:documentation "XML and s-expression based serialization for Common Lisp and CLOS"))

(defpackage :upanishad
  (:use :cl :s-serialization)
  (:nicknames :up)
  (:export
   #:make-pool
   #:make-transaction
   #:execute
   #:query
   #:snapshot
   #:restore
   #:backup
   #:get-root-object
   #:get-option
   #:remove-root-object
   #:pool
   #:guarded-pool
   #:get-guard
   #:transaction
   #:no-rollback-error
   #:initiates-rollback
   #:totally-destroy

   #:print-transaction-log #:show-transaction-log #:print-snapshot #:transaction-log-tail

   #:blob
   #:get-file
   #:get-name
   #:name
   #:get-mime-type
   #:mime-type
   #:get-size
   #:size
   #:get-keywords
   #:keywords
   #:*blob-root*
   #:copy-to-stream
   #:fill-from-stream
   #:fill-from-file
   #:destroy

   #:execute-transaction
   #:atman
   #:get-id
   #:id
   #:find-all-objects
   #:find-object-with-id
   #:get-object-with-id
   #:find-object-with-slot
   #:tx-create-id-counter
   #:tx-create-object
   #:tx-delete-object
   #:tx-change-object-slots
   #:get-preference
   #:all-preferences-keys
   #:tx-set-preference
   #:index-on
   #:drop-index-on
   #:tx-remove-object-on-slot-index

   ;; Plato Wu,2009/02/28: Add for testing system.
   #:close-open-streams
   #:next-id

   #:start-master-client
   #:stop-master-client
   #:start-slave-server
   #:stop-slave-server)
  (:documentation "An implementation of Object Prevalence for Common Lisp"))

;;;; eof
