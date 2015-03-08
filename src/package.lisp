;;;;;
;;;;; Contents
;;;;;  1. Define package
;;;;;  2. Utilities
;;;;;

;;;
;;; 1. Define package
;;;
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
   #:pool-p
   #:make-pool
   #:make-transaction
   #:execute
   #:query
   #:snapshot
   #:restore
   #:backup
   #:remove-root-object
   #:get-root-object
   #:get-option
   #:get-guard
   #:transaction
   #:no-rollback-error
   #:initiates-rollback
   #:totally-destroy
   #:print-transaction-log
   #:show-transaction-log
   #:print-snapshot
   #:transaction-log-tail
   #:get-file
   #:*blob-root*
   #:copy-to-stream
   #:fill-from-stream
   #:fill-from-file
   #:destroy
   #:execute-transaction
   #:tx-create-id-counter
   #:tx-create-object
   #:tx-delete-object
   #:tx-change-object-slots
   #:tx-set-preference
   #:tx-remove-object-on-slot-index
   #:find-all-objects
   #:find-object-with-id
   #:get-object-with-id
   #:find-object-with-slot
   #:get-preference
   #:all-preferences-keys
   #:index-on
   #:drop-index-on
   #:close-open-streams
   #:next-id
   #:get-at-id
   ;; master slave
   #:start-master-client
   #:stop-master-client
   #:start-slave-server
   #:stop-slave-server
   ;;;
   ;;; Class: pool and guarded-pool
   ;;;
   #:pool
   #:guarded-pool
   ;;;
   ;;; Class: blob
   ;;;
   ;; slots
   #:blob
   #:name
   #:size
   #:mime-type
   #:keywords
   ;; getter / setter
   #:get-name
   #:get-mime-type
   #:get-size
   #:get-keywords
   ;;;
   ;;; Class: meme
   ;;;
   #:meme
   ;;;
   ;;; Class: atman
   ;;;
   #:atman
   ;; slots
   #:id
   ;; getter / setter
   #:get-id)
  (:documentation "An implementation of Object Prevalence for Common Lisp"))



#|
-*- Mode: LISP -*-

$Id$

Package definitions for the CL-PREVALENCE project

Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.

You are granted the rights to distribute and use this software
as governed by the terms of the Lisp Lesser General Public License
(http://opensource.franz.com/preamble.html), also known as the LLGPL.
|#
