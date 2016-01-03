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
  (:export #:serializable-slots
           #:serialize-xml
           #:serialize-sexp
           #:deserialize-xml
           #:deserialize-sexp
           #:make-serialization-state
           #:reset-known-slots)
  (:documentation "XML and s-expression based serialization for Common Lisp and CLOS"))


(defpackage :upanishad
  (:use :cl :s-serialization)
  (:nicknames :up)
  (:export #:pool-p
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
           #:tx-create-%id-counter
           #:tx-create-object
           #:tx-delete-object
           #:tx-change-object-slots
           #:tx-set-preference
           #:tx-remove-object-on-slot-index
           #:get-object-at-%id
           #:find-object
           #:get-preference
           #:all-preferences-keys
           #:index-on
           #:drop-index-on
           #:close-open-streams
           #:next-%id
           #:get-at-%id
           #:get-object-list
           ;; master slave
           #:start-master-client
           #:stop-master-client
           #:start-slave-server
           #:stop-slave-server
           ;; Class: pool and guarded-pool
           #:pool
           #:guarded-pool
           ;; Class: blob
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
           ;; Class: meme
           #:meme
           ;; Class: atman
           #:atman
           ;; slots
           #:%id)
  (:documentation "An implementation of Object Prevalence for Common Lisp"))
