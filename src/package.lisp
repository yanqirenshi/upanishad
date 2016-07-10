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

(defpackage :upanishad.objects
  (:use :cl)
  (:export #:objects
           #:make-objects
           #:add-object
           #:remove-object)
  (:documentation ""))

(defpackage :upanishad.index
  (:use :cl)
  (:export #:index
           #:class-symbol
           #:slot-symbol
           #:add-object
           #:add-objects
           #:remove-object
           #:make-index)
  (:documentation ""))

(defpackage :upanishad
  (:use :cl :s-serialization)
  (:nicknames :up)
  (:import-from :alexandria
                #:ensure-gethash)
  (:import-from :upanishad.index
                #:index
                #:class-symbol
                #:slot-symbol)
  (:export #:atman
           #:meme
           #:blob
           #:pool
           #:guarded-pool
           #:transaction
           ;; slots
           #:name
           #:size
           #:mime-type
           #:keywords
           #:get-name
           #:get-mime-type
           #:get-size
           #:get-keywords
           #:%id
           #:guard
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; blob
           #:*blob-root*
           #:get-file
           #:copy-to-stream
           #:fill-from-stream
           #:fill-from-file
           #:destroy
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; debug-pool
           #:print-transaction-log
           #:show-transaction-log
           #:print-snapshot
           #:transaction-log-tail
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; pool/basic
           #:poolp
           #:get-root-object
           #:remove-root-object
           #:get-index-object
           #:remove-index-object
           #:get-option
           #:no-rollback-error
           #:initiates-rollback
           #:query
           #:close-open-streams
           #:stop
           #:totally-destroy
           #:make-pool
           #:make-transaction
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; pool/guarded-pool
           #:execute
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; pool/snapshot-backup-restore
           #:snapshot
           #:backup
           #:restore
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; managed-pool/transaction
           #:execute-transaction
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; managed-pool/root-objects
           #:get-object-at-%id
           #:find-objects
           #:get-object-list
           #:tx-create-object
           #:tx-change-object-slots
           #:tx-delete-object
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; managed-pool/preference
           #:get-preference
           #:tx-set-preference
           #:all-preferences-keys
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; managed-pool/%id-counter
           #:tx-create-%id-counter
           #:next-%id
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; managed-pool/index
           #:index-at
           #:add-index
           #:drop-index
           #:tx-remove-object-on-index
           ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; master slave
           #:start-master-client
           #:stop-master-client
           #:start-slave-server
           #:stop-slave-server)
  (:documentation "An implementation of Object Prevalence for Common Lisp"))
