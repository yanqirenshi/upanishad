(in-package :cl-user)

(defpackage :upanishad.pool
  (:use #:cl
        #:s-serialization
        #:upanishad.meme)
  (:nicknames :up.pool)
  (:import-from #:alexandria
                #:ensure-gethash)
  (:import-from #:upanishad.index
                #:index
                #:class-symbol
                #:slot-symbol)
  ;; classes
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
           ;; blob
           #:*blob-root*
           #:get-file
           #:copy-to-stream
           #:fill-from-stream
           #:fill-from-file
           #:destroy)
  (:export #:poolp ;; pool/basic
           #:memes
           #:indexes
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
           ;; pool/guarded-pool
           #:execute
           ;; managed-pool/transaction
           #:execute-transaction
           ;; managed-pool/root-objects
           #:get-object-at-%id
           #:find-objects
           #:get-object-list
           #:tx-create-object
           #:tx-change-object-slots
           #:tx-delete-object
           ;; managed-pool/%id-counter
           #:tx-create-%id-counter
           #:next-%id
           ;; managed-pool/index
           #:index-at
           #:add-index
           #:drop-index
           #:tx-remove-object-on-index)
  ;; pool/snapshot-backup-restore
  (:export #:snapshot
           #:backup
           #:restore)
  ;; managed-pool/preference
  (:export #:get-preference
           #:tx-set-preference
           #:all-preferences-keys)
  ;; master slave
  (:export #:start-master-client
           #:stop-master-client
           #:start-slave-server
           #:stop-slave-server)
  ;; pool - index
  (:export #:get-index
           #:tx-add-index
           #:tx-remove-index)
  ;; pool - index - meme
  (:export #:tx-add-meme-to-index
           #:tx-remove-meme-from-index)
  ;; pool - memes
  (:export #:tx-add-memes
           #:tx-remove-memes
           #:get-memes)
  ;; pool - meme
  (:export #:get-meme
           #:find-meme
           #:tx-change-meme-slots
           #:tx-create-meme
           #:tx-delete-meme)
  ;; debug-pool
  (:export #:print-transaction-log
           #:show-transaction-log
           #:print-snapshot
           #:transaction-log-tail)
  (:documentation "An implementation of Object Prevalence for Common Lisp"))
