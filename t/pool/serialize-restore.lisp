(defpackage :upanishad-test.pool.serialize-restore
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.pool.serialize-restore)

(defparameter *test-pool-directory* (test-pool-directory "upanishad-test.pool.serialize-restore"))

(plan nil)

(subtest "RESTORE-OBJECTS" (skip 1 "..."))

(subtest "RESTORE-TRANSACTION-LOG" (skip 1 "..."))

(subtest "RESTORE" (skip 1 "..."))

(finalize)
