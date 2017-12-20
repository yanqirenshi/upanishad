(defpackage :upanishad-test.pool.serialize-backup
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.pool.serialize-backup)

(defparameter *test-pool-directory* (test-pool-directory "upanishad-test.pool.serialize-backup"))

(plan nil)

(subtest "BACKUP" (skip 1 "..."))

(finalize)
