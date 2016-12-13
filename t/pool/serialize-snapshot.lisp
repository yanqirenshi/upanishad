(defpackage :upanishad-test.pool.serialize-snapshot
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.pool.serialize-snapshot)

(defparameter *test-pool-directory* (test-pool-directory "upanishad-test.pool.serialize-snapshot"))

(plan nil)

(subtest "GET-SNAPSHOT-OBJECTS" (skip 1 "..."))

(subtest "SNAPSHOT-OBJECTS" (skip 1 "..."))

(subtest "SNAPSHOT" (skip 1 "..."))

(finalize)
