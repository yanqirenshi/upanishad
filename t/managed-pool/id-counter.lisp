(defpackage :upanishad-test.managed-pool.id-counter
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.id-counter)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.id-counter"))

(plan 2)


(subtest ":tx-create-%id-counter" (skip 1 "準備中"))
(subtest ":next-%id" (skip 1 "準備中"))


(finalize)
