(defpackage :upanishad-test.managed-pool.preference
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.preference)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.preference"))

(plan nil)

(subtest ":get-preference" (skip 1 "準備中"))
(subtest ":tx-set-preference" (skip 1 "準備中"))
(subtest ":all-preferences-keys" (skip 1 "準備中"))

(finalize)
