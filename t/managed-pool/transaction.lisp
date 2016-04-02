(defpackage :upanishad-test.managed-pool.transaction
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.transaction)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.transaction"))

(plan nil)




(finalize)
