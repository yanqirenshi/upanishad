(defpackage :upanishad-test.managed-pool.preference
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.preference)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.preference"))

(plan nil)




(finalize)
