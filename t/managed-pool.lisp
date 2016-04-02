(defpackage :upanishad-test.managed-pool
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool"))

(plan nil)




(finalize)
