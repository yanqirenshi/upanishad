(defpackage :upanishad-test.pool
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.pool)

(defparameter *test-pool-directory* (test-pool-directory "pool"))

(plan nil)




(finalize)
