(defpackage :upanishad-test.debug-pool
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.debug-pool)

(defparameter *test-pool-directory* (test-pool-directory "debug-pool"))

(plan nil)




(finalize)
