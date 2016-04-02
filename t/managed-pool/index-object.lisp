(defpackage :upanishad-test.managed-pool.index-object
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.index-object)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.index-object"))

(plan nil)




(finalize)
