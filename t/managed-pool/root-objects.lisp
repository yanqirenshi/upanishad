(defpackage :upanishad-test.managed-pool.root-objects
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.root-objects)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.root-objects"))

(plan nil)




(finalize)
