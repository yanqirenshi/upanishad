(defpackage :upanishad-test.managed-pool.pool
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.pool)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.pool"))

(plan nil)




(finalize)
