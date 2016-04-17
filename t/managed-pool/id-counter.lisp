(defpackage :upanishad-test.managed-pool.index
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.id-counter)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.id-counter"))

(plan nil)

(finalize)
