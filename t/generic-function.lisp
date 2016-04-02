(defpackage :upanishad-test.generic-function
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.generic-function)

(defparameter *test-pool-directory* (test-pool-directory "generic-function"))

(plan nil)




(finalize)
