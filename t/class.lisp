(defpackage :upanishad-test.class
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.class)

(defparameter *test-pool-directory* (test-pool-directory "class"))

(plan nil)




(finalize)
