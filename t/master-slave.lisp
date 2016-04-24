(defpackage :upanishad-test.master-slave
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.master-slave)

(defparameter *test-pool-directory* (test-pool-directory "master-slave"))

(plan nil)


(finalize)
