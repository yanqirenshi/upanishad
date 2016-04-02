(defpackage :upanishad-test.utility
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.utility)

(defparameter *test-pool-directory* (test-pool-directory "utility"))

(plan nil)




(finalize)
