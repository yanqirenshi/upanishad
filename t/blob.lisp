(defpackage :upanishad-test.blob
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.blob)

(defparameter *test-pool-directory* (test-pool-directory "blob"))

(plan nil)




(finalize)
