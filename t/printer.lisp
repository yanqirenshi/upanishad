(defpackage :upanishad-test.printer
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.printer)

(defparameter *test-pool-directory* (test-pool-directory "printer"))

(plan nil)




(finalize)
