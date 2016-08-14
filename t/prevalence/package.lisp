(defpackage :upanishad-test.prevalence
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.prevalence)

(setf prove:*default-reporter* :dot)
