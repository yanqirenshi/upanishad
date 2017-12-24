(defpackage :upanishad-test
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility)
  (:nicknames :up-test))
(in-package :upanishad-test)

(setf prove:*default-reporter* :list)
