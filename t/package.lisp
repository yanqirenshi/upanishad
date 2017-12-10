(defpackage :upanishad-test
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization)
  (:nicknames :up-test))
(in-package :upanishad-test)

(setf prove:*default-reporter* :dot)
