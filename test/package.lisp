(defpackage :upanishad-test
  (:use :cl :upanishad :5am :s-serialization)
  (:nicknames :up-test)
  (:export
   #:run!
   #:upanishad-test))
(in-package :upanishad-test)

(def-suite upanishad-test)


