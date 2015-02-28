(defpackage :upanishad-test
  (:use :cl :upanishad :5am :s-serialization)
  (:nicknames :up-test)
  (:export
   #:run!
   #:upanishad-test
   #:run-all-test))
(in-package :upanishad-test)

(def-suite upanishad-test)

(def-suite test-managed-pool  :in upanishad-test)
(def-suite test-master-slave  :in upanishad-test)
(def-suite test-pool          :in upanishad-test)
(def-suite test-serialization :in upanishad-test)

(defun run-all-test ()
  (progn (in-suite test-serialization)
         (upanishad-test:run!)
         (in-suite test-pool)
         (upanishad-test:run!)
         (in-suite test-managed-pool)
         (upanishad-test:run!)
         (in-suite test-master-slave)
         (upanishad-test:run!)))
