(defpackage :upanishad-test.printer
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.printer)

(defparameter *test-pool-directory* (test-pool-directory "printer"))

(plan 1)


(subtest ":print-root-objects" (skip 1 "準備中"))


(finalize)
