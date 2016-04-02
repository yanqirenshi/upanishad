(defpackage :upanishad-test.utility
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.utility)

(defparameter *test-pool-directory* (test-pool-directory "utility"))

(plan 3)


(subtest ":getter-name" (skip 1 "準備中"))
(subtest ":defreader" (skip 1 "準備中"))
(subtest ":defwriter" (skip 1 "準備中"))


(finalize)
