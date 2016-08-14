(defpackage :upanishad-test.debug-pool
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.debug-pool)

(defparameter *test-pool-directory* (test-pool-directory "debug-pool"))

(plan 4)


(subtest "::print-transaction-log" (skip 1 "準備中"))
(subtest "::show-transaction-log" (skip 1 "準備中"))
(subtest "::print-snapshot" (skip 1 "準備中"))
(subtest "::transaction-log-tail" (skip 1 "準備中"))


(finalize)
