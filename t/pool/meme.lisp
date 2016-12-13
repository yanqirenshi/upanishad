(defpackage :upanishad-test.managed-pool.pool-meme
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.pool-meme)

(defparameter *test-pool-directory*
  (test-pool-directory "managed-pool.pool-meme"))

(plan nil)

(subtest ":GET-MEME"
  (skip 1 "wait"))

(subtest ":FIND-MEME"
  (skip 1 "wait"))

(subtest ":TX-CHANGE-MEME-SLOTS"
  (skip 1 "wait"))

(subtest ":TX-CREATE-MEME"
  (skip 1 "wait"))

(subtest ":TX-DELETE-MEME"
  (skip 1 "wait"))

(finalize)
