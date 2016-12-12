(defpackage :upanishad-test.managed-pool.pool-memes
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.pool-memes)

(defparameter *test-pool-directory*
  (test-pool-directory "managed-pool.pool-memes"))

(plan nil)

(subtest ":TX-ADD-MEMES"
  (skip 1 "wait"))

(subtest ":TX-REMOVE-MEMES"
  (skip 1 "wait"))

(subtest ":GET-MEMES"
  (skip 1 "wait"))

(finalize)
