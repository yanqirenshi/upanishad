(defpackage :upanishad-test.memes
  (:use :cl
        #:upanishad
        #:prove
        #:upanishad-test.test-utility
        #:upanishad.memes))
(in-package :upanishad-test.memes)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.pool"))

(plan nil)

(subtest "memes" (skip 1 "準備中"))
(subtest "add-meme" (skip 1 "準備中"))
(subtest "remove-meme" (skip 1 "準備中"))
(subtest "make-memes" (skip 1 "準備中"))

(finalize)
