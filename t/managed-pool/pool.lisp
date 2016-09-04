(defpackage :upanishad-test.managed-pool.pool
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.pool)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.pool"))

(plan nil)

;;;;;
;;;;; indexes
;;;;;
(subtest "GET-SLOT-INDEX")
(subtest "%TX-ADD-SLOT-INDEX")
(subtest "TX-ADD-INDEX")
(subtest "TX-REMOVE-INDEX")

;;;;;
;;;;; index
;;;;;
(subtest "TX-ADD-MEME-TO-SLOT-INDEX")
(subtest "TX-REMOVE-MEME-FROM-SLOT-INDEX")

;;;;;
;;;;; memes
;;;;;
(subtest "TX-ADD-MEMES")
(subtest "TX-REMOVE-MEMES")
(subtest "GET-MEMES-AT")

;;;;;
;;;;; meme
;;;;;
(subtest "GET-MEME-AT-%ID")
(subtest "FIND-MEME")
(subtest "SLOT-VALUE-CHANGED-P")
(subtest "TX-CHANGE-MEME-SLOTS")
(subtest "TX-CREATE-MEME")
(subtest "TX-DELETE-MEME")

(finalize)
