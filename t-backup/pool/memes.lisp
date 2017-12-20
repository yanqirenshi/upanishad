(defpackage :upanishad-test.managed-pool.pool-memes
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility)
  (:import-from :upanishad.memes
                #:make-memes))
(in-package :upanishad-test.managed-pool.pool-memes)

(defparameter *test-pool-directory*
  (test-pool-directory "managed-pool.pool-memes"))

(defclass test-meme-1 (meme) ())
(defclass test-meme-2 (meme) ())
(defclass test-meme-3 (meme) ())

(plan 3)

(subtest ":TX-ADD-MEMES"
  (with-pool (pool *test-pool-directory*)
    (let ((memes-1 (make-memes 'test-meme-1))
          (memes-2 (make-memes 'test-meme-2)))
      ;; add
      (is (tx-add-memes pool memes-1)
          memes-1
          "make memes-1")
      (is (tx-add-memes pool memes-2)
          memes-2
          "make memes-2")
      ;; get
      (is (get-memes pool :class 'test-meme-1)
          memes-1
          "get memes-1")
      (is (get-memes pool :class 'test-meme-2)
          memes-2
          "get memes-2")
      (is (get-memes pool :class 'test-meme-3)
          nil
          "get memes-3 is null"))))

(subtest ":TX-REMOVE-MEMES"
  (with-pool (pool *test-pool-directory*)
    (let ((memes-1 (make-memes 'test-meme-1))
          (memes-2 (make-memes 'test-meme-2)))
      ;; add
      (is (tx-add-memes pool memes-1)
          memes-1
          "make memes-1")
      (is (tx-add-memes pool memes-2)
          memes-2
          "make memes-2")
      ;; get (before)
      (is (get-memes pool :class 'test-meme-1)
          memes-1
          "get memes-1")
      (is (get-memes pool :class 'test-meme-2)
          memes-2
          "get memes-2")
      ;; remove
      (tx-remove-memes pool memes-1)
      ;; get (after)
      (is (get-memes pool :class 'test-meme-1)
          nil
          "get memes-1 is nil")
      (is (get-memes pool :class 'test-meme-2)
          memes-2
          "get memes-2"))))

(subtest ":GET-MEMES"
  (with-pool (pool *test-pool-directory*)
    (let ((memes-1 (make-memes 'test-meme-1))
          (memes-2 (make-memes 'test-meme-2)))
      ;; add
      (is (tx-add-memes pool memes-1)
          memes-1
          "make memes-1")
      (is (tx-add-memes pool memes-2)
          memes-2
          "make memes-2")
      ;; get
      (is (get-memes pool :class 'test-meme-1)
          memes-1
          "get memes-1")
      (is (get-memes pool :class 'test-meme-2)
          memes-2
          "get memes-2")
      (is (get-memes pool :class 'test-meme-3)
          nil
          "get memes-3 is null"))))

(finalize)
