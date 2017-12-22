(in-package :upanishad-test)

(defparameter *test-pool-directory* (test-pool-directory "demo"))

(defclass test-meme (upanishad:meme)
  ((name :accessor name
         :initarg :name
         :initform "")))

(plan 4)

(with-pool (pool *test-pool-directory*)
  (subtest "create meme"
    (let ((meme (tx-create-meme pool 'test-meme)))
      (ok meme "tx-create-meme")))

  (tx-create-meme pool 'test-meme)
  (tx-create-meme pool 'test-meme)

  (subtest "get-meme"
    (ok (get-meme pool 'test-meme :%id 1))
    (ok (get-meme pool 'test-meme :%id 2))
    (ok (get-meme pool 'test-meme :%id 3)))

  (subtest "find-meme"
    (skip 1 "wait ....")))

(finalize)
