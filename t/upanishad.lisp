(in-package :upanishad-test)

(defparameter *test-pool-directory* (test-pool-directory "demo"))

(defclass test-meme (upanishad:meme) ())

(plan nil)

(with-pool (pool *test-pool-directory*)
  (subtest "create meme"
    (let ((meme (tx-create-meme pool 'test-meme)))
      (ok meme "tx-create-meme"))))

(finalize)
