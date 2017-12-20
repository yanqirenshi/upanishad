(in-package :upanishad-test)

(defparameter *test-pool-directory* (test-pool-directory "demo"))

(defclass test-meme (upanishad:meme) ())

(plan nil)

(with-pool (pool *test-pool-directory*)
  (print pool))

(finalize)
