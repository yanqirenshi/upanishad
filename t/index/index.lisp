(in-package :upanishad-test)

(defparameter *test-pool-directory* (test-pool-directory "demo"))

(defclass test-meme (upanishad:meme)
  ((name :accessor name
         :initarg :name
         :initform "")))

(plan nil)

(diag "utility.lisp")

(with-pool (pool *test-pool-directory*)
  (subtest "get-slot-index-class"
    (skip 1 "wait ...."))

  (subtest "assert-class"
    (skip 1 "wait ....")))

(finalize)
