(in-package :upanishad-test)

(plan nil)

(diag "utility.lisp")

(with-pool (pool *test-pool-directory*)
  (subtest "get-slot-index-class"
    (skip 1 "wait ...."))

  (subtest "assert-class"
    (skip 1 "wait ....")))

(finalize)
