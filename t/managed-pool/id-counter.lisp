(defpackage :upanishad-test.managed-pool.id-counter
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.id-counter)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.id-counter"))

(plan nil)

(subtest "tx-create-%id-counter"
  (with-pool (pool *test-pool-directory* :with-id-counter nil)

    (ok (null (get-root-object pool :%id-counter)) "%id-counter is null")

    (is (tx-create-%id-counter pool) 0 "success tx-create-%id-counter")

    (ok (get-root-object pool :%id-counter) "can created %id-counter")))

(subtest "next-%id"
  (with-pool (pool *test-pool-directory*)
    (is (get-root-object pool :%id-counter) 0 "first value is zero")
    (is (next-%id pool) 1 "success next-%id")
    (is (get-root-object pool :%id-counter) 1 "updated value is 1")))

(finalize)
