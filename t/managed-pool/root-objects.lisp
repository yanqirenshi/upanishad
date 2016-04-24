(defpackage :upanishad-test.managed-pool.root-objects
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.root-objects)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.root-objects"))

(plan 17)


(subtest "::get-objects-root-name" (skip 1 "準備中"))
(subtest ":get-object-at-%id" (skip 1 "準備中"))
(subtest "::find-all-objects" (skip 1 "準備中"))
(subtest "::find-objects-with-slot-use-index" (skip 1 "準備中"))
(subtest "::find-objects-with-slot-full-scan" (skip 1 "準備中"))
(subtest "::find-objects-with-slot" (skip 1 "準備中"))
(subtest ":find-objects" (skip 1 "準備中"))
(subtest "::slot-value-changed-p" (skip 1 "準備中"))
(subtest ":tx-create-object" (skip 1 "準備中"))
(subtest ":tx-delete-object" (skip 1 "準備中"))
(subtest ":tx-change-object-slots" (skip 1 "準備中"))
(subtest "::class-rootp" (skip 1 "準備中"))
(subtest "::class-%id-list" (skip 1 "準備中"))
(subtest "::root-list" (skip 1 "準備中"))
(subtest "::object-root-name" (skip 1 "準備中"))
(subtest ":get-object-list" (skip 1 "準備中"))
(subtest ":print-root-list" (skip 1 "準備中"))


(finalize)
