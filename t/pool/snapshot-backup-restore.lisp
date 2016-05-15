(defpackage :upanishad-test.pool.snapshot-backup-restore
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.pool.snapshot-backup-restore)

(defparameter *test-pool-directory* (test-pool-directory "pool"))

(plan 13)


(subtest "::snapshot-type-p"
  (ok (upanishad::snapshot-type-p :object))
  (ok (null (upanishad::snapshot-type-p :objects)))
  (ok (upanishad::snapshot-type-p :index)))

(subtest "snapshot-pathnames"
  (with-pool (pool *test-pool-directory*)
    (is (upanishad::snapshot-pathnames pool :object)
        #p"~/var/upanishad/test/pool/snapshot-object.xml"
        :test 'equalp)
    (is (upanishad::snapshot-pathnames pool :index)
        #p"~/var/upanishad/test/pool/snapshot-index.xml"
        :test 'equalp)
    (is-error (upanishad::snapshot-pathnames pool :indexs)
              'error)))

(subtest "::make-snapshot-filename" (skip 1 "準備中"))
(subtest "::make-snapshot-pathname" (skip 1 "準備中"))
(subtest "make-snapshot-pathname" (skip 1 "準備中"))
(subtest "make-snapshot-backup-pathname" (skip 1 "準備中"))
(subtest "snapshot-copy-snapshot-file" (skip 1 "準備中"))
(subtest "backup-snapshot" (skip 1 "準備中"))

(subtest "::make-transaction-log-filename" (skip 1 "準備中"))
(subtest "make-transaction-log-pathname" (skip 1 "準備中"))
(subtest "transaction-log-backup-file" (skip 1 "準備中"))
(subtest "snapshot-transaction-log" (skip 1 "準備中"))
(subtest "backup-transaction-log" (skip 1 "準備中"))

(finalize)
