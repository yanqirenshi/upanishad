(defpackage :upanishad-test.pool.serialize-files
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.pool.serialize-files)

(defparameter *test-pool-directory* (test-pool-directory "upanishad-test.pool.serialize-files"))

(plan nil)

(subtest "TIMETAG" (skip 1 "..."))

(subtest "TMP-PATHNAME" (skip 1 "..."))

(subtest "TRUNCATE-FILE" (skip 1 "..."))

(subtest "COPY-FILE" (skip 1 "..."))

(subtest "::SNAPSHOT-TYPE-P"
  (ok (upanishad::snapshot-type-p :object))
  (ok (null (upanishad::snapshot-type-p :objects)))
  (ok (upanishad::snapshot-type-p :index)))

(subtest "SNAPSHOT-PATHNAMES"
  (with-pool (pool *test-pool-directory*)
    (is (upanishad::snapshot-pathnames pool :object)
        (merge-pathnames "snapshot-object.xml" *test-pool-directory*)
        :test 'equalp)
    (is (upanishad::snapshot-pathnames pool :index)
        (merge-pathnames "snapshot-index.xml" *test-pool-directory*)
        :test 'equalp)
    (is-error (upanishad::snapshot-pathnames pool :indexs)
              'error)))

(subtest "MAKE-SNAPSHOT-FILENAME" (skip 1 "..."))

(subtest "MAKE-SNAPSHOT-PATHNAME" (skip 1 "..."))

(subtest "MAKE-SNAPSHOT-BACKUP-PATHNAME" (skip 1 "..."))

(subtest "SNAPSHOT-COPY-SNAPSHOT-FILE" (skip 1 "..."))

(subtest "BACKUP-SNAPSHOT" (skip 1 "..."))

(subtest "MAKE-TRANSACTION-LOG-FILENAME" (skip 1 "..."))

(subtest "MAKE-TRANSACTION-LOG-PATHNAME" (skip 1 "..."))

(subtest "TRANSACTION-LOG-BACKUP-FILE" (skip 1 "..."))

(subtest "SNAPSHOT-TRANSACTION-LOG" (skip 1 "..."))

(subtest "BACKUP-TRANSACTION-LOG" (skip 1 "..."))

(finalize)
