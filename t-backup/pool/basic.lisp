(defpackage :upanishad-test.pool.basic
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.pool.basic)

(defparameter *test-pool-directory* (test-pool-directory "pool"))

(plan nil)

(subtest ":make-pool" (skip 1 "準備中"))
(subtest ":make-transaction" (skip 1 "準備中"))
(subtest "::poolp" (skip 2 "準備中"))
(subtest ":get-root-object" (skip 2 "準備中"))
(subtest ":get-option" (skip 2 "準備中"))
(subtest ":initiates-rollback(condition)" (skip 1 "準備中"))
(subtest ":initiates-rollback(no-rollback-error)" (skip 1 "準備中"))
(subtest "::initialize-instance" (skip 1 "準備中"))
(subtest "::transaction-log-stream" (skip 1 "準備中"))
(subtest "::close-open-streams" (skip 1 "準備中"))
(subtest ":totally-destroy" (skip 1 "準備中"))
(subtest ":print-object" (skip 1 "準備中"))
(subtest ":remove-root-object" (skip 1 "準備中"))
(subtest ":execute" (skip 1 "準備中"))
(subtest "::log-transaction" (skip 1 "準備中"))
(subtest "::log-transaction :after" (skip 1 "準備中"))
(subtest ":query" (skip 1 "準備中"))
(subtest "::execute-on" (skip 1 "準備中"))
(subtest ":snapshot" (skip 1 "準備中"))
(subtest ":backup" (skip 1 "準備中"))
(subtest ":restore" (skip 1 "準備中"))
(subtest ":execute(guarded-pool)" (skip 1 "準備中"))
(subtest ":query(guarded-pool)" (skip 1 "準備中"))
(subtest ":snapshot(guarded-pool)" (skip 1 "準備中"))
(subtest ":backup(guarded-pool)" (skip 1 "準備中"))
(subtest ":restore(guarded-pool)" (skip 1 "準備中"))
(subtest ":stop" (skip 1 "準備中"))
(subtest "::timetag" (skip 1 "準備中"))
(subtest "::make-transaction-log-filename" (skip 1 "準備中"))
(subtest "::make-snapshot-filename" (skip 1 "準備中"))
(subtest "::truncate-file" (skip 1 "準備中"))
(subtest "::copy-file" (skip 1 "準備中"))
(subtest "::reset-known-slots" (skip 1 "準備中"))

(finalize)
