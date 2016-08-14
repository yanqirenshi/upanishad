(defpackage :upanishad-test.blob
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.blob)

(defparameter *test-pool-directory* (test-pool-directory "blob"))

(plan 10)


(subtest ":print-object" (skip 1 "準備中"))
(subtest ":get-file" (skip 1 "準備中"))
(subtest ":get-size" (skip 1 "準備中"))
(subtest "::copy-stream" (skip 1 "準備中"))
(subtest ":fill-from-stream" (skip 1 "準備中"))
(subtest ":copy-to-stream" (skip 1 "準備中"))
(subtest ":fill-from-file" (skip 1 "準備中"))
(subtest ":destroy" (skip 1 "準備中"))
(subtest "::size-from-file" (skip 1 "準備中"))
(subtest "::set-size-from-file" (skip 1 "準備中"))


(finalize)
