(defpackage :upanishad-test.slot-index-multiple
  (:use :cl
        #:upanishad
        #:prove
        #:upanishad-test.test-utility
        #:upanishad.index)
  (:import-from :upanishad
                #:meme
                #:%id))
(in-package :upanishad-test.slot-index-multiple)

(defparameter *test-pool-directory* (test-pool-directory "upanishad-test.slot-index-multiple"))

(plan nil)

(subtest "::slot-index-multiple-contexts" (skip 1 "wait"))

(subtest "::ensure-%id->object" (skip 1 "wait"))

(subtest "::add-object-add-multi" (skip 1 "wait"))

(subtest ":add-object" (skip 1 "wait"))

(subtest ":add-objects" (skip 1 "wait"))

(subtest "::add-object-remove-multi" (skip 1 "wait"))

(subtest ":remove-object" (skip 1 "wait"))

(subtest ":change-object" (skip 1 "wait"))

(finalize)
