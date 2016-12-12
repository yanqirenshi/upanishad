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

(defclass meme1 (meme)
  ((slot1 :documentation ""
          :accessor slot1 :initarg :slot1 :initform nil)
   (slot2 :documentation ""
          :accessor slot2 :initarg :slot2 :initform nil)))

(defclass meme2 (meme)
  ((slot1 :documentation ""
          :accessor slot1 :initarg :slot1 :initform nil)
   (slot2 :documentation ""
          :accessor slot2 :initarg :slot2 :initform nil)))

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