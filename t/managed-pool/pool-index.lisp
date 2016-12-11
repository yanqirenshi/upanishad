(defpackage :upanishad-test.managed-pool.pool-index
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.pool-index)

(defparameter *test-pool-directory*
  (test-pool-directory "managed-pool.pool-indexes"))

(plan nil)

(defclass meme1 (up::meme)
  ((slot1 :accessor slot1 :initarg :slot1 :initform nil)
   (slot2 :accessor slot2 :initarg :slot2 :initform nil)))

(defclass meme2 (up::meme)
  ((slot1 :accessor slot1 :initarg :slot1 :initform nil)
   (slot2 :accessor slot2 :initarg :slot2 :initform nil)))

(subtest "tx-add-index -> tx-remove-index with get-index "
  (with-pool (pool *test-pool-directory*)
    (let ((index1 (up.index:make-slot-index 'meme1 'slot1 :unique))
          (index2 (up.index:make-slot-index 'meme2 'slot1 :unique))
          (meme1 (make-instance 'meme1 :slot1 "1"))
          (meme2 (make-instance 'meme2 :slot1 "1")))

      (subtest "can add index"
        (is (tx-add-index pool index1) index1 "meme1 slot1")
        (is (tx-add-index pool index2) index2 "meme2 slot1"))

      (ok (tx-add-meme-to-index pool 'slot1 meme1))
      (ok (tx-add-meme-to-index pool 'slot1 meme2))

      (subtest "remove meme"
        (tx-remove-meme-from-index pool 'slot1 meme1)
        (tx-remove-meme-from-index pool 'slot1 meme2))
      )))

(finalize)
