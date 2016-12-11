(defpackage :upanishad-test.managed-pool.pool-indexes
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.pool-indexes)

(defparameter *test-pool-directory*
  (test-pool-directory "managed-pool.pool-indexes"))

(plan nil)

(defclass meme1 (up::meme)
  ((slot1 :accessor slot1 :initargs slot1 :initform nil)
   (slot2 :accessor slot2 :initargs slot2 :initform nil)))

(defclass meme2 (up::meme)
  ((slot1 :accessor slot1 :initargs slot1 :initform nil)
   (slot2 :accessor slot2 :initargs slot2 :initform nil)))

(subtest "tx-add-index -> tx-remove-index with get-index "
  (with-pool (pool *test-pool-directory*)
    (let ((index1 (up.index:make-slot-index 'meme1 'slot1 :unique))
          (index2 (up.index:make-slot-index 'meme1 'slot2 :unique))
          (index3 (up.index:make-slot-index 'meme2 'slot2 :unique)))
      (subtest "can add index"
        (is (tx-add-index pool index1) index1 "meme1 slot1")
        (is (tx-add-index pool index2) index2 "meme1 slot2")
        (is (tx-add-index pool index3) index3 "meme2 slot2"))

      (subtest "test result of tx-add-index "
        (ok (get-index pool 'meme1 'slot1) "can return index1")
        (ok (get-index pool 'meme1 'slot2) "can return index2")
        (is (get-index pool 'meme2 'slot1) nil "can not return index")
        (ok (get-index pool 'meme2 'slot2) "can return index3"))

      (subtest "can remove index"
        (is (tx-remove-index pool index1) pool
            "meme1 slot1")
        (is (tx-remove-index pool index3) pool
            "meme2 slot2"))

      (subtest "test result of tx-remove-index "
        (is (get-index pool 'meme1 'slot1) nil "can not return index1")
        (ok (get-index pool 'meme1 'slot2) "can return index2")
        (is (get-index pool 'meme2 'slot1) nil "can not return index")
        (is (get-index pool 'meme2 'slot2) nil "can not return index3")))))

(finalize)
