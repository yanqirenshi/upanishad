(defpackage :upanishad-test.slot-index
  (:use :cl
        #:upanishad
        #:prove
        #:upanishad-test.test-utility
        #:upanishad.index)
  (:import-from :upanishad
                #:meme
                #:%id))
(in-package :upanishad-test.slot-index)

(defparameter *test-pool-directory* (test-pool-directory "upanishad-test.slot-index"))

(plan nil)

(subtest ":GET-INDEX-KEY"
  (let ((class-symbol 'meme1)
        (slot-symbol 'slot1))
    (multiple-value-bind (got-class-symbol got-slot-symbol)
        (get-index-key (make-instance 'slot-index-unique
                                      :class-symbol class-symbol
                                      :slot-symbol slot-symbol))
      (is got-class-symbol class-symbol
          "can return class-symbol")
      (is got-slot-symbol slot-symbol
          "can return slot-symbol"))))

(subtest ":MAKE-SLOT-INDEX"

  (subtest "without :objects"
    (subtest "type = :unique"
      (let ((index (make-slot-index 'meme1 'slot1 :unique)))
        (is (class-name (class-of index)) 'slot-index-unique)
        (is (class-symbol index) 'meme1)
        (is (slot-symbol index) 'slot1)
        (is (hash-table-count (value->object index)) 0)))
    (subtest "type = :multiple"
      (let ((index (make-slot-index 'meme1 'slot1 :multiple)))
        (is (class-name (class-of index)) 'slot-index-multiple)
        (is (class-symbol index) 'meme1)
        (is (slot-symbol index) 'slot1)
        (is (hash-table-count (value->objects index)) 0))))

  (subtest "with :objects"
    (let ((meme1-1 (make-instance 'meme1 :%id 1 :slot1 1))
          (meme1-2 (make-instance 'meme1 :%id 2 :slot1 1))
          (meme1-3 (make-instance 'meme1 :%id 3 :slot1 2)))

      (let ((index (make-slot-index 'meme1 'slot1 :unique (list meme1-1 meme1-3))))
        (is (hash-table-count (value->object index)) 2))

      (let ((index (make-slot-index 'meme1 'slot1 :multiple (list meme1-1 meme1-2 meme1-3))))
        (is-%id->value (up.index::%id->value index)
                       '((1 . 1) (2 . 1) (3 . 2))
                       "can update %id->value")
        (is-value->objects (value->objects index)
                           `((1 (,meme1-1 ,meme1-2)) (2 (,meme1-3)))
                           "can update value->objects")))))

(finalize)
