(defpackage :upanishad-test.slot-index-unique
  (:use :cl
        #:upanishad
        #:prove
        #:upanishad-test.test-utility
        #:upanishad.index)
  (:import-from :upanishad
                #:meme
                #:%id))
(in-package :upanishad-test.slot-index-unique)

(defparameter *test-pool-directory* (test-pool-directory "upanishad-test.slot-index-unique"))

(plan nil)

(subtest "::CHANGE-OBJECT"
  (let* ((slot-symbol 'slot1)
         (index (make-instance 'slot-index-unique
                               :class-symbol 'meme1
                               :slot-symbol slot-symbol))
         (meme (make-instance 'meme1 :slot1 1)))

    (subtest "befor not exist meme on index"
      (is (change-object index meme)
          index "can return index")
      (let ((value->object (value->object index)))
        (is (hash-table-count value->object) 1
            "index count")
        (is (gethash (slot-value meme slot-symbol)
                     value->object)
            meme "can get meme")))

    (subtest "befor exist meme on index"
      (setf (slot-value meme slot-symbol) 2)
      (is (change-object index meme)
          index "can return index")
      (let ((value->object (value->object index)))
        (is (hash-table-count value->object) 1
            "index count")
        (is (gethash (slot-value meme slot-symbol)
                     value->object)
            meme "can get meme")))))

(subtest ":ADD-OBJECT"
  (let* ((slot-symbol 'slot1)
         (index (make-instance 'slot-index-unique
                               :class-symbol 'meme1
                               :slot-symbol slot-symbol))
         (meme (make-instance 'meme1 :slot1 1)))

    (subtest "not exist"
      (is (add-object index meme)
          index "can return index")
      (is (hash-table-count (value->object index))
          1 "contents count")
      (is (gethash (slot-value meme slot-symbol)
                   (value->object index))
          meme "can get meme"))

    (setf (slot-value meme slot-symbol) 2)
    (subtest "not exist"
      (is (add-object index meme)
          index "can return index")
      (is (hash-table-count (value->object index))
          2 "contents count")
      (is (gethash (slot-value meme slot-symbol)
                   (value->object index))
          meme "can get meme"))

    (subtest "can raise error"
      (is-error (add-object index (make-instance 'meme2))
                'error "bad meme class"))))

(subtest ":ADD-OBJECTS"
  (let* ((slot-symbol 'slot1)
         (index (make-instance 'slot-index-unique
                               :class-symbol 'meme1
                               :slot-symbol slot-symbol))
         (meme1 (make-instance 'meme1 :slot1 1))
         (meme2 (make-instance 'meme1 :slot1 2))
         (memes (list meme1 meme2)))
    (is (add-objects index memes)
        index "can return index")
    (let ((contents (value->object index)))
      (is (hash-table-count contents)
          (length memes) "key count")
      (is (gethash (slot-value meme1 slot-symbol) contents)
          meme1 "can get meme1")
      (is (gethash (slot-value meme2 slot-symbol) contents)
          meme2 "can get meme2"))))

(subtest ":MAKE-SLOT-INDEX"
  (let* ((class-symbol 'meme1)
         (slot-symbol 'slot1)
         (meme1 (make-instance class-symbol :slot1 1))
         (meme2 (make-instance class-symbol :slot1 2))
         (memes (list meme1 meme2)))
    (subtest "without memes"
      (let ((index (make-slot-index class-symbol slot-symbol :unique)))
        (is (type-of index)
            'slot-index-unique "can return index")
        (let ((contents (value->object index)))
          (is (hash-table-count contents)
              0 "key count"))))
    (subtest "with memes"
      (let ((index (make-slot-index class-symbol slot-symbol :unique memes)))
        (is (type-of index)
            'slot-index-unique "can return index")
        (let ((contents (value->object index)))
          (is (hash-table-count contents)
              (length memes) "key count")
          (is (gethash (slot-value meme1 slot-symbol) contents)
              meme1 "can get meme1")
          (is (gethash (slot-value meme2 slot-symbol) contents)
              meme2 "can get meme2"))))
    (subtest "can raise error"
      (is-error (make-slot-index 1 slot-symbol :unique memes)
                'error "class-symbol is not symbol")
      (is-error (make-slot-index class-symbol 1 :unique memes)
                'error "class-symbol is not symbol")
      (is-error (make-slot-index class-symbol slot-symbol :unique 1)
                'error "memes is not list"))))

(subtest ":REMOVE-OBJECT"
  (let* ((class-symbol 'meme1)
         (slot-symbol 'slot1)
         (meme1 (make-instance class-symbol :slot1 1))
         (meme2 (make-instance class-symbol :slot1 2))
         (memes (list meme1 meme2))
         (index (make-slot-index class-symbol slot-symbol :unique memes)))
    (is (remove-object index meme1)
        index "can return index")
    (let ((contents (value->object index)))
      (is (gethash (slot-value meme1 slot-symbol) contents)
          nil "can not get meme1")
      (is (gethash (slot-value meme2 slot-symbol) contents)
          meme2 "can get meme2"))))

(finalize)
