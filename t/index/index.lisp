(defpackage :upanishad-test.index
  (:use :cl
        #:upanishad
        #:prove
        #:upanishad-test.test-utility
        #:upanishad.index)
  (:import-from :upanishad
                #:meme
                #:%id))
(in-package :upanishad-test.index)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.pool"))

(defclass test-meme-1 (meme)
  ((test-slot-a :documentation ""
                :accessor test-slot-a
                :initarg :test-slot-a
                :initform nil)
   (test-slot-b :documentation ""
                :accessor test-slot-b
                :initarg :test-slot-b
                :initform nil)))

(defclass test-meme-2 (meme)
  ((test-slot-c :documentation ""
                :accessor test-slot-c
                :initarg :test-slot-c
                :initform nil)
   (test-slot-d :documentation ""
                :accessor test-slot-d
                :initarg :test-slot-d
                :initform nil)))

(plan nil)

(subtest ":INDEX"
  (labels ((is-index (got expected)
             (is (class-symbol got)
                 (getf expected :class-symbol)
                 "class-symbol")
             (is (slot-symbol got)
                 (getf expected :slot-symbol)
                 "slot-symbol")
             (let ((contents (contents got)))
               (subtest "contents"
                 (if (null (getf expected :contents))
                     (progn
                       (ok (hash-table-p contents))
                       (is (hash-table-count contents)
                           0) "is default value")
                     (is contents
                         (getf expected :contents)
                         "is init value"))))))
    (subtest "without init"
      (is-index (make-instance 'slot-index-unique)
                '(:class-symbol nil
                  :slot-symbol nil
                  :contents nil)))

    (subtest "without init"
      (let ((class-symbol 'slot-index-unique)
            (slot-symbol 'contents)
            (contents (make-hash-table)))
        (is-index (make-instance 'slot-index-unique
                                 :class-symbol class-symbol
                                 :slot-symbol slot-symbol
                                 :contents contents)
                  `(:class-symbol ,class-symbol
                    :slot-symbol ,slot-symbol
                    :contents ,contents))))))

;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Slot Index
;;;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(subtest ":GET-INDEX-KEY"
  (let ((class-symbol 'test-meme-1)
        (slot-symbol 'test-slot-a))
    (multiple-value-bind (got-class-symbol got-slot-symbol)
        (get-index-key (make-instance 'slot-index-unique
                                      :class-symbol class-symbol
                                      :slot-symbol slot-symbol))
      (is got-class-symbol class-symbol
          "can return class-symbol")
      (is got-slot-symbol slot-symbol
          "can return slot-symbol"))))

(subtest "::ASSERT-CLASS"
  (let* ((meme-class-ok 'test-meme-1)
         (meme-class-ng 'test-meme-2)
         (meme (make-instance meme-class-ok)))
    (is (up.index::assert-class meme-class-ok meme)
        nil "can not raise error")
    (is-error (up.index::assert-class meme-class-ng meme)
              'error "can raise error")))

;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Slot Index multiple
;;;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(subtest "::CHANGE-OBJECT"
  (let* ((slot-symbol 'test-slot-a)
         (index (make-instance 'slot-index-unique
                               :class-symbol 'test-meme-1
                               :slot-symbol slot-symbol))
         (meme (make-instance 'test-meme-1 :test-slot-a 1)))
    (subtest "befor not exist meme on index"
      (is (up.index::change-object index slot-symbol meme)
          index "can return index")
      (let ((contents (contents index)))
        (is (hash-table-count contents) 1
            "index count")
        (is (gethash (slot-value meme slot-symbol)
                     contents)
            meme "can get meme")))
    (subtest "befor exist meme on index"
      (setf (slot-value meme slot-symbol) 2)
      (is (up.index::change-object index slot-symbol meme :old-value 1)
          index "can return index")
      (let ((contents (contents index)))
        (is (hash-table-count contents) 1
            "index count")
        (is (gethash (slot-value meme slot-symbol)
                     contents)
            meme "can get meme")))))

(subtest ":ADD-OBJECT"
  (let* ((slot-symbol 'test-slot-a)
         (index (make-instance 'slot-index-unique
                               :class-symbol 'test-meme-1
                               :slot-symbol slot-symbol))
         (meme (make-instance 'test-meme-1 :test-slot-a 1)))
    (subtest "not exist"
      (is (add-object index meme)
          index "can return index")
      (is (hash-table-count (contents index))
          1 "contents count")
      (is (gethash (slot-value meme slot-symbol)
                   (contents index))
          meme "can get meme"))
    (setf (slot-value meme slot-symbol) 2)
    (subtest "not exist"
      (is (add-object index meme)
          index "can return index")
      (is (hash-table-count (contents index))
          2 "contents count")
      (is (gethash (slot-value meme slot-symbol)
                   (contents index))
          meme "can get meme"))
    (subtest "can raise error"
      (is-error (add-object index (make-instance 'test-meme-2))
                'error "bad meme class"))))

(subtest ":ADD-OBJECTS"
  (let* ((slot-symbol 'test-slot-a)
         (index (make-instance 'slot-index-unique
                               :class-symbol 'test-meme-1
                               :slot-symbol slot-symbol))
         (meme1 (make-instance 'test-meme-1 :test-slot-a 1))
         (meme2 (make-instance 'test-meme-1 :test-slot-a 2))
         (memes (list meme1 meme2)))
    (is (add-objects index memes)
        index "can return index")
    (let ((contents (contents index)))
      (is (hash-table-count contents)
          (length memes) "key count")
      (is (gethash (slot-value meme1 slot-symbol) contents)
          meme1 "can get meme1")
      (is (gethash (slot-value meme2 slot-symbol) contents)
          meme2 "can get meme2"))))

(subtest ":MAKE-SLOT-INDEX"
  (let* ((class-symbol 'test-meme-1)
         (slot-symbol 'test-slot-a)
         (meme1 (make-instance class-symbol :test-slot-a 1))
         (meme2 (make-instance class-symbol :test-slot-a 2))
         (memes (list meme1 meme2)))
    (subtest "without memes"
      (let ((index (make-slot-index class-symbol slot-symbol :unique)))
        (is (type-of index)
            'slot-index-unique "can return index")
        (let ((contents (contents index)))
          (is (hash-table-count contents)
              0 "key count"))))
    (subtest "with memes"
      (let ((index (make-slot-index class-symbol slot-symbol :unique memes)))
        (is (type-of index)
            'slot-index-unique "can return index")
        (let ((contents (contents index)))
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
  (let* ((class-symbol 'test-meme-1)
         (slot-symbol 'test-slot-a)
         (meme1 (make-instance class-symbol :test-slot-a 1))
         (meme2 (make-instance class-symbol :test-slot-a 2))
         (memes (list meme1 meme2))
         (index (make-slot-index class-symbol slot-symbol :unique memes)))
    (is (remove-object index meme1)
        index "can return index")
    (let ((contents (contents index)))
      (is (gethash (slot-value meme1 slot-symbol) contents)
          nil "can not get meme1")
      (is (gethash (slot-value meme2 slot-symbol) contents)
          meme2 "can get meme2"))))


;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Slot Index multiple
;;;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(subtest ":slot-index-multiple")

(subtest "::make-object->object"
  (let ((object->object (up.index::make-object->object)))
    (is (type-of object->object) 'hash-table "can return hash table")
    (is (hash-table-test object->object) 'eql ":test is 'eql")
    (is (hash-table-count object->object) 0 "count is zero")))

(subtest "::remove-on-object->object"
  (let ((meme1 (make-instance 'test-meme-1))
        (meme2 (make-instance 'test-meme-2))
        (object->object (up.index::make-object->object)))
    (setf (gethash meme1 object->object) meme1)
    (setf (gethash meme2 object->object) meme2)

    (subtest "before"
      (ok (gethash meme1 object->object) "can return meme1")
      (ok (gethash meme2 object->object) "can return meme2"))

    (subtest "remove meme1"
      (is (up.index::remove-on-object->object object->object meme1)
          object->object "can return object->object")
      (is (gethash meme1 object->object)
          nil "can not return meme1")
      (ok (gethash meme2 object->object) "can return meme2"))

    (subtest "re remove meme1"
      (is (up.index::remove-on-object->object object->object meme1)
          object->object "can return object->object")
      (is (gethash meme1 object->object)
          nil "can not return meme1")
      (ok (gethash meme2 object->object) "can return meme2"))))

(subtest "::ensure-object->object"
  (let* ((object->object-1 (make-hash-table))
         (value->objects
           (alexandria:alist-hash-table `(("1" .  ,object->object-1))
                                        :test 'equalp)))
    (subtest "can return object->object"
      (is (up.index::ensure-object->object value->objects "1")
          object->object-1 "exist object->object")
      (subtest "not exist object->object"
        (let ((object->object-2 (up.index::ensure-object->object value->objects "2")))
          (is (type-of object->object-2) 'hash-table
              "can return hashtable")
          (is (hash-table-test object->object-2) 'eql
              "test is eql")
          (is (hash-table-count object->object-2) 0
              "count is 0"))))))

(subtest "::remove-on-index-core")
(subtest "::add-on-index-core")
(subtest "::change-on-index-core")
(subtest ":add-object")
(subtest ":add-objects")
(subtest ":remove-object")

(finalize)
