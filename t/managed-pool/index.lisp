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
      (is-index (make-instance 'index)
                '(:class-symbol nil
                  :slot-symbol nil
                  :contents nil)))

    (subtest "without init"
      (let ((class-symbol 'index)
            (slot-symbol 'contents)
            (contents (make-hash-table)))
        (is-index (make-instance 'index
                                 :class-symbol class-symbol
                                 :slot-symbol slot-symbol
                                 :contents contents)
                  `(:class-symbol ,class-symbol
                    :slot-symbol ,slot-symbol
                    :contents ,contents))))))

(subtest ":GET-INDEX-KEY"
  (let ((class-symbol 'test-meme-1)
        (slot-symbol 'test-slot-a))
    (multiple-value-bind (got-class-symbol got-slot-symbol)
        (get-index-key (make-instance 'index
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

(subtest "::CHANGE-MEME")

(subtest ":ADD-MEME")

(subtest ":ADD-MEMES")

(subtest ":MAKE-INDEX")

(subtest ":REMOVE-MEME")

(finalize)
