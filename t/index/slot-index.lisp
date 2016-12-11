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

(subtest "::ASSERT-CLASS"
  (let* ((meme-class-ok 'meme1)
         (meme-class-ng 'meme2)
         (meme (make-instance meme-class-ok)))
    (is (up.index::assert-class meme-class-ok meme)
        nil "can not raise error")
    (is-error (up.index::assert-class meme-class-ng meme)
              'error "can raise error")))

(finalize)
