(defpackage :upanishad-test.managed-pool.pool
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility)
  (:import-from :up.index
                #:make-index))
(in-package :upanishad-test.managed-pool.pool)

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

;;;;;
;;;;; indexes
;;;;;
(subtest "::GET-SLOT-INDEX"
  (let ((class-symbol 'test-meme-1)
        (slot-symbol 'test-slot-b)
        (class-symbol-ng 'test-meme-2)
        (slot-symbol-ng1 'test-slot-a)
        (slot-symbol-ng2 'test-slot-c))
    (with-pool (pool *test-pool-directory* :with-id-counter nil)
      (let ((index (make-index class-symbol slot-symbol)))
        (up::tx-add-index pool index)
        (is (up::get-slot-index pool class-symbol slot-symbol)
            index "can return index")
        (is (up::get-slot-index pool class-symbol slot-symbol-ng1)
            nil "can not return index")
        (is (up::get-slot-index pool class-symbol-ng slot-symbol-ng2)
            nil "can not return index")))))

(subtest "::%TX-ADD-SLOT-INDEX"
  (let ((class-symbol 'test-meme-1)
        (slot-symbol 'test-slot-b)
        (class-symbol-ng 'test-meme-2)
        (slot-symbol-ng1 'test-slot-a)
        (slot-symbol-ng2 'test-slot-c))
    (with-pool (pool *test-pool-directory* :with-id-counter nil)
      (let ((indexes (indexes pool))
            (index (make-index class-symbol slot-symbol)))
        (subtest "add index"
          (is (up::%tx-add-slot-index indexes class-symbol slot-symbol index)
              index "can return index"))
        (subtest "after add index"
          (is (up::get-slot-index pool class-symbol slot-symbol)
              index "can return index")
          (subtest "can not return index"
            (is (up::get-slot-index pool class-symbol slot-symbol-ng1)
                nil "bad slot-symbol")
            (is (up::get-slot-index pool class-symbol-ng slot-symbol-ng2)
                nil "bad class-symbol")))
        (subtest "can raise error"
          (is-error (up::%tx-add-slot-index indexes class-symbol slot-symbol index)
                   'error "aledy exist indexe"))))))

(subtest "::TX-ADD-INDEX"
  (let ((class-symbol 'test-meme-1)
        (slot-symbol 'test-slot-b)
        (class-symbol-ng 'test-meme-2)
        (slot-symbol-ng1 'test-slot-a)
        (slot-symbol-ng2 'test-slot-c))
    (with-pool (pool *test-pool-directory* :with-id-counter nil)
      (let ((index (make-index class-symbol slot-symbol)))
        (subtest "add index"
          (is (up::tx-add-index pool index)
              pool "can return pool"))
        (subtest "after add index"
          (is (up::get-slot-index pool class-symbol slot-symbol)
              index "can return index")
          (subtest "can not return index"
            (is (up::get-slot-index pool class-symbol slot-symbol-ng1)
                nil "bad slot-symbol")
            (is (up::get-slot-index pool class-symbol-ng slot-symbol-ng2)
                nil "bad class-symbol")))
        (subtest "can raise error"
          (is-error (up::tx-add-index pool index)
                    'error "aledy exist indexe"))))))

(subtest "::TX-REMOVE-INDEX"
  (let ((class-symbol-1 'test-meme-1)
        (class-symbol-2 'test-meme-2)
        (slot-symbol-a 'test-slot-a)
        (slot-symbol-b 'test-slot-b)
        (slot-symbol-c 'test-slot-c))
    (with-pool (pool *test-pool-directory* :with-id-counter nil)
      (let ((index1 (make-index class-symbol-1 slot-symbol-a))
            (index2 (make-index class-symbol-1 slot-symbol-b))
            (index3 (make-index class-symbol-2 slot-symbol-c)))
        (up::tx-add-index pool index1)
        (up::tx-add-index pool index2)
        (up::tx-add-index pool index3)
        (up::tx-remove-index pool index2)
        (is (up::get-slot-index pool class-symbol-1 slot-symbol-b)
            nil "can not return deleted index")
        (subtest "can return not deleted index"
          (is (up::get-slot-index pool class-symbol-1 slot-symbol-a)
              index1)
          (is (up::get-slot-index pool class-symbol-2 slot-symbol-c)
              index3))))))

;;;;;
;;;;; index
;;;;;
(subtest "::TX-ADD-MEME-TO-SLOT-INDEX")
(subtest "::TX-REMOVE-MEME-FROM-SLOT-INDEX")

;;;;;
;;;;; memes
;;;;;
(subtest ":TX-ADD-MEMES")
(subtest ":TX-REMOVE-MEMES")
(subtest ":GET-MEMES-AT")

;;;;;
;;;;; meme
;;;;;
(subtest ":GET-MEME-AT-%ID")
(subtest "::FIND-MEME")
(subtest "::SLOT-VALUE-CHANGED-P")
(subtest ":TX-CHANGE-MEME-SLOTS")
(subtest ":TX-CREATE-MEME")
(subtest ":TX-DELETE-MEME")

(finalize)
