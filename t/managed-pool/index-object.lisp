(defpackage :upanishad-test.managed-pool.index-object
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.index-object)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.index-object"))

(defclass person (meme)
  ((firstname :initarg :firstname :initform "" :accessor get-firstname)
   (lastname  :initarg :lastname  :initform "" :accessor get-lastname)))

(plan 13)

(subtest "::class-%id-indexp"
  (is (up::class-%id-indexp 'objtect-%id-index) t)
  (is (up::class-%id-indexp 'objtect_%id_index) nil))

(subtest "::classname-at"
  (let* ((expected "MEME")
         (test-class 'up:meme)
         (obj (make-instance test-class)))
    (ok (string= (up::classname-at test-class) expected)
        "symbol")
    (ok (string= (up::classname-at (class-of obj)) expected)
        "class")
    (is-error (up::classname-at obj)
              'error
              "class instance")))

(subtest "::get-objects-slot-index-name"
  (is (up::get-objects-slot-index-name 'person) :person-%id-index))

(subtest "::index-at"
  (let* ((object-class 'person)
         (%id-index-name (up::get-objects-slot-index-name object-class)))
    (with-pool (pool *test-pool-directory*)
      (tx-create-%id-counter pool)
      (tx-create-object pool object-class)
      (ok (up::index-at pool :name %id-index-name))
      (is-error (up::index-at pool) 'error))))

(skip 1 "::tx-create-objects-slot-index")
(skip 1 "::tx-remove-objects-slot-index")
(skip 1 "::%add-object-to-slot-index")
(skip 1 "::add-object-to-slot-index")
(skip 1 "::%remove-object-from-slot-index")
(skip 1 "::remove-object-from-slot-index")
(skip 1 ":index-on")
(skip 1 ":drop-index-on")
(skip 1 ":tx-remove-object-on-slot-index")

(finalize)
