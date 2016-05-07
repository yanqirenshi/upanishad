(defpackage :upanishad-test.managed-pool.index
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.index)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.index"))

(defclass person (meme)
  ((firstname :initarg :firstname :initform "" :accessor get-firstname)
   (lastname  :initarg :lastname  :initform "" :accessor get-lastname)))

(plan nil)

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

(subtest "::get-index-name"
  (is (up::get-index-name 'meme) :meme-%id-index))

(subtest "::make-index"
  (ok (hash-table-p (up::make-index)) "can return hash-table"))

(subtest "make-%id-map"
  (ok (hash-table-p (up::make-%id-map)) "can return hash-table"))

(subtest "%index-at"
  (with-pool (pool *test-pool-directory*)
    (tx-create-object pool 'person)

    (subtest "at class and slot"
      (let ((result (up::%index-at pool nil 'person '%id)))
        (ok (hash-table-p result) "index is hash-table")
        (is (hash-table-count result) 1 "hash-table size is 1")))

    (subtest "at name"
      (let ((result (up::%index-at pool :person-%id-index nil nil)))
        (ok (hash-table-p result) "index is hash-table")
        (is (hash-table-count result) 1 "hash-table size is 1")))))

(subtest "::index-at"
  (let* ((object-class 'person)
         (%id-index-name :person-%id-index))
    (with-pool (pool *test-pool-directory*)
      (tx-create-object pool object-class)

      (let ((result (up::index-at pool :name %id-index-name)))
        (ok (hash-table-p result) "index is hash-table")
        (is (hash-table-count result) 1 "hash-table size is 1"))

      (is-error (up::index-at pool) 'error "can rise error"))))

(subtest "::slot-index-at"
  (with-pool (pool *test-pool-directory*)

    (ok (null (upanishad::slot-index-at pool '%id :class 'person)))

    (let ((person (up:tx-create-object pool 'person)))

      (subtest "can return index(hash table)"
        (let ((result (upanishad::slot-index-at pool '%id :class 'person)))
          (ok (hash-table-p result) "at class"))

        (let ((result (upanishad::slot-index-at pool '%id :object person)))
          (ok (hash-table-p result) "at object"))))

    (is-error (upanishad::slot-index-at pool '%id) 'error "can raised error")))

(skip 1 "%add-object-to-index")

(skip 1 "::add-object-to-index")

(skip 1 "tx-create-index-for-objects-slot")

(skip 1 "index-on")

(skip 1 "%remove-object-from-index")

(skip 1 "remove-object-from-index")

(skip 1 "tx-remove-object-on-slot-index")

(skip 1 "tx-remove-objects-slot-index")

(skip 1 "drop-index")

(finalize)
