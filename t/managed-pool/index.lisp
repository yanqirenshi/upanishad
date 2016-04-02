(defpackage :upanishad-test.managed-pool.index
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.managed-pool.index)

(plan nil)


(subtest "class-%id-indexp"
  (is (up::class-%id-indexp 'objtect-%id-index) t)
  (is (up::class-%id-indexp 'objtect_%id_index) nil))

(subtest "classname-at"
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

(subtest "get-objects-slot-index-name"
  (is (up::get-objects-slot-index-name 'meme) :meme-%id-index))


(finalize)
