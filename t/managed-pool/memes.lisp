(defpackage :upanishad-test.memes
  (:use :cl
        #:upanishad
        #:prove
        #:upanishad-test.test-utility
        #:upanishad.memes))
(in-package :upanishad-test.memes)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.pool"))

(plan nil)

(subtest "memes"
  (let* ((contents '(1 2 3))
         (class-symbol 'number)
         (memes (make-instance 'memes
                               :class-symbol class-symbol
                               :contents contents)))
    (ok memes "can make-isntace")
    (is (class-symbol memes)
        class-symbol
        "can return class-symbol")
    (is (contents memes)
        contents
        "can return contents")
    (subtest "can return %id-ht"
      (let ((%id-ht (%id-ht memes)))
        (is (type-of %id-ht)
            'hash-table "type is hash table")
        (is (hash-table-count %id-ht)
            0 "empty")))))

(subtest "add-meme" (skip 1 "準備中"))

(subtest "remove-meme" (skip 1 "準備中"))

(subtest "make-memes" (skip 1 "準備中"))

(finalize)
