(in-package :upanishad-test)

(plan nil)

(diag "slot-index-multiple.lisp")

(subtest "add-object"
  (with-pool (pool *test-pool-directory*)
    (let ((index (upanishad.index:make-slot-index 'test-meme '%id :unique))
          (meme1 (make-instance 'test-meme :%id 1))
          (meme2 (make-instance 'test-meme :%id 2)))
      (is (upanishad.index:add-object index meme1)
          index "can add meme(%id=1)")
      (subtest "exist meme"
        (is-error (upanishad.index:add-object index meme)
                  'error "can not add meme(%id=1)")
        (is (upanishad.index:add-object index meme2)
            index "can add meme(%id=2)")))))

(subtest "add-objects"
  (skip 1 "wait ...."))

(subtest "remove-object"
  (skip 1 "wait ...."))

(subtest "change-object-remove"
  (skip 1 "wait ...."))

(subtest "change-object-add"
  (skip 1 "wait ...."))

(subtest "change-object"
  (skip 1 "wait ...."))

(subtest "get-at-value"
  (skip 1 "wait ...."))

(finalize)
