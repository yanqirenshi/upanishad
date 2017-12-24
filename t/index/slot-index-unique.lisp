(in-package :upanishad-test)

(plan nil)

(diag "slot-index-multiple.lisp")

(with-pool (pool *test-pool-directory*)
  (subtest "add-object"
    (let ((index (upanishad.index:make-slot-index 'test-meme 'name :unique))
          (meme (make-instance 'test-meme)))
      (is (upanishad.index:add-object index meme)
          index)
      (skip 1 "wait ....")))

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
    (skip 1 "wait ....")))

(finalize)
