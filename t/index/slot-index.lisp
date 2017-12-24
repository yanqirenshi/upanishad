(in-package :upanishad-test)

(plan nil)

(diag "slot-index.lisp")

(subtest "make-slot-index"
  (subtest ":unique"
    (let ((index (upanishad.index:make-slot-index 'test-meme 'name :unique)))
      (is 'upanishad.index:slot-index-unique
          (class-name (class-of index)))

      (is (upanishad.index:class-symbol index)
          'test-meme)

      (is (upanishad.index:slot-symbol index)
          'name)

      (is 'hash-table
          (type-of (upanishad.index::%id->value index)))

      (is 'hash-table
          (type-of (upanishad.index:value->object index)))))

  (subtest ":multiple"
    (let ((index (upanishad.index:make-slot-index 'test-meme 'name :multiple)))
      (is 'upanishad.index:slot-index-multiple
          (class-name (class-of index)))

      (is (upanishad.index:class-symbol index)
          'test-meme)

      (is (upanishad.index:slot-symbol index)
          'name)

      (is 'hash-table
          (type-of (upanishad.index::%id->value index)))

      (is 'hash-table
          (type-of (upanishad.index:value->objects index))))))

(subtest "get-index-key"
  (subtest ":unique"
    (let ((index (upanishad.index:make-slot-index 'test-meme 'name :unique)))
      (is (multiple-value-list
           (upanishad.index:get-index-key index))
          '(test-meme name))))
  (subtest ":multiple"
    (let ((index (upanishad.index:make-slot-index 'test-meme 'name :multiple)))
      (is (multiple-value-list
           (upanishad.index:get-index-key index))
          '(test-meme name)))))

(finalize)
