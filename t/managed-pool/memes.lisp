(defpackage :upanishad-test.memes
  (:use :cl
        #:upanishad
        #:prove
        #:upanishad-test.test-utility
        #:upanishad.memes)
  (:import-from :upanishad
                #:meme
                #:%id))
(in-package :upanishad-test.memes)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.pool"))

(defclass test-meme (meme) ())

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

(subtest "get-meme"
  (let ((memes (make-instance 'memes))
        (meme (make-instance 'test-meme :%id 1)))
    (add-meme memes meme)
    (get-meme memes 1)))

(subtest "add-meme"
  (let ((memes (make-instance 'memes))
        (meme (make-instance 'test-meme :%id 1)))
    (is (add-meme memes meme)
        memes "can return memes")
    (is (get-meme memes 1)
        meme "can return meme")))

(subtest "make-memes"
  (let ((memes (make-memes 'test-meme)))
    (ok memes "can return memes instance")
    (is (contents memes) nil "contents is empty"))

  (let* ((contents (list (make-instance 'test-meme :%id 1)))
         (memes (make-memes 'test-meme :contents contents)))
    (ok memes "can return memes instance")
    (is (mapcar #'%id (contents memes))
        (mapcar #'%id contents)
        :test 'equalp
        "can return contents")))

(subtest "remove-meme"
  (let* ((meme1 (make-instance 'test-meme :%id 1))
         (meme2 (make-instance 'test-meme :%id 2))
         (memes (make-memes 'memes
                            :contents (list meme1 meme2))))
    (subtest "before remove-meme"
      (is (length (contents memes)) 2)
      (is (hash-table-count (%id-ht memes)) 2)
      (ok (get-meme memes 1))
      (ok (get-meme memes 2)))
    (subtest "submit remove-meme"
      (is (remove-meme memes meme1)
          memes "can return memes"))
    (subtest "after remove-meme"
      (is (length (contents memes)) 1)
      (is (hash-table-count (%id-ht memes)) 1)
      (ok (not (get-meme memes 1)))
      (ok (get-meme memes 2)))))

(finalize)
