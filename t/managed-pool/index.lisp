(defpackage :upanishad-test.index
  (:use :cl
        #:upanishad
        #:prove
        #:upanishad-test.test-utility
        #:upanishad.index)
  (:import-from :upanishad
                #:meme
                #:%id))
(in-package :upanishad-test.index)

(defparameter *test-pool-directory* (test-pool-directory "managed-pool.pool"))

(defclass test-meme (meme) ())

(plan nil)

(subtest ":index"
  (labels ((is-index (got expected)
             (is (class-symbol got)
                 (getf expected :class-symbol)
                 "class-symbol")
             (is (slot-symbol got)
                 (getf expected :slot-symbol)
                 "slot-symbol")
             (let ((contents (contents got)))
               (subtest "contents"
                 (if (null (getf expected :contents))
                     (progn
                       (ok (hash-table-p contents))
                       (is (hash-table-count contents)
                           0) "is default value")
                     (is contents
                         (getf expected :contents)
                         "is init value"))))))
    (subtest "without init"
      (is-index (make-instance 'index)
                '(:class-symbol nil
                  :slot-symbol nil
                  :contents nil)))

    (subtest "without init"
      (let ((class-symbol 'index)
            (slot-symbol 'contents)
            (contents (make-hash-table)))
        (is-index (make-instance 'index
                                 :class-symbol class-symbol
                                 :slot-symbol slot-symbol
                                 :contents contents)
                  `(:class-symbol ,class-symbol
                    :slot-symbol ,slot-symbol
                    :contents ,contents))))))

(subtest ":get-index-key")

(subtest "::assert-class")

(subtest "::change-meme")

(subtest ":add-meme")

(subtest ":add-memes")

(subtest ":make-index")

(subtest ":remove-meme")

(finalize)
