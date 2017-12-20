(defpackage :upanishad-test.index-utility
  (:use :cl
        #:upanishad
        #:prove
        #:upanishad-test.test-utility
        #:upanishad.index)
  (:import-from :upanishad
                #:meme
                #:%id))
(in-package :upanishad-test.index-utility)

(defparameter *test-pool-directory* (test-pool-directory "upanishad-test.index-utility"))

(plan nil)

(subtest "::GET-SLOT-INDEX-CLASS" 
  (is (upanishad.index::get-slot-index-class :unique) 'slot-index-unique)
  (is (upanishad.index::get-slot-index-class :multiple) 'slot-index-multiple)
  (is-error (upanishad.index::get-slot-index-class :not-found) 'error))

(subtest "::ASSERT-CLASS"
  (let* ((meme-class-ok 'meme1)
         (meme-class-ng 'meme2)
         (meme (make-instance meme-class-ok)))
    (is (up.index::assert-class meme-class-ok meme)
        nil "can not raise error")
    (is-error (up.index::assert-class meme-class-ng meme)
              'error "can raise error")))

(finalize)
