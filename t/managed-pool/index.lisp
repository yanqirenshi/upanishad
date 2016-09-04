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

(finalize)
