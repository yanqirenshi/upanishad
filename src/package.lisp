(in-package :cl-user)

(defpackage :upanishad
  (:use #:cl)
  (:nicknames :up)
  (:import-from :upanishad.meme
                #:%id
                #:meme)
  (:import-from :upanishad.pool
                #:make-pool
                #:stop)
  (:export #:%id
           #:meme
           ;; from :upanishad.pool
           #:make-pool
           #:stop)
  (:documentation ""))
