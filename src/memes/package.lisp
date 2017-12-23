(in-package :cl-user)

(defpackage :upanishad.memes
  (:use :cl)
  (:nicknames :up.memes)
  (:import-from :upanishad.meme
                #:meme
                #:%id)
  (:import-from :upanishad.index
                #:make-slot-index
                #:get-at-value
                #:add-object
                #:remove-object)
  (:export #:memes
           #:meme-class
           #:meme-list
           #:%id-index
           #:get-meme
           #:add-meme
           #:make-memes
           #:remove-meme)
  (:documentation ""))

(in-package :upanishad.memes)
