(in-package :upanishad.memes)

(defun make-%id-index (class)
  (make-slot-index class '%id :unique))

(defun %make-memes (object-symbol)
  (make-instance 'memes
                 :meme-class object-symbol
                 :%id-index (make-%id-index object-symbol)))

(defun add-memes (memes meme-list)
  (dolist (meme meme-list)
    (add-meme memes meme))
  memes)

(defgeneric make-memes (meme-class &key meme-list)
  (:method ((object-symbol symbol) &key meme-list)
    (add-memes (%make-memes object-symbol)
               meme-list))
  (:documentation ""))
