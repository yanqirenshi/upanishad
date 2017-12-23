(in-package :upanishad.memes)

(defclass memes ()
  ((meme-class
    :documentation ""
    :accessor meme-class
    :initarg :meme-class)
   (meme-list :documentation ""
              :accessor meme-list
              :initarg :meme-list
              :initform nil)
   (%id-index :documentation ""
              :accessor %id-index
              :initarg :%id-index
              :initform nil))
  (:documentation ""))
