(in-package :upanishad.memes)

(defclass memes ()
  ((class-symbol
    :documentation ""
    :accessor class-symbol
    :initarg :class-symbol)
   (meme-list :documentation ""
              :accessor meme-list
              :initarg :meme-list
              :initform nil)
   (%id-index :documentation ""
              :accessor %id-index
              :initarg :%id-index
              :initform (make-hash-table))))

(defun get-meme (memes %id)
  (when (and memes %id)
    (gethash %id (%id-index memes))))

(defgeneric add-meme (memes meme)
  (:method ((memes memes) (meme upanishad:meme))
    (let ((%id (up:%id meme)))
      (when (get-meme memes %id)
        (error "Aledy exist meme"))
      (setf (gethash %id (%id-index memes)) meme)
      (push meme (meme-list memes)))
    memes))

(defgeneric make-memes (class-symbol &key meme-list)
  (:method ((object-symbol symbol) &key meme-list)
    (let ((new-memes (make-instance 'memes
                                    :class-symbol object-symbol)))
      (dolist (meme meme-list)
        (add-meme new-memes meme))
      new-memes)))

(defgeneric remove-meme (memes meme)
  (:method ((memes memes) (meme up:meme))
    (let ((%id (up:%id meme)))
      (unless (get-meme memes %id)
        (error "Not exist meme"))
      (remhash %id (%id-index memes))
      (setf (meme-list memes)
            (remove meme (meme-list memes)))
      memes)))
