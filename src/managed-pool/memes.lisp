(in-package :upanishad.memes)

(defclass memes ()
  ((class-symbol
    :documentation ""
    :accessor class-symbol
    :initarg :class-symbol)
   (contents :documentation ""
             :accessor contents
             :initarg :contents
             :initform nil)
   (%id-ht :documentation ""
           :accessor %id-ht
           :initarg :%id-ht
           :initform (make-hash-table))))

(defgeneric add-meme (memes meme)
  (:method ((memes memes) (meme upanishad:meme))
    (let ((%id-ht (%id-ht memes))
          (%id meme))
      (when (gethash %id %id-ht)
        (error "Aledy exist meme"))
      (setf (gethash %id %id-ht) meme)
      (push meme (contents memes)))
    memes))

(defgeneric make-memes (class-symbol &key contents)
  (:method ((object-symbol symbol) &key contents)
    (let ((new-memes (make-instance 'memes
                                    :class-symbol object-symbol)))
      (dolist (meme contents)
        (add-meme new-memes meme))
      new-memes)))

(defgeneric remove-meme (memes meme)
  (:method ((memes memes) (meme up:meme))
    (let ((%id-ht (%id-ht memes))
          (%id meme))
      (unless (gethash %id %id-ht)
        (error "Not exist meme"))
      (remhash %id %id-ht)
      (remove meme memes))))
