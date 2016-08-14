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

(defun get-meme (memes %id)
  (when (and memes %id)
    (gethash %id (%id-ht memes))))

(defgeneric add-meme (memes meme)
  (:method ((memes memes) (meme upanishad:meme))
    (let ((%id (up:%id meme)))
      (when (get-meme memes %id)
        (error "Aledy exist meme"))
      (setf (gethash %id (%id-ht memes)) meme)
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
    (let ((%id meme))
      (unless (get-meme memes %id)
        (error "Not exist meme"))
      (remhash %id (%id-ht memes))
      (remove meme memes))))
