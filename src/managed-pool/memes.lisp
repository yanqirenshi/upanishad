(in-package :upanishad.objects)

(defclass memes (meme)
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
      (push meme (contents memes)))))

(defgeneric remove-meme (memes meme)
  (:method ((memes memes) (meme up:meme))
    (let ((%id-ht (%id-ht memes))
          (%id meme))
      (unless (gethash %id %id-ht)
        (error "Not exist meme"))
      (remhash %id %id-ht)
      (remove meme memes))))

(defun make-memes (object-symbol &key memes)
  (let ((new-memes (make-instance 'memes :object-symbol object-symbol)))
    (dolist (meme memes)
      (add-object new-memes meme))))
