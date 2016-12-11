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
              :initform (make-hash-table))))

(defun get-meme (memes &key %id slot value)
  (assert memes)
  (cond (%id (gethash %id (%id-index memes)))
        (slot (first (find-if #'(lambda (meme)
                                  (equalp (slot-value meme slot)
                                          value))
                              memes)))
        (t nil)))

(defgeneric add-meme (memes meme)
  (:method ((memes memes) (meme upanishad:meme))
    (let ((%id (up:%id meme)))
      (when (get-meme memes :%id %id)
        (error "Aledy exist meme"))
      (setf (gethash %id (%id-index memes)) meme)
      (push meme (meme-list memes)))
    memes))

(defgeneric make-memes (meme-class &key meme-list)
  (:method ((object-symbol symbol) &key meme-list)
    (let ((new-memes (make-instance 'memes
                                    :meme-class object-symbol)))
      (dolist (meme meme-list)
        (add-meme new-memes meme))
      new-memes)))

(defgeneric remove-meme (memes meme)
  (:method ((memes memes) (meme up:meme))
    (let ((%id (up:%id meme)))
      (unless (get-meme memes :%id %id)
        (error "Not exist meme"))
      (remhash %id (%id-index memes))
      (setf (meme-list memes)
            (remove meme (meme-list memes)))
      memes)))
