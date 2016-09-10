(in-package :upanishad.index)

(defclass index () ())

;;;
;;; slot-index
;;;
(defclass slot-index (index)
  ((class-symbol :documentation ""
                 :accessor class-symbol
                 :initarg :class-symbol
                 :initform nil)
   (slot-symbol :documentation ""
                :accessor slot-symbol
                :initarg :slot-symbol
                :initform nil)))

(defclass slot-index-unique (slot-index)
  ((contents :documentation ""
             :accessor contents
             :initarg :contents
             :initform (make-hash-table :test 'equalp)))
  (:documentation ""))

(defclass slot-index-multiple (slot-index)
  ((contents :documentation ""
             :accessor contents
             :initarg :contents
             :initform (make-hash-table :test 'equalp)))
  (:documentation ""))

;;;
;;; get-index-key
;;;
(defgeneric get-index-key (slot-index-unique)
  (:method ((slot-index-unique slot-index-unique))
    (values (up.index:class-symbol slot-index-unique)
            (up.index:slot-symbol slot-index-unique))))

;;;
;;; add-meme
;;;
(defun assert-class (class meme)
  (unless (eq class (type-of meme))
    (error "index is not meme's slot index")))

(defun change-meme (slot-index-unique slot meme &key (old-value nil))
  (let ((value (slot-value meme slot))
        (ht (contents slot-index-unique)))
    (let ((old-meme (gethash value ht)))
      (unless (eq meme old-meme)
        (when old-value
          (remhash old-value ht))
        (setf (gethash value ht) meme))))
  slot-index-unique)

(defgeneric add-meme (index meme)
  (:method ((slot-index-unique slot-index-unique) meme)
    (multiple-value-bind (class slot)
        (get-index-key slot-index-unique)
      (assert-class class meme)
      (change-meme slot-index-unique slot meme))))

;;;
;;; add-memes
;;;
(defgeneric add-memes (index memes)
  (:method ((slot-index-unique slot-index-unique) memes)
    (dolist (object memes)
      (add-meme slot-index-unique object))
    slot-index-unique))

;;;
;;; make-index
;;;
(defun make-index (class-symbol slot-symbol &optional memes)
  (assert (and (symbolp class-symbol)
               (symbolp slot-symbol)
               (or (null memes) (listp memes))))
  (let ((slot-index-unique (make-instance 'slot-index-unique :class-symbol class-symbol
                                                             :slot-symbol slot-symbol)))
    (when memes
      (add-memes slot-index-unique memes))
    slot-index-unique))

;;;
;;; remove-meme
;;;
(defgeneric remove-meme (index meme)
  (:method ((slot-index-unique slot-index-unique) meme)
    (let ((key (slot-value meme (slot-symbol slot-index-unique))))
      (remhash key (contents slot-index-unique)))
    slot-index-unique))
