(in-package :upanishad.index)

(defclass index ()
  ((class-symbol :documentation ""
                 :accessor class-symbol
                 :initarg :class-symbol
                 :initform nil)
   (slot-symbol :documentation ""
                :accessor slot-symbol
                :initarg :slot-symbol
                :initform nil)
   (contents :documentation ""
             :accessor contents
             :initarg :contents
             :initform (make-hash-table :test 'equalp)))
  (:documentation ""))

;;;
;;; get-index-key
;;;
(defgeneric get-index-key (index)
  (:method ((index index))
    (values (up.index:class-symbol index)
            (up.index:slot-symbol index))))

;;;
;;; add-meme
;;;
(defun assert-class (class meme)
  (unless (eq class (type-of meme))
    (error "index is not meme's slot index")))

(defun change-meme (index slot meme &key (old-value nil))
  (let ((value (slot-value meme slot))
        (ht (contents index)))
    (let ((old-meme (gethash value ht)))
      (unless (eq meme old-meme)
        (when old-value
          (remhash old-value ht))
        (setf (gethash value ht) meme))))
  index)

(defgeneric add-meme (index meme &key old-value)
  (:method ((index index) meme &key (old-value nil))
    (multiple-value-bind (class slot)
        (get-index-key index)
      (assert-class class meme)
      (change-meme index slot meme :old-value old-value))))

;;;
;;; add-memes
;;;
(defgeneric add-memes (index memes)
  (:method ((index index) memes)
    (dolist (object memes)
      (add-meme index object))))

;;;
;;; make-index
;;;
(defun make-index (class-symbol slot-symbol &optional memes)
  (assert (and (symbolp class-symbol)
               (symbolp slot-symbol)
               (or (null memes) (listp memes))))
  (let ((index (make-instance 'index :object class-symbol
                                     :slot slot-symbol)))
    (when memes
      (add-memes index memes))))

;;;
;;; remove-meme
;;;
(defgeneric remove-meme (index meme)
  (:method ((index index) meme)
    (let ((key (slot-value meme (slot-symbol index))))
      (remhash key (contents index)))
    index))
