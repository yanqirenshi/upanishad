(in-package :upanishad.index)

;;;
;;; add-object
;;;
(defun change-object (slot-index-unique slot meme &key (old-value nil))
  (let ((value (slot-value meme slot))
        (ht (contents slot-index-unique)))
    (let ((old-meme (gethash value ht)))
      (unless (eq meme old-meme)
        (when old-value
          (remhash old-value ht))
        (setf (gethash value ht) meme))))
  slot-index-unique)

(defmethod add-object ((index slot-index-unique) meme)
  (multiple-value-bind (class slot)
      (get-index-key index)
    (assert-class class meme)
    (change-object index slot meme)))

;;;
;;; add-objects
;;;
(defmethod add-objects ((index slot-index-unique) objects)
  (dolist (object objects)
    (add-object index object))
  index)

;;;
;;; remove-meme
;;;
(defmethod remove-object ((index slot-index-unique) meme)
  (let ((key (slot-value meme (slot-symbol index))))
    (remhash key (contents index)))
  index)
