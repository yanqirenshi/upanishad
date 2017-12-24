(in-package :upanishad.index)

;;;
;;; add-object
;;;
(defmethod add-object ((index slot-index-unique) meme)
  "すでに存在する場合はエラーとする。
同じ value - object が存在する場合はエラーにせず正常終了する。"
  (multiple-value-bind (class slot)
      (get-index-key index)
    (assert-class class meme)
    (let* ((val->obj (value->object index))
           (val (slot-value meme slot)))
      (when (eq (gethash val val->obj) meme)
        (error "exist!"))
      (setf (gethash val val->obj) meme)))
  index)

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
  (multiple-value-bind (class slot)
      (get-index-key index)
    (assert-class class meme)
    (remhash (slot-value meme slot)
             (value->object index))
    index))

;;;
;;; change-object
;;;
(defun change-object-remove (index value meme)
  (let ((value->object (value->object index))
        (%id->value (%id->value index))
        (%id (%id meme)))
    ;; remove: value->object
    (remhash value value->object)
    ;; remove: %id->value
    (remhash %id %id->value)))

(defun change-object-add (index value meme)
  (let ((value->object (value->object index))
        (%id->value (%id->value index))
        (%id (%id meme)))
    ;; add: value->object
    (setf (gethash value value->object) meme)
    ;; add: %id->value
    (setf (gethash %id %id->value) value)))

(defmethod change-object ((index slot-index-unique) meme)
  "contents のキーはバリューだな。"
  (multiple-value-bind (class slot)
      (get-index-key index)
    (assert-class class meme)
    (let ((old-val (gethash (%id meme) (%id->value index)))
          (new-val (slot-value meme slot)))
      (cond ((null old-val)
             (change-object-add index new-val meme))
            ((not (equalp old-val new-val))
             (change-object-remove index old-val meme)
             (change-object-add index new-val meme)))))
  index)

;;;
;;;
;;;
(defmethod get-at-value ((index slot-index-unique) value)
  (gethash value (value->object index)))
