(in-package :upanishad.index)

(defclass index ()
  ((class-symbol
    :documentation ""
    :accessor class-symbol
    :initarg :class-symbol)
   (slot-symbol
    :documentation ""
    :accessor slot-symbol
    :initarg :slot-symbol)
   (contents
    :documentation ""
    :accessor contents
    :initform (make-hash-table :test 'equalp)))
  (:documentation ""))

(defmethod add-object ((index index) object)
  (list index object))

(defmethod add-objects ((index index) objects)
  (dolist (object objects)
    (add-object index object)))

(defun make-index (object-symbol slot-symbol &optional objects)
  (assert (and (symbolp object-symbol)
               (symbolp slot-symbol)
               (or (null objects) (listp objects))))
  (let ((index (make-instance 'index :object object-symbol :slot slot-symbol)))
    (when objects
      (add-objects index objects))))

(defmethod remove-object ((index index) object)
  (let ((key (slot-value object (slot-symbol index))))
    (remhash key (contents index)))
  index)

