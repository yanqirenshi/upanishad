(in-package :upanishad.index)

(defmethod get-index-key ((slot-index slot-index))
  (values (class-symbol slot-index)
          (slot-symbol slot-index)))

(defun make-slot-index (class-symbol slot-symbol type &optional objects)
  (assert (and (symbolp class-symbol)
               (symbolp slot-symbol)
               (symbolp type)
               (or (null objects) (listp objects))))
  (let ((index (make-instance (get-slot-index-class type)
                              :class-symbol class-symbol
                              :slot-symbol slot-symbol)))
    (when objects
      (add-objects index objects))
    index))
