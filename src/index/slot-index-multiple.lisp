(in-package :upanishad.index)

;;;;;
;;;;; Slot Index multiple
;;;;;
(defclass slot-index-multiple (slot-index)
  ((value->objects :documentation ""
                   :accessor value->objects
                   :initarg :value->objects
                   :initform (make-hash-table :test 'equalp)))
  (:documentation ""))

;;;
;;; object->object
;;;
(defun make-object->object ()
  (make-hash-table))

(defun remove-on-object->object (object->object object)
  (when (gethash object object->object)
    (remhash object object->object))
  object->object)

(defun ensure-object->object (value->objects value)
  (let ((object->object (gethash value value->objects)))
    (or object->object
        (setf (gethash value value->objects)
              (make-object->object)))))

;;;
;;; core
;;;
(defun add-on-index-core (value->objects value object->value object)
  (setf (gethash object object->value) value)
  (let ((object->object (ensure-object->object value->objects value)))
    (unless (gethash object object->object)
      (setf (gethash object object->object) object))))

(defun remove-on-index-core (value->objects value object->value object)
  (remhash object object->value)
  (let ((object->object (gethash object value->objects)))
    (remove-on-object->object object->object object)
    (when (= (hash-table-count object->object) 0)
      (remhash value value->objects))))

(defun change-on-index-core (value->objects value-old value-new
                             object->value object)
  (unless (equalp value-old value-new)
    (remove-on-index-core value->objects value-old
                          object->value object))
  (unless (gethash value-new value->objects)
    (add-on-index-core value->objects value-old
                       object->value object)))

;;;
;;; add-object
;;;
(defmethod add-object ((index slot-index-multiple) object)
  (multiple-value-bind (class slot)
      (get-index-key index)
    (assert-class class object)
    (let ((value->objects (value->objects index))
          (object->value  (object->value  index)))
      (change-on-index-core value->objects
                            (gethash object object->value)
                            (slot-value slot object)
                            object->value
                            object)))
  index)

;;;
;;; add-objects
;;;
(defmethod add-objects ((index slot-index-multiple) objects)
  (dolist (object objects)
    (add-object index object)))

;;;
;;; remove-meme
;;;
(defmethod remove-object ((index slot-index-multiple) object)
  (multiple-value-bind (class)
      (get-index-key index)
    (assert-class class object)
    (let ((value->objects (value->objects index))
          (object->value  (object->value  index)))
      (remove-on-index-core value->objects (gethash object object->value)
                            object->value object)))
  index)
