(in-package :upanishad.index)

#|

# Contents

1. Index
1. Slot Index
1. Slot Index Unique
1. Slot Index multiple

|#

;;;;;
;;;;; Index
;;;;;
(defclass index () ())

(defgeneric get-index-key (index))
(defgeneric add-meme (index meme))
(defgeneric add-memes (index memes))
(defgeneric remove-meme (index meme))

;;;;;
;;;;; Slot Index
;;;;;
(defclass slot-index (index)
  ((class-symbol :documentation ""
                 :accessor class-symbol
                 :initarg :class-symbol
                 :initform nil)
   (slot-symbol :documentation ""
                :accessor slot-symbol
                :initarg :slot-symbol
                :initform nil)
   (object->value :documentation ""
                  :accessor object->value
                  :initarg :object->value
                  :initform (make-hash-table))))

;;;
;;; make-index
;;;
(defmethod get-index-key ((slot-index slot-index))
  (values (class-symbol slot-index)
          (slot-symbol slot-index)))

;;;
;;; make-index
;;;
(defun get-slot-index-class (type)
  (ecase type
    (:unique 'slot-index-unique)
    (:multiple 'slot-index-multiple)))

(defun make-slot-index (class-symbol slot-symbol type &optional memes)
  (assert (and (symbolp class-symbol)
               (symbolp slot-symbol)
               (symbolp type)
               (or (null memes) (listp memes))))
  (let ((index (make-instance (get-slot-index-class type)
                              :class-symbol class-symbol
                              :slot-symbol slot-symbol)))
    (when memes
      (add-memes index memes))
    index))

;;;;;
;;;;; Slot Index Unique
;;;;;
(defclass slot-index-unique (slot-index)
  ((contents :documentation ""
             :accessor contents
             :initarg :contents
             :initform (make-hash-table :test 'equalp))
   (valu->object :documentation ""
                 :accessor value->object
                 :initarg :value->object
                 :initform (make-hash-table)))
  (:documentation ""))

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

(defmethod add-meme ((index slot-index-unique) meme)
  (multiple-value-bind (class slot)
      (get-index-key index)
    (assert-class class meme)
    (change-meme index slot meme)))

;;;
;;; add-memes
;;;
(defmethod add-memes ((index slot-index-unique) memes)
  (dolist (object memes)
    (add-meme index object))
  index)

;;;
;;; remove-meme
;;;
(defmethod remove-meme ((index slot-index-unique) meme)
  (let ((key (slot-value meme (slot-symbol index))))
    (remhash key (contents index)))
  index)


;;;;;
;;;;; Slot Index multiple
;;;;;
(defclass slot-index-multiple (slot-index)
  ((value->objects :documentation ""
                   :accessor value->objects
                   :initarg :value->objects
                   :initform (make-hash-table)))
  (:documentation ""))

(defun remove-on-index-core (value->objects value object->value object)
  (remhash object object->value)
  (remhash value value->objects))

(defun add-on-index-core (value->objects value object->value object)
  (setf (gethash object object->value) value)
  (setf (gethash value value->objects) object))

(defmethod add-meme ((index slot-index-multiple) object)
  (multiple-value-bind (class slot)
      (get-index-key index)
    (assert-class class object)
    (let* ((value->objects (value->objects index))
           (object->value (object->value index))
           (value-new     (slot-value slot object))
           (value-old     (gethash object object->value)))
      (unless (equalp value-old value-new)
        (remove-on-index-core value->objects value-old
                              object->value object))
      (unless (gethash value-new value->objects)
        (add-on-index-core value->objects value-old
                           object->value object))))
  index)

(defmethod add-memes ((index slot-index-multiple) objects)
  (dolist (object objects)
    (add-meme index object)))

(defmethod remove-meme ((index slot-index-multiple) object)
  (multiple-value-bind (class)
      (get-index-key index)
    (assert-class class object)
    (let* ((value->objects (value->objects index))
           (object->value (object->value index))
           (value         (gethash object object->value)))
      (remove-on-index-core value->objects value
                            object->value object)))
  index)
