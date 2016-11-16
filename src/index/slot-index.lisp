(in-package :upanishad.index)

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

(defun assert-class (class meme)
  (unless (eq class (type-of meme))
    (error "index is not meme's slot index")))

;;;;;
;;;;; Slot Index Unique
;;;;;
(defclass slot-index-unique (slot-index)
  ((contents :documentation ""
             :accessor contents
             :initarg :contents
             :initform (make-hash-table :test 'equalp))
   (value->object :documentation ""
                  :accessor value->object
                  :initarg :value->object
                  :initform (make-hash-table :test 'equalp)))
  (:documentation ""))

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
