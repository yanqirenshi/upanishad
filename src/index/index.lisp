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
                :initform nil)))

(defmethod get-index-key ((slot-index slot-index))
  (values (class-symbol slot-index)
          (slot-symbol slot-index)))

;;;;;
;;;;; Slot Index Unique
;;;;;
(defclass slot-index-unique (slot-index)
  ((contents :documentation ""
             :accessor contents
             :initarg :contents
             :initform (make-hash-table :test 'equalp)))
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
;;; make-index
;;;
(defun make-slot-index (class-symbol slot-symbol &optional memes)
  (assert (and (symbolp class-symbol)
               (symbolp slot-symbol)
               (or (null memes) (listp memes))))
  (let ((index (make-instance 'slot-index-unique :class-symbol class-symbol
                                                 :slot-symbol slot-symbol)))
    (when memes
      (add-memes index memes))
    index))

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
  ((contents :documentation ""
             :accessor contents
             :initarg :contents
             :initform (make-hash-table :test 'equalp)))
  (:documentation ""))
