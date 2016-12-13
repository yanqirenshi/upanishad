(in-package :upanishad)

;;;
;;; get-meme
;;;
(defun get-meme-at-slot (pool class slot value)
  (let ((index (get-index pool class slot)))
    (if index
        (up.index:get-at-value index value)
        (up.memes:get-meme (get-memes pool :class class)
                           :slot slot
                           :value value))))

(defgeneric get-meme (pool class &key %id slot value)
  (:method ((pool pool) (class symbol) &key %id slot value)
    (let ((memes (get-memes pool :class class)))
      (cond (%id (up.memes:get-meme memes :%id %id))
            (slot (get-meme-at-slot pool class slot value))
            (t nil))))
  (:documentation ""))

;;;
;;; find-meme
;;;
(defgeneric find-meme (pool class &key slot value test)
  (:documentation ""))

;;;
;;; tx-change-meme-slots
;;;
(defun slot-value-changed-p (object slot value)
  (or (not (slot-boundp object slot))
      (not (eql (slot-value object slot) value))))

(defgeneric tx-change-meme-slots (pool class %id slots-and-values)
  (:method ((pool pool) (class symbol) (%id integer) (slots-and-values list))
    (let ((meme (get-meme pool class :%id %id)))
      (unless meme
        (error "no meme of class ~a with %id ~d found in ~s" class %id pool))
      (loop :for (slot value) :in slots-and-values
            :do (when (slot-value-changed-p meme slot value)
                  (tx-remove-meme-from-index pool slot meme)
                  (setf (slot-value meme slot) value)
                  (tx-add-meme-to-index pool slot meme)))
      meme))
  (:documentation ""))

;;;
;;; tx-create-meme
;;;
(defgeneric tx-create-meme (pool class &optional slots-and-values)
  (:method ((pool pool) class &optional slots-and-values)
    (let* ((%id (next-%id pool))
           (meme (make-instance class :%id %id))
           (memes (get-memes pool :class class)))
      (up.memes:add-meme memes meme)
      (tx-change-meme-slots pool class %id slots-and-values)))
  (:documentation ""))

;;;
;;; tx-delete-meme
;;;
(defgeneric tx-delete-meme (pool class %id)
  (:method ((pool pool) (class symbol) (%id integer))
    (let ((memes (get-memes pool :class class))
          (meme (get-meme pool class :%id %id)))
      (if meme
          (progn
            ;; TODO: remove all index
            (up.memes:remove-meme memes meme))
          (error "no meme of class ~a with %id ~d found in ~s" class %id pool))))
  (:documentation ""))
