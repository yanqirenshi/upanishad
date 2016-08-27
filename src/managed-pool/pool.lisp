(in-package :upanishad)

;;;
;;; memes
;;;
(defgeneric tx-add-memes (pool memes)
  (:method ((pool pool) (memes up.memes:memes))
    (let ((ht (memes pool))
          (key (up.memes:meme-class memes)))
      (when (gethash key ht)
        (error ""))
      (setf (gethash key ht) memes)
      memes))
  (:method ((pool pool) (class symbol))
    (let ((memes (make-instance 'up.memes:memes
                                :meme-class class)))
      (tx-add-memes pool memes))))

(defgeneric tx-remove-memes (pool memes)
  (:method ((pool pool) (memes up.memes:memes))
    (tx-remove-memes pool (up.memes:meme-class memes)))
  (:method ((pool pool) (class symbol))
    (let ((ht (memes pool))
          (key class))
      (when (gethash key ht)
        (remhash key ht)))))

(defgeneric get-memes-at (pool &key class)
  (:method ((pool pool) &key class)
    (let ((ht (memes pool))
          (key class))
      (gethash key ht))))

;;;;;
;;;;; meme
;;;;;
(defgeneric get-meme-at-%id (pool class &key %id)
  (:method ((pool pool) (class symbol) &key %id)
    (let ((memes (get-memes-at pool :class class)))
      (up.memes:get-meme memes %id))))

;;;
;;; find-meme
;;;
(defgeneric find-meme (pool class &key slot value test))

;;;
;;; tx-change-meme-slots
;;;
(defun slot-value-changed-p (object slot value)
  (or (not (slot-boundp object slot))
      (not (eql (slot-value object slot) value))))

(defgeneric tx-change-meme-slots (pool class %id slots-and-values)
  (:method ((pool pool) (class symbol) (%id integer) (slots-and-values list))
    (let ((meme (get-meme-at-%id pool class :%id %id)))
      (unless meme
        (error "no meme of class ~a with %id ~d found in ~s" class %id pool))
      (loop :for (slot value) :in slots-and-values
            :do (when (slot-value-changed-p meme slot value)
                  ;; TODO: (remove-meme-from-index pool class slot meme)
                  (setf (slot-value meme slot) value)
                  ;; TODO: (add-meme-to-index pool class slot meme)
                  ))
      meme)))

;;;
;;; tx-create-meme
;;;
(defgeneric tx-create-meme (pool class &optional slots-and-values)
  (:method ((pool pool) class &optional slots-and-values)
    (let* ((%id (next-%id pool))
           (meme (make-instance class :%id %id))
           (memes (get-memes-at pool :class class)))
      (up.memes:add-meme memes meme)
      (tx-change-meme-slots pool class %id slots-and-values))))

;;;
;;; tx-delete-meme
;;;
(defgeneric tx-delete-meme (pool class %id)
  (:method ((pool pool) (class symbol) (%id integer))
    (let ((memes (get-memes-at pool :class class))
          (meme (get-meme-at-%id pool class :%id %id)))
      (if meme
          (progn
            ;; TODO: remove all index
            (up.memes:remove-meme memes meme))
          (error "no meme of class ~a with %id ~d found in ~s" class %id pool)))))
