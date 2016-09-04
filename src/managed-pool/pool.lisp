(in-package :upanishad)

;;;;;
;;;;; indexes
;;;;;
(defgeneric get-slot-index (pool class slot)
  (:method ((pool pool) (class symbol) (slot symbol))
    (let* ((class-ht (indexes pool))
           (slot-ht (gethash class class-ht)))
      (when slot-ht
        (gethash slot slot-ht)))))

(defun %tx-add-slot-index (indexes class slot index)
  (let* ((class-ht indexes)
         (slot-ht (alexandria:ensure-gethash class class-ht
                                             (make-hash-table))))
    (when (gethash slot slot-ht)
      (error "aledy exist index"))
    (setf (gethash slot slot-ht) index)))

(defgeneric tx-add-index (pool index)
  (:method ((pool pool) (index up.index:index))
    (multiple-value-bind (class slot)
        (up.index:get-index-key index)
      (let ((indexes (indexes pool)))
        (when (get-slot-index pool class slot)
          (error "Aledy exist index"))
        (%tx-add-slot-index indexes class slot index)))))

(defgeneric tx-remove-index (pool index)
  (:method ((pool pool) (index up.index:index))
    (multiple-value-bind (class slot)
        (up.index:get-index-key index)
      (let ((slot-ht (get-slot-index pool class slot)))
        (when slot-ht
          (remhash slot slot-ht)
          index)))))

;;;
;;; index
;;;
(defgeneric tx-add-meme-to-slot-index (pool slot meme)
  (:method ((pool pool) (slot symbol) (meme meme))
    (let ((index (get-slot-index pool (type-of meme) slot)))
      (unless index (error "index not found"))
      (up.index:add-meme index meme))))

(defgeneric tx-remove-meme-from-slot-index (pool slot meme)
  (:method ((pool pool) (slot symbol) (meme meme))
    (let ((index (get-slot-index pool (type-of meme) slot)))
      (when index
        (up.index:remove-meme index meme)))))

;;;;;
;;;;; memes
;;;;;
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
(defgeneric get-meme-at-%id (pool class %id)
  (:method ((pool pool) (class symbol) %id)
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

(defgeneric tx-change-meme-slots (pool class %id alist-slots-values)
  (:method ((pool pool) (class symbol) (%id integer) (alist-slots-values list))
    (let ((meme (get-meme-at-%id pool class %id)))
      (unless meme
        (error "no meme of class ~a with %id ~d found in ~s" class %id pool))
      (loop :for (slot value) :in alist-slots-values
            :do (when (slot-value-changed-p meme slot value)
                  (tx-remove-meme-from-slot-index pool slot meme)
                  (setf (slot-value meme slot) value)
                  (tx-add-meme-to-slot-index pool slot meme)))
      meme)))

;;;
;;; tx-create-meme
;;;
(defgeneric tx-create-meme (pool class &optional alist-slots-values)
  (:method ((pool pool) class &optional alist-slots-values)
    (let* ((%id (next-%id pool))
           (meme (make-instance class :%id %id))
           (memes (get-memes-at pool :class class)))
      (up.memes:add-meme memes meme)
      (tx-change-meme-slots pool class %id alist-slots-values))))

;;;
;;; tx-delete-meme
;;;
(defgeneric tx-delete-meme (pool class target)
  (:method ((pool pool) (class symbol) (%id integer))
    (let ((memes (get-memes-at pool :class class))
          (meme (get-meme-at-%id pool class %id)))
      (if meme
          (progn
            ;; TODO: remove all index
            (up.memes:remove-meme memes meme))
          (error "no meme of class ~a with %id ~d found in ~s" class %id pool)))))
