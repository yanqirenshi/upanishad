(in-package :upanishad)

(defmethod add-index-new ((pool pool) (index index))
  (let ((class (class-symbol index))
        (slot (slot-symbol index)))
    (ensure-gethash slot
                    (ensure-gethash class
                                    (index-objects pool)
                                    (make-hash-table))
                    index))
  index)

(defmethod drop-index-new ((pool pool) (index index))
  (let* ((class (class-symbol index))
         (slot (slot-symbol index))
         (ht-class (gethash class (index-objects pool))))
    (when ht-class
      (gethash slot ht-class))
    (list pool index)))

;;;
;;; Pool
;;;
(defmethod index-at-new ((pool pool) &key class object slot (ensure nil))
  (cond ((and class slot)
         (let ((index (gethash slot (gethash class (index-objects pool)))))
           (or index
               (when ensure
                 (add-index-new pool (upanishad.index:make-slot-index object slot))))))
        ((and object slot)
         (index-at pool :class (class-name (class-of object)) :slot slot))
        (t (error "bad parameter"))))
