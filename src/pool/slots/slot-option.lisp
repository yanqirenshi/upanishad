(in-package :upanishad.pool)

(defmethod get-option ((pool pool) name)
  (with-slots (options) pool
    (gethash name options)))

(defmethod (setf get-option) (value (pool pool) name)
  (with-slots (options) pool
    (setf (gethash name options) value)))
