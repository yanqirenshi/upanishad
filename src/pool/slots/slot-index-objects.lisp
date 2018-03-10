(in-package :upanishad.pool)

(defmethod get-index-object ((pool pool) name)
  (gethash name (index-objects pool)))

(defmethod (setf get-index-object) (value (pool pool) name)
  (setf (gethash name (index-objects pool)) value))

(defmethod remove-index-object ((pool pool) name)
  (remhash name (index-objects pool)))
