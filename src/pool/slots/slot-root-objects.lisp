(in-package :upanishad.pool)

(defmethod get-root-object ((pool pool) name)
  (gethash name (root-objects pool)))

(defmethod (setf get-root-object) (value (pool pool) name)
  (setf (gethash name (root-objects pool)) value))

(defmethod remove-root-object ((pool pool) name)
  (remhash name (root-objects pool)))
