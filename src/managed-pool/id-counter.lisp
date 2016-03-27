(in-package :upanishad)

(defmethod tx-create-%id-counter ((pool pool))
  (setf (get-root-object pool :%id-counter) 0))

(defmethod next-%id ((pool pool))
  (incf (get-root-object pool :%id-counter)))
