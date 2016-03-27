(in-package :upanishad)

(defmethod get-preference ((pool pool) key)
  "Retrieve the value of the persistent preference stored under key in pool"
  (let ((preferences (get-root-object pool :preferences)))
    (when preferences
      (gethash key preferences))))

(defmethod tx-set-preference ((pool pool) key value)
  "Set the value of the persistent preference key in pool"
  (let ((preferences (get-root-object pool :preferences)))
    (when (not preferences)
      (setf preferences (make-hash-table)
            (get-root-object pool :preferences) preferences))
    (setf (gethash key preferences) value)))

(defmethod all-preferences-keys ((pool pool))
  "Return a list of all persistent preference keys of pool"
  (let ((preferences (get-root-object pool :preferences)))
    (when preferences
      (let (keys)
        (maphash #'(lambda (key value)
                     (declare (ignore value))
                     (push key keys))
                 preferences)
        keys))))
