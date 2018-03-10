(in-package :upanishad.pool)

(defmethod close-open-streams ((pool pool) &key abort)
  "Close all open stream associated with pool (optionally aborting operations in progress)"
  (with-slots (transaction-log-stream) pool
    (when transaction-log-stream
      (close transaction-log-stream :abort abort)
      (setf transaction-log-stream nil))))

(defmethod stop ((pool pool) &key abort)
  (close-open-streams pool :abort abort))

(defun datastore-files (pool)
  (directory (merge-pathnames (make-pathname :name :wild :type (file-extension pool))
                              (get-directory pool))))

(defmethod delete-all-files (pool)
  (when (probe-file (get-directory pool))
    (dolist (pathname (datastore-files pool))
      (delete-file pathname))))

(defmethod clear-objects (pool)
  (clrhash (root-objects pool))
  (clrhash (index-objects pool)))

(defmethod totally-destroy ((pool pool) &key abort)
  "Totally destroy pool from permanent storage by deleting any files used by the pool, remove all root objects"
  (stop pool :abort abort)
  (delete-all-files pool)
  (clear-objects pool))
