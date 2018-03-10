(in-package :upanishad.pool)

(defmethod poolp ((pool pool)) t)
(defmethod poolp (other) nil)

(defun make-pool (directory &key (pool-class 'pool) init-args)
  "Create and return a new prevalence system on directory. When the
  directory contains a valid snapshot and/or transaction log file, the
  system will be restored. Optionally specify the prevalence system's
  class."
  (apply #'make-instance pool-class :directory directory init-args))

(defmethod initialize-instance :after ((pool pool) &rest initargs &key &allow-other-keys)
  "After a pool is initialized, derive its file paths and try to restore it"
  (declare (ignore initargs))
  (with-slots (directory) pool
    (ensure-directories-exist directory)
    (init-snapshot-pathname pool directory)
    (init-transaction-log-pathname pool directory))
  (restore pool))
