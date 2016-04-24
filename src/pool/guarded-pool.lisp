(in-package :upanishad)
(defmethod execute ((pool guarded-pool) (transaction transaction))
  "Execute a transaction on a pool controlled by a guard"
  (funcall (guard pool)
           #'(lambda () (call-next-method pool transaction))))


(defmethod query ((pool guarded-pool) function &rest args)
  "Execute an exclusive query function on a sytem controlled by a guard"
  (funcall (guard pool)
           #'(lambda () (apply function (cons pool args)))))


(defmethod snapshot ((pool guarded-pool))
  "Make a snapshot of a pool controlled by a guard"
  (funcall (guard pool)
           #'(lambda () (call-next-method pool))))


(defmethod backup ((pool guarded-pool) &key directory)
  "Do a backup on a pool controlled by a guard"
  (funcall (guard pool)
           #'(lambda () (call-next-method pool directory))))


(defmethod restore ((pool guarded-pool))
  "Restore a pool controlled by a guard"
  (funcall (guard pool)
           #'(lambda () (call-next-method pool))))
