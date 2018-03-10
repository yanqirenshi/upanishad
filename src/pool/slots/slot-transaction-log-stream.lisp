(in-package :upanishad.pool)

(defmethod transaction-log-stream :before ((pool pool))
  (with-slots (transaction-log-stream) pool
    (unless transaction-log-stream
      (setf transaction-log-stream (open (transaction-log pool)
                                         :direction :output
                                         :if-does-not-exist :create
                                         #+ccl :sharing #+ccl nil
                                                    :if-exists :append)))))
