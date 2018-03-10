(in-package :upanishad.pool)

(define-condition no-rollback-error (error)
  ()
  (:documentation "Thrown by code inside a transaction to indicate that no rollback is needed"))

(defmethod initiates-rollback ((condition condition))
  t)

(defmethod initiates-rollback ((no-rollback-error no-rollback-error))
  nil)

(defmethod log-transaction ((pool pool) (transaction transaction))
  "Log transaction for pool"
  (let ((out (transaction-log-stream pool)))
    (funcall (serializer pool) transaction out (serialization-state pool))
    (terpri out)
    (finish-output out)))

(defmethod log-transaction :after ((pool pool) (transaction transaction))
  "Execute the transaction-hook"
  (funcall (transaction-hook pool) transaction))

(defmethod execute-on ((transaction transaction) (pool pool))
  "Execute a transaction itself in the context of a pool"
  (apply (get-function transaction)
         (cons pool (args transaction))))

(defmethod execute ((pool pool) (transaction transaction))
  "Execute a transaction on a pool and log it to the transaction log"
  (let ((result (multiple-value-list
                 (handler-bind ((error #'(lambda (condition)
                                           (when (and (get-option pool :rollback-on-error)
                                                      (initiates-rollback condition))
                                             (format *standard-output*
                                                     ";; Notice: pool rollback/restore due to error (~a)~%"
                                                     condition)
                                             (restore pool)))))
                   (execute-on transaction pool)))))
    (log-transaction pool transaction)
    (apply #'values result)))
