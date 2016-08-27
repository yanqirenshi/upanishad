(in-package :upanishad)

;;;
;;; Pool
;;;
(defmethod initialize-instance :after ((pool pool) &rest initargs &key &allow-other-keys)
  "After a pool is initialized, derive its file paths and try to restore it"
  (declare (ignore initargs))
  (with-slots (directory) pool
    (ensure-directories-exist directory)
    (setf (snapshot-pathnames pool :object) (make-snapshot-pathname pool directory :object)
          (snapshot-pathnames pool :index) (make-snapshot-pathname pool directory :index)
          (transaction-log pool) (make-transaction-log-pathname pool directory)))
  (restore pool))

(defmethod transaction-log-stream :before ((pool pool))
  (with-slots (transaction-log-stream) pool
    (unless transaction-log-stream
      (setf transaction-log-stream (open (transaction-log pool)
                                         :direction :output
                                         :if-does-not-exist :create
                                         #+ccl :sharing #+ccl nil
                                                    :if-exists :append)))))

(defmethod poolp ((pool pool)) t)

(defmethod poolp (other) nil)

;;;
;;; Root objects
;;;
(defmethod get-root-object ((pool pool) name)
  (gethash name (root-objects pool)))

(defmethod (setf get-root-object) (value (pool pool) name)
  (setf (gethash name (root-objects pool)) value))

(defmethod remove-root-object ((pool pool) name)
  (remhash name (root-objects pool)))

;;;
;;; Index objects
;;;
(defmethod get-index-object ((pool pool) name)
  (gethash name (index-objects pool)))

(defmethod (setf get-index-object) (value (pool pool) name)
  (setf (gethash name (index-objects pool)) value))

(defmethod remove-index-object ((pool pool) name)
  (remhash name (index-objects pool)))

;;;
;;; Option
;;;
(defmethod get-option ((pool pool) name)
  (with-slots (options) pool
    (gethash name options)))

(defmethod (setf get-option) (value (pool pool) name)
  (with-slots (options) pool
    (setf (gethash name options) value)))

;;;
;;; Print
;;;
(defmethod print-object ((transaction transaction) stream)
  (print-unreadable-object (transaction stream :type t :identity t)
    (format stream "~a ~a"
            (get-function transaction)
            (or (args transaction) "()"))))

;;;
;;; execute
;;;
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


;;;
;;; query
;;;
(defmethod query ((pool pool) function &rest args)
  "Execute an exclusive query function on a sytem"
  (apply function (cons pool args)))


;;;
;;; closign
;;;
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


;;;
;;; Make
;;;
(defun make-pool (directory &key (pool-class 'pool) init-args)
  "Create and return a new prevalence system on directory. When the
  directory contains a valid snapshot and/or transaction log file, the
  system will be restored. Optionally specify the prevalence system's
  class."
  (apply #'make-instance pool-class :directory directory init-args))

(defun make-transaction (function &rest args)
  "Create and return a new transaction specifying a function name and
  an argument list. The function should accept the pool instance
  prepended to the argument list as arguments and implement the actual
  transaction in a re-entrant way."
  (make-instance 'transaction :function function :args args))
