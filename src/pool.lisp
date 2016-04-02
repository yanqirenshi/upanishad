;;;;;
;;;;; Contents
;;;;;  1. Public API: Functions and Generic Functions
;;;;;  2. Generic functions
;;;;;  3. Conditions
;;;;;  4. Implementation
;;;;;  5. Some utilities
;;;;;  6. Some file manipulation utilities
;;;;;  7. from the serialization package
;;;;;  8. extra documentation
;;;;;

(in-package :upanishad)

;;;
;;; 1. Public API: Functions and Generic Functions
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



;;;
;;; 2. Generic functions
;;;
(defmethod poolp ((pool pool)) t)

(defmethod poolp (other) nil)


(defmethod get-root-object ((pool pool) name)
  (gethash name (root-objects pool)))

(defmethod (setf get-root-object) (value (pool pool) name)
  (setf (gethash name (root-objects pool)) value))


(defmethod get-index-object ((pool pool) name)
  (gethash name (index-objects pool)))

(defmethod (setf get-index-object) (value (pool pool) name)
  (setf (gethash name (index-objects pool)) value))


(defmethod get-option ((pool pool) name)
  (with-slots (options) pool
    (gethash name options)))

(defmethod (setf get-option) (value (pool pool) name)
  (with-slots (options) pool
    (setf (gethash name options) value)))



;;;
;;; 3. Conditions
;;;
(define-condition no-rollback-error (error)
  ()
  (:documentation "Thrown by code inside a transaction to indicate that no rollback is needed"))


(defmethod initiates-rollback ((condition condition))
  t)


(defmethod initiates-rollback ((no-rollback-error no-rollback-error))
  nil)



;;;
;;; 4. Implementation
;;;
(defmethod initialize-instance :after ((pool pool) &rest initargs &key &allow-other-keys)
  "After a pool is initialized, derive its file paths and try to restore it"
  (declare (ignore initargs))
  (with-slots (directory) pool
    (ensure-directories-exist directory)
    (setf (get-snapshot pool) (merge-pathnames (make-pathname :name (get-snapshot-filename pool)
                                                              :type (file-extension pool))
                                               directory)
          (transaction-log pool) (merge-pathnames (make-pathname :name (get-transaction-log-filename pool)
                                                                 :type (file-extension pool))
                                                  directory)))
  (restore pool))


(defmethod transaction-log-stream :before ((pool pool))
  (with-slots (transaction-log-stream) pool
    (unless transaction-log-stream
      (setf transaction-log-stream (open (transaction-log pool)
                                         :direction :output
                                         :if-does-not-exist :create
                                         #+ccl :sharing #+ccl nil
                                                    :if-exists :append)))))


(defmethod close-open-streams ((pool pool) &key abort)
  "Close all open stream associated with pool (optionally aborting operations in progress)"
  (with-slots (transaction-log-stream) pool
    (when transaction-log-stream
      (close transaction-log-stream :abort abort)
      (setf transaction-log-stream nil))))


(defmethod totally-destroy ((pool pool) &key abort)
  "Totally destroy pool from permanent storage by deleting any files used by the pool, remove all root objects"
  (close-open-streams pool :abort abort)
  (when (probe-file (get-directory pool))
    (dolist (pathname (directory (merge-pathnames (make-pathname :name :wild :type (file-extension pool))
                                                  (get-directory pool))))
      (delete-file pathname)))
  (clrhash (root-objects pool)))


(defmethod print-object ((transaction transaction) stream)
  (print-unreadable-object (transaction stream :type t :identity t)
    (format stream "~a ~a"
            (get-function transaction)
            (or (args transaction) "()"))))


(defmethod remove-root-object ((pool pool) name)
  (remhash name (root-objects pool)))


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


(defmethod log-transaction ((pool pool) (transaction transaction))
  "Log transaction for pool"
  (let ((out (transaction-log-stream pool)))
    (funcall (serializer pool) transaction out (serialization-state pool))
    (terpri out)
    (finish-output out)))


(defmethod log-transaction :after ((pool pool) (transaction transaction))
  "Execute the transaction-hook"
  (funcall (transaction-hook pool) transaction))


(defmethod query ((pool pool) function &rest args)
  "Execute an exclusive query function on a sytem"
  (apply function (cons pool args)))


(defmethod execute-on ((transaction transaction) (pool pool))
  "Execute a transaction itself in the context of a pool"
  (apply (get-function transaction)
         (cons pool (args transaction))))


(defmethod snapshot ((pool pool))
  "Write to whole pool to persistent storage resetting the transaction log"
  (let ((timetag (timetag))
        (transaction-log (transaction-log pool))
        (snapshot (get-snapshot pool)))
    (close-open-streams pool)
    (when (probe-file snapshot)
      (copy-file snapshot (merge-pathnames (make-pathname :name (get-snapshot-filename pool timetag)
                                                          :type (file-extension pool))
                                           snapshot)))
    (with-open-file (out snapshot
                         :direction :output :if-does-not-exist :create :if-exists :supersede)
      (funcall (serializer pool) (root-objects pool) out (serialization-state pool)))
    (when (probe-file transaction-log)
      (copy-file transaction-log (merge-pathnames (make-pathname :name (get-transaction-log-filename pool timetag)
                                                                 :type (file-extension pool))
                                                  transaction-log))
      (delete-file transaction-log))))


(defmethod backup ((pool pool) &key directory)
  "Make backup copies of the current snapshot and transaction-log files"
  (let* ((timetag (timetag))
         (transaction-log (transaction-log pool))
         (snapshot (get-snapshot pool))
         (transaction-log-backup (merge-pathnames (make-pathname :name (get-transaction-log-filename pool timetag)
                                                                 :type (file-extension pool))
                                                  (or directory transaction-log)))
         (snapshot-backup (merge-pathnames (make-pathname :name (get-snapshot-filename pool timetag)
                                                          :type (file-extension pool))
                                           (or directory snapshot))))
    (close-open-streams pool)
    (when (probe-file transaction-log)
      (copy-file transaction-log transaction-log-backup))
    (when (probe-file snapshot)
      (copy-file snapshot snapshot-backup))
    timetag))


(defmethod restore ((pool pool))
  "Load a pool from persistent storage starting from the last snapshot and replaying the transaction log"
  (clrhash (root-objects pool))
  (close-open-streams pool)
  (when (probe-file (get-snapshot pool))
    (with-open-file (in (get-snapshot pool) :direction :input)
      (setf (root-objects pool) (funcall (deserializer pool) in (serialization-state pool)))))
  (when (probe-file (transaction-log pool))
    (let ((position 0))
      (handler-bind ((s-xml:xml-parser-error
                       #'(lambda (condition)
                           (format *standard-output*
                                   ";; Warning: error during transaction log restore: ~s~%"
                                   condition)
                           (truncate-file (transaction-log pool) position)
                           (return-from restore))))
        (with-open-file (in (transaction-log pool) :direction :input)
          (loop
            (let ((transaction (funcall (deserializer pool) in (serialization-state pool))))
              (setf position (file-position in))
              (if transaction
                  (execute-on transaction pool)
                  (return)))))))))


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

(defmethod stop ((pool pool) &key abort)
  (close-open-streams pool :abort abort))


;;;
;;; 5. Some utilities
;;;
(defun timetag (&optional (universal-time (get-universal-time)))
  "Return a GMT string of universal-time as YYMMDDTHHMMSS"
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time 0)
    (format nil
            "~d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d"
            year month date hour minute second)))


(defmethod get-transaction-log-filename ((pool pool) &optional suffix)
  "Return the name of the transaction-log filename, optionally using a suffix"
  (format nil "transaction-log~@[-~a~]" suffix))


(defmethod get-snapshot-filename ((pool pool) &optional suffix)
  "Return the name of the snapshot filename, optionally using a suffix"
  (format nil "snapshot~@[-~a~]" suffix))



;;;
;;; 6. Some file manipulation utilities
;;;
(defun truncate-file (file position)
  "Truncate the physical file at position by copying and replacing it"
  (let ((tmp-file (merge-pathnames (concatenate 'string "tmp-" (pathname-name file)) file))
        (buffer (make-string 4096))
        (index 0)
        (read-count 0))
    (with-open-file (in file :direction :input)
      (with-open-file (out tmp-file :direction :output :if-exists :overwrite :if-does-not-exist :create)
        (when (> position (file-length in)) (return-from truncate-file))
        (loop
          (when (= index position) (return))
          (setf read-count (read-sequence buffer in))
          (when (>= (+ index read-count) position)
            (setf read-count (- position index)))
          (incf index read-count)
          (write-sequence buffer out :end read-count))))
    (delete-file file)
    (rename-file tmp-file file))
  (format t ";; Notice: truncated transaction log at position ~d~%" position))


(defun copy-file (source target)
  (let ((buffer (make-string 4096))
        (read-count 0))
    (with-open-file (in source :direction :input)
      (with-open-file (out target :direction :output :if-exists :overwrite :if-does-not-exist :create)
        (loop
          (setf read-count (read-sequence buffer in))
          (write-sequence buffer out :end read-count)
          (when (< read-count 4096) (return)))))))



;;;
;;; 7. from the serialization package
;;;
(defmethod reset-known-slots ((pool pool) &optional class)
  (reset-known-slots (serialization-state pool) class))



;;;
;;; 8. extra documentation
;;;

(setf (documentation 'guard 'function) "Access the guard function of a sytem")

#-allegro
(setf (documentation '(setf guard) 'function) "Set the guard function of a pool")
