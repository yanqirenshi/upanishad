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
  an argument list. The function should accept the system instance
  prepended to the argument list as arguments and implement the actual
  transaction in a re-entrant way."
  (make-instance 'transaction :function function :args args))



;;;
;;; 2. Generic functions
;;;
(defmethod get-root-object ((pool pool) name)
  (gethash name (get-root-objects pool)))


(defmethod (setf get-root-object) (value (pool pool) name)
  (setf (gethash name (get-root-objects pool)) value))


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
(defmethod initialize-instance :after ((system pool) &rest initargs &key &allow-other-keys)
  "After a system is initialized, derive its file paths and try to restore it"
  (declare (ignore initargs))
  (with-slots (directory) system
    (ensure-directories-exist directory)
    (setf (get-snapshot system) (merge-pathnames (make-pathname :name (get-snapshot-filename system)
                                                                :type (get-file-extension system))
                                                 directory)
          (get-transaction-log system) (merge-pathnames (make-pathname :name (get-transaction-log-filename system)
                                                                       :type (get-file-extension system))
                                                        directory)))
  (restore system))


(defmethod get-transaction-log-stream :before ((system pool))
  (with-slots (transaction-log-stream) system
    (unless transaction-log-stream
      (setf transaction-log-stream (open (get-transaction-log system)
                                         :direction :output
                                         :if-does-not-exist :create
                                         #+ccl :sharing #+ccl nil
                                         :if-exists :append)))))


(defmethod close-open-streams ((system pool) &key abort)
  "Close all open stream associated with system (optionally aborting operations in progress)"
  (with-slots (transaction-log-stream) system
    (when transaction-log-stream
      (close transaction-log-stream :abort abort)
      (setf transaction-log-stream nil))))


(defmethod totally-destroy ((system pool) &key abort)
  "Totally destroy system from permanent storage by deleting any files used by the system, remove all root objects"
  (close-open-streams system :abort abort)
  (when (probe-file (get-directory system))
    (dolist (pathname (directory (merge-pathnames (make-pathname :name :wild :type (get-file-extension system))
                                                  (get-directory system))))
      (delete-file pathname)))
  (clrhash (get-root-objects system)))


(defmethod print-object ((transaction transaction) stream)
  (print-unreadable-object (transaction stream :type t :identity t)
    (format stream "~a ~a"
            (get-function transaction)
            (or (get-args transaction) "()"))))


(defmethod remove-root-object ((system pool) name)
  (remhash name (get-root-objects system)))


(defmethod execute ((system pool) (transaction transaction))
  "Execute a transaction on a system and log it to the transaction log"
  (let ((result
         (handler-bind ((error #'(lambda (condition)
                                   (when (and (get-option system :rollback-on-error)
                                              (initiates-rollback condition))
                                     (format *standard-output*
                                             ";; Notice: system rollback/restore due to error (~a)~%"
                                             condition)
                                     (restore system)))))
           (execute-on transaction system))))
    (log-transaction system transaction)
    result))


(defmethod log-transaction ((system pool) (transaction transaction))
  "Log transaction for system"
  (let ((out (get-transaction-log-stream system)))
    (funcall (get-serializer system) transaction out (get-serialization-state system))
    (terpri out)
    (finish-output out)))


(defmethod log-transaction :after ((system pool) (transaction transaction))
  "Execute the transaction-hook"
  (funcall (get-transaction-hook system) transaction))


(defmethod query ((system pool) function &rest args)
  "Execute an exclusive query function on a sytem"
  (apply function (cons system args)))


(defmethod execute-on ((transaction transaction) (system pool))
  "Execute a transaction itself in the context of a system"
  (apply (get-function transaction)
         (cons system (get-args transaction))))


(defmethod snapshot ((system pool))
  "Write to whole system to persistent storage resetting the transaction log"
  (let ((timetag (timetag))
        (transaction-log (get-transaction-log system))
        (snapshot (get-snapshot system)))
    (close-open-streams system)
    (when (probe-file snapshot)
      (copy-file snapshot (merge-pathnames (make-pathname :name (get-snapshot-filename system timetag)
                                                          :type (get-file-extension system))
                                           snapshot)))
    (with-open-file (out snapshot
                         :direction :output :if-does-not-exist :create :if-exists :supersede)
      (funcall (get-serializer system) (get-root-objects system) out (get-serialization-state system)))
    (when (probe-file transaction-log)
      (copy-file transaction-log (merge-pathnames (make-pathname :name (get-transaction-log-filename system timetag)
                                                                 :type (get-file-extension system))
                                                  transaction-log))
      (delete-file transaction-log))))


(defmethod backup ((system pool) &key directory)
  "Make backup copies of the current snapshot and transaction-log files"
  (let* ((timetag (timetag))
         (transaction-log (get-transaction-log system))
         (snapshot (get-snapshot system))
         (transaction-log-backup (merge-pathnames (make-pathname :name (get-transaction-log-filename system timetag)
                                                                 :type (get-file-extension system))
                                                  (or directory transaction-log)))
         (snapshot-backup (merge-pathnames (make-pathname :name (get-snapshot-filename system timetag)
                                                          :type (get-file-extension system))
                                           (or directory snapshot))))
    (close-open-streams system)
    (when (probe-file transaction-log)
      (copy-file transaction-log transaction-log-backup))
    (when (probe-file snapshot)
      (copy-file snapshot snapshot-backup))
    timetag))


(defmethod restore ((system pool))
  "Load a system from persistent storage starting from the last snapshot and replaying the transaction log"
  (clrhash (get-root-objects system))
  (close-open-streams system)
  (when (probe-file (get-snapshot system))
    (with-open-file (in (get-snapshot system) :direction :input)
      (setf (get-root-objects system) (funcall (get-deserializer system) in (get-serialization-state system)))))
  (when (probe-file (get-transaction-log system))
    (let ((position 0))
      (handler-bind ((s-xml:xml-parser-error
                      #'(lambda (condition)
                          (format *standard-output*
                                  ";; Warning: error during transaction log restore: ~s~%"
                                  condition)
                          (truncate-file (get-transaction-log system) position)
                          (return-from restore))))
        (with-open-file (in (get-transaction-log system) :direction :input)
          (loop
             (let ((transaction (funcall (get-deserializer system) in (get-serialization-state system))))
               (setf position (file-position in))
               (if transaction
                   (execute-on transaction system)
                   (return)))))))))


(defmethod execute ((system guarded-pool) (transaction transaction))
  "Execute a transaction on a system controlled by a guard"
  (funcall (get-guard system)
           #'(lambda () (call-next-method system transaction))))


(defmethod query ((system guarded-pool) function &rest args)
  "Execute an exclusive query function on a sytem controlled by a guard"
  (funcall (get-guard system)
           #'(lambda () (apply function (cons system args)))))


(defmethod snapshot ((system guarded-pool))
  "Make a snapshot of a system controlled by a guard"
  (funcall (get-guard system)
           #'(lambda () (call-next-method system))))


(defmethod backup ((system guarded-pool) &key directory)
  "Do a backup on a system controlled by a guard"
  (funcall (get-guard system)
           #'(lambda () (call-next-method system directory))))


(defmethod restore ((system guarded-pool))
  "Restore a system controlled by a guard"
  (funcall (get-guard system)
           #'(lambda () (call-next-method system))))



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


(defmethod get-transaction-log-filename ((system pool) &optional suffix)
  "Return the name of the transaction-log filename, optionally using a suffix"
  (format nil "transaction-log~@[-~a~]" suffix))


(defmethod get-snapshot-filename ((system pool) &optional suffix)
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
(defmethod reset-known-slots ((system pool) &optional class)
  (reset-known-slots (get-serialization-state system) class))



;;;
;;; 8. extra documentation
;;;

(setf (documentation 'get-guard 'function) "Access the guard function of a sytem")

#-allegro
(setf (documentation '(setf get-guard) 'function) "Set the guard function of a system")




#|
-*- mode: Lisp -*-

$Id$

Object Prevalence in Common Lisp

Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.

You are granted the rights to distribute and use this software
as governed by the terms of the Lisp Lesser General Public License
(http://opensource.franz.com/preamble.html), also known as the LLGPL.
|#
