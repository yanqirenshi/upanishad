(in-package :upanishad)

(defun timetag (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time 0)
    (format nil
            "~d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d"
            year month date hour minute second)))

;;;
;;; file operator
;;;
(defun truncate-file (file position)
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
;;; snapshot file
;;;
(defun snapshot-type-p (type)
  (find type '(:object :index)))

(defmethod snapshot-pathnames (pool type)
  (assert (snapshot-type-p type))
  (let ((pathnames (slot-value pool 'snapshot)))
    (gethash type pathnames)))

(defmethod (setf snapshot-pathnames) (value pool type)
  (assert (snapshot-type-p type))
  (let ((pathnames (slot-value pool 'snapshot)))
    (setf (gethash type pathnames) value)))

(defmethod make-snapshot-filename ((pool pool) (type symbol) &optional suffix)
  (assert (or (null type) (find type '(:object :index))))
  (unless type (warn "type が nil ですよ。"))
  (if type
      (format nil "snapshot-~a~@[-~a~]" (string-downcase (symbol-name type)) suffix)
      (format nil "snapshot~@[-~a~]" suffix)))

(defmethod make-snapshot-pathname (pool directory type &optional suffix)
  (merge-pathnames (make-pathname :name (make-snapshot-filename pool type suffix)
                                  :type (file-extension pool))
                   directory))

(defun make-snapshot-backup-pathname (pool directory type timetag)
  (make-snapshot-pathname pool directory type timetag))

(defun snapshot-copy-snapshot-file (pool directory type timetag)
  (when (probe-file directory)
    (copy-file directory (make-snapshot-backup-pathname pool directory type timetag))))

(defun backup-snapshot (snapshot snapshot-backup)
  (when (probe-file snapshot)
    (copy-file snapshot snapshot-backup)))

;;;
;;; transaction log file
;;;
(defmethod make-transaction-log-filename ((pool pool) &optional suffix)
  (format nil "transaction-log~@[-~a~]" suffix))

(defmethod make-transaction-log-pathname (pool directory &optional suffix)
  (merge-pathnames (make-pathname :name (make-transaction-log-filename pool suffix)
                                  :type (file-extension pool))
                   directory))

(defun transaction-log-backup-file (pool directory timetag)
  (make-transaction-log-pathname pool directory timetag))

(defun snapshot-transaction-log (pool directory timetag)
  (when (probe-file directory)
    (copy-file directory (transaction-log-backup-file pool directory timetag))))

(defun backup-transaction-log (transaction-log transaction-log-backup)
  (when (probe-file transaction-log)
    (copy-file transaction-log transaction-log-backup)))

;;;
;;; snapshot
;;;
(defun snapshot-root-objects (pool snapshot)
  (with-open-file (out snapshot :direction :output
                                :if-does-not-exist :create
                                :if-exists :supersede)
    (funcall (serializer pool) (root-objects pool) out (serialization-state pool))))

(defmethod snapshot ((pool pool))
  (let ((timetag (timetag))
        (transaction-log (transaction-log pool))
        (snapshot-object (snapshot-pathnames pool :object))
        (snapshot-index (snapshot-pathnames pool :index)))
    (close-open-streams pool)
    (snapshot-copy-snapshot-file pool snapshot-object :object timetag)
    (snapshot-copy-snapshot-file pool snapshot-index :object timetag)
    (snapshot-root-objects pool snapshot-object)
    (snapshot-transaction-log pool transaction-log timetag)
    (delete-file transaction-log)))


;;;
;;; backup
;;;
(defmethod backup ((pool pool) &key directory)
  (let* ((timetag (timetag))
         (transaction-log (transaction-log pool))
         (transaction-log-backup (transaction-log-backup-file pool (or directory transaction-log) timetag))
         (snapshot-object (snapshot-pathnames pool :object))
         (snapshot-object-backup (make-snapshot-backup-pathname pool (or directory snapshot-object) :object timetag))
         (snapshot-index (snapshot-pathnames pool :index))
         (snapshot-index-backup (make-snapshot-backup-pathname pool (or directory snapshot-index) :object timetag)))
    (close-open-streams pool)
    (backup-transaction-log transaction-log transaction-log-backup)
    (backup-snapshot snapshot-object snapshot-object-backup)
    (backup-snapshot snapshot-object snapshot-index-backup)
    timetag))


;;;
;;; restore
;;;
(defun restore-root-objects (pool)
  (let ((deserializer (deserializer pool))
        (serialization-state (serialization-state pool)))
    (when (probe-file (snapshot-pathnames pool :object))
      (with-open-file (in (snapshot-pathnames pool :object) :direction :input)
        (setf (root-objects pool)
              (funcall deserializer in serialization-state))))))

(defun restore-transaction-log (pool)
  (when (probe-file (transaction-log pool))
    (let ((position 0))
      (handler-bind ((s-xml:xml-parser-error
                       #'(lambda (condition)
                           (format *standard-output*
                                   ";; Warning: error during transaction log restore: ~s~%"
                                   condition)
                           (truncate-file (transaction-log pool) position)
                           (return-from restore-transaction-log))))
        (with-open-file (in (transaction-log pool) :direction :input)
          (loop
            (let ((transaction (funcall (deserializer pool) in (serialization-state pool))))
              (setf position (file-position in))
              (if transaction
                  (execute-on transaction pool)
                  (return)))))))))

(defmethod restore ((pool pool))
  (clrhash (root-objects pool))
  (close-open-streams pool)
  (restore-root-objects pool)
  (restore-transaction-log pool))
