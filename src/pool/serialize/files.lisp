(in-package :upanishad.pool)

;;;
;;; utility
;;;
(defun timetag (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time 0)
    (format nil
            "~d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d"
            year month date hour minute second)))

;;;
;;; file operator
;;;
(defun tmp-pathname (file)
  (merge-pathnames (concatenate 'string "tmp-" (pathname-name file)) file))

(defun truncate-file (file position)
  (let ((tmp-file (tmp-pathname file))
        (buffer (make-string 4096))
        (index 0)
        (read-count 0))
    (with-open-file (in file :direction :input)
      (with-open-file (out tmp-file :direction :output
                                    :if-exists :overwrite
                                    :if-does-not-exist :create)
        (when (> position (file-length in))
          (return-from truncate-file))
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
      (with-open-file (out target :direction :output
                                  :if-exists :overwrite
                                  :if-does-not-exist :create)
        (loop
          (setf read-count (read-sequence buffer in))
          (write-sequence buffer out :end read-count)
          (when (< read-count 4096) (return)))))))


;;;
;;; snapshot file
;;;
(defvar *snapshot-types*
  '(:object :index   ;; TODO: この二つは廃棄予定
    :memes :indexes) ;;       新たにこの二つになります。
  "スナップショットを取得する種類")

(defun snapshot-type-p (type)
  "type が snapshot-type かを返す。"
  (find type *snapshot-types*))

(defmethod snapshot-pathnames (pool type)
  (assert (snapshot-type-p type))
  (let ((pathnames (slot-value pool 'snapshot)))
    (gethash type pathnames)))

(defmethod (setf snapshot-pathnames) (value pool type)
  (assert (snapshot-type-p type))
  (let ((pathnames (slot-value pool 'snapshot)))
    (setf (gethash type pathnames) value)))

(defmethod make-snapshot-filename (type &optional suffix)
  "snapshot のファイル名を返す。"
  (assert (snapshot-type-p type))
  (unless type (warn "type が nil ですよ。"))
  (if type
      (format nil "snapshot-~a~@[-~a~]" (string-downcase (symbol-name type)) suffix)
      (format nil "snapshot~@[-~a~]" suffix)))

(defmethod make-snapshot-pathname (pool directory type &optional suffix)
  "snapshot のファイル名をフルパスで返す。"
  (let ((filename (make-snapshot-filename type suffix)))
    (merge-pathnames (make-pathname :name filename
                                    :type (file-extension pool))
                     directory)))

(defun make-snapshot-backup-pathname (pool directory type timetag)
  "snapshot のバックアップ・ファイル名を返します。"
  (make-snapshot-pathname pool directory type timetag))

(defun snapshot-copy-snapshot-file (pool directory type timetag)
  ""
  (when (probe-file directory)
    (copy-file directory
               (make-snapshot-backup-pathname pool directory type timetag))))

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
