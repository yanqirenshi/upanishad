(in-package :upanishad.pool)

(defun set-pathname (pool type directory)
  "pool の snapshot に pathname を追加します。"
  (setf (snapshot-pathnames pool type)
        (make-snapshot-pathname pool directory type)))

(defun init-snapshot-pathname (pool directory)
  "pool の snapshot を初期化します。"
  (dolist (type '(:object :index :memes :indexes))
    (set-pathname pool type directory)))
