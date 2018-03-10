(in-package :upanishad.pool)

(defun init-transaction-log-pathname (pool directory)
  "pool の transaction-log を初期化します。"
  (setf (transaction-log pool)
        (make-transaction-log-pathname pool directory)))
