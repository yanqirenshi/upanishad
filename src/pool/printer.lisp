(in-package :upanishad.pool)

(defmethod print-object ((transaction transaction) stream)
  (print-unreadable-object (transaction stream :type t :identity t)
    (format stream "~a ~a"
            (get-function transaction)
            (or (args transaction) "()"))))
