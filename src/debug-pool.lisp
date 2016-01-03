;;;;;
;;;;; Contents
;;;;;  1. the code for #'s-xml::echo-xml is in "echo.lisp" in S-XML's test code
;;;;;

(in-package :upanishad)

;;;
;;; 1. the code for #'s-xml::echo-xml is in "echo.lisp" in S-XML's test code
;;;
(defun print-transaction-log (pool)
  "Echo the XML making up the transaction log of pool to t"
  (with-open-file (in (transaction-log pool) :direction :input)
    (loop
      (let ((transaction (s-xml::echo-xml in *standard-output*)))
        (when (null transaction) (return)))))
  t)


(defun show-transaction-log (pool)
  "Print the transaction objects making up the transaction log of pool to t"
  (with-open-file (in (transaction-log pool) :direction :input)
    (loop
      (let ((transaction (deserialize-xml in (serialization-state pool))))
        (if (null transaction)
            (return)
            (format t "~a~%" transaction)))))
  t)


(defun print-snapshot (pool)
  "Echo the XML making up the snapshot of pool to t"
  (with-open-file (in (get-snapshot pool) :direction :input)
    (s-xml::echo-xml in *standard-output*))
  t)


(defun transaction-log-tail (pool &optional (count 8))
  "Return a list of the count last transaction objects of pool"
  (let (transactions)
    (with-open-file (in (transaction-log pool) :direction :input)
      (loop
        (let ((transaction (deserialize-xml in (serialization-state pool))))
          (if (null transaction)
              (return)
              (push transaction transactions)))))
    (setf transactions (nreverse transactions))
    (nthcdr (max 0 (- (length transactions) count)) transactions)))
