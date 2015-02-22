;;;;;
;;;;;
;;;;;
;;;;;

(in-package :upanishad)

(defun print-root-objects (pool)
  (maphash #'(lambda (k v)
               (when (not (eq :ID-COUNTER k))
                 (let ((type (type-of v)))
                   (format t "~20a : ~a~%"
                           k
                           (cond ((eq 'CONS type) (length v))
                                 ((eq 'HASH-TABLE type) (hash-table-size v)))))))
           (get-root-objects pool)))
