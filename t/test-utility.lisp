(defpackage :upanishad-test.test-utility
  (:use :cl :upanishad :prove :s-serialization)
  (:export #:clear-pool-datastor))
(in-package :upanishad-test.test-utility)

(defun clear-pool-datastor (directory)
  (when (probe-file directory)
    (dolist (pathname (directory (merge-pathnames "*.xml" directory)))
      (delete-file pathname))))
