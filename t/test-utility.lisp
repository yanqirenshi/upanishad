(defpackage :upanishad-test.test-utility
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization)
  (:export #:clear-pool-datastor
           #:test-pool-directory
           #:with-pool))
(in-package :upanishad-test.test-utility)

(defun clear-pool-datastor (directory)
  (when (probe-file directory)
    (dolist (pathname (directory (merge-pathnames "*.xml" directory)))
      (delete-file pathname))))


(defvar *test-pool-root-directory* (pathname "~/tmp/.upanishad/test/"))


(defun ensure-directory-path (target)
  (if (string= "/" (subseq target (- (length target) 1)))
      target
      (concatenate 'string target "/")))

(defun test-pool-directory (target)
  (assert target)
  (merge-pathnames (ensure-directory-path target)
                   *test-pool-root-directory*))

(defmacro with-pool ((pool directory) &body body)
  `(let ((,pool nil))
     (unwind-protect
          (clear-pool-datastor ,directory)
       (setf ,pool (make-pool ,directory))
       ,@body
       (when pool
         (stop pool)))))

