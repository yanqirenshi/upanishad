(defpackage :upanishad-test.test-utility
  (:use :cl :upanishad :prove :s-serialization)
  (:export #:clear-pool-datastor
           #:test-pool-directory
           #:with-pool))
(in-package :upanishad-test.test-utility)

(defun clear-pool-datastor (directory)
  (when (probe-file directory)
    (dolist (pathname (directory (merge-pathnames "*.xml" directory)))
      (delete-file pathname))))

;;;
;;; directory
;;;
(defvar *test-pool-root-directory* (pathname "~/var/upanishad/test/"))

(defun ensure-directory-path (directory-path)
  (let ((len (length directory-path)))
    (if (string= (subseq directory-path (- len 1)) "/")
        directory-path
        (concatenate 'string directory-path "/"))))

(defun test-pool-directory (path &key (base-directory *test-pool-root-directory*))
  (ensure-directories-exist
   (merge-pathnames (ensure-directory-path path) base-directory)))

;;;
;;; with
;;;
(defmacro with-pool ((pool directory &key (with-id-counter t)) &body body)
  `(let ((,pool nil))
     (unwind-protect
          (clear-pool-datastor ,directory)
       (setf ,pool (make-pool ,directory))
       (when ,with-id-counter (tx-create-%id-counter ,pool))
       ,@body
       (when pool
         (stop pool)))))
