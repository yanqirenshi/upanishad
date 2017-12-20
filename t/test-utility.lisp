(defpackage :upanishad-test.test-utility
  (:use #:cl
        #:prove
        #:s-serialization)
  (:import-from :upanishad.meme
                #:%id)
  (:import-from :upanishad.pool
                #:stop
                #:make-pool
                #:execute-transaction
                #:tx-create-%id-counter)
  (:import-from :alexandria
                #:hash-table-alist
                #:hash-table-values)
  (:export #:clear-pool-datastor
           #:test-pool-directory
           #:with-pool
           #:is-%id->value
           #:is-value->objects)
  (:export #:meme1
           #:meme2
           #:slot1
           #:slot2))
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
          (progn
            (clear-pool-datastor ,directory)
            (setf ,pool (make-pool ,directory))
            (when ,with-id-counter
               (execute-transaction
                 (tx-create-%id-counter ,pool)))
            ,@body)
       (when ,pool
         (stop ,pool)))))

;;;
;;; is-xxx
;;;
(defun is-%id->value (got-ht expect &optional comment)
  (is (sort (hash-table-alist got-ht)
            #'(lambda (a b)
                (< (car a) (car b))))
      expect
      :test 'equalp comment))

(defun is-value->objects (got-ht expect &optional comment)
  (labels ((2alist (%id->object)
             (let ((out nil))
               (maphash #'(lambda (k v)
                            (push (list k
                                        (sort (hash-table-values v)
                                              #'(lambda (a b)
                                                  (< (%id a) (%id b)))))
                                  out))
                        %id->object)
               (sort out #'(lambda (a b) (< (car a) (car b)))))))
    (is (2alist got-ht)
        expect
        comment)))

;;;
;;; class
;;;
(defclass meme1 (meme)
  ((slot1 :documentation ""
          :accessor slot1 :initarg :slot1 :initform nil)
   (slot2 :documentation ""
          :accessor slot2 :initarg :slot2 :initform nil)))

(defclass meme2 (meme)
  ((slot1 :documentation ""
          :accessor slot1 :initarg :slot1 :initform nil)
   (slot2 :documentation ""
          :accessor slot2 :initarg :slot2 :initform nil)))
