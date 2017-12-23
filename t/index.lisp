(in-package :upanishad-test)

(defparameter *test-pool-directory* (test-pool-directory "demo"))

(defclass test-meme (upanishad:meme)
  ((name :accessor name
         :initarg :name
         :initform "")))

(plan nil)

;;;
;;; slot-index.lisp
;;;
(diag "slot-index.lisp")

(with-pool (pool *test-pool-directory*)
  (subtest "make-slot-index"
    (skip 1 "wait ...."))

  (subtest "get-index-key"
    (skip 1 "wait ....")))

;;;
;;; slot-index-unique.lisp
;;;
(diag "slot-index-unique.lisp")


;;;
;;; slot-index-multiple.lisp
;;;
(diag "slot-index-multiple.lisp")


(finalize)
