(defpackage :upanishad-test.test-managed-pool
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.test-managed-pool)

(defparameter *test-pool-directory* (test-pool-directory "test-managed-pool"))

(defvar *test-pool* nil)

;;;
;;; A Test CLOS class
;;;
(defclass managed-person (meme)
  ((firstname :initarg :firstname :initform "" :accessor get-firstname)
   (lastname  :initarg :lastname  :initform "" :accessor get-lastname)))

(defmethod (setf get-firstname) (value (managed-person managed-person))
  (execute-transaction
   (tx-change-object-slots *test-pool* 'managed-person (%id managed-person) (list (list 'firstname value)))))

(defmethod (setf get-lastname) (value (managed-person managed-person))
  (execute-transaction
   (tx-change-object-slots *test-pool* 'managed-person (%id managed-person) (list (list 'lastname value)))))

(defun pairify (list)
  (when list (concatenate 'list
                          (list (subseq list 0 2))
                          (pairify (rest (rest list))))))

(defun make-managed-person (&rest slots)
  (let ((slots-and-values (pairify slots)))
    (execute-transaction
     (tx-create-object *test-pool* 'managed-person slots-and-values))))

(defun get-managed-person (slot value)
  (first (find-objects *test-pool* 'managed-person :slot slot :value value)))

(defun delete-managed-person (managed-person)
  (execute-transaction
   (tx-delete-object *test-pool* 'managed-person (%id managed-person))))

(defvar *jlp*)

(defvar *kj*)

(defvar *managed-guard*)

(defun managed-guard (thunk)
  (setf *managed-guard* t)
  (funcall thunk))

;;;
;;; Test!
;;;
(plan 13)

(subtest "test-managed-pool-start"
  "Create a new prevalence pool for testing purposes"
  (let ((directory *test-pool-directory*))
    ;; Throw away any xml files that we find: we want to start from scratch
    (when (probe-file directory)
      (dolist (pathname (directory (merge-pathnames "*.xml" directory)))
        (delete-file pathname)))
    (setf *test-pool* (make-pool directory))
    (ok *test-pool*)
    (index-on *test-pool* 'managed-person '(firstname lastname) 'equal)))

(subtest "test-create-counter"
  "Create a new %id counter"
  (execute-transaction (tx-create-%id-counter *test-pool*))
  (ok (zerop (get-root-object *test-pool* :%id-counter))))

;;;
;;; 4. A place to store our test managed-person's id outside of the pool
;;;

(subtest "test-create-managed-person"
  "Create a new test managed-person"
  (let ((managed-person (make-managed-person 'firstname "Jean-Luc" 'lastname "Picard")))
    (ok (eq (class-of managed-person) (find-class 'managed-person)))
    (ok (equal (get-firstname managed-person) "Jean-Luc"))
    (ok (equal (get-lastname managed-person) "Picard"))
    (ok (equal (get-lastname (get-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (get-managed-person 'lastname "Picard")) "J-Lu")
    (ok (equal (get-lastname (get-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (get-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (ok (equal (get-firstname (get-managed-person 'lastname "Picard")) "Jean-Luc"))
    (ok (eq NIL (get-managed-person 'firstname "J-Lu")))
    (setf *jlp* (%id managed-person))))

(subtest "test-get-managed-person"
  (let ((managed-person (get-object-at-%id *test-pool* 'managed-person *jlp*)))
    (ok (eq (class-of managed-person) (find-class 'managed-person)))
    (ok (equal (get-firstname managed-person) "Jean-Luc"))
    (ok (equal (get-lastname managed-person) "Picard"))
    (ok (equal (get-lastname (get-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (get-managed-person 'lastname "Picard")) "J-Lu")
    (ok (equal (get-lastname (get-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (get-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (ok (equal (get-firstname (get-managed-person 'lastname "Picard")) "Jean-Luc"))
    (ok (eq NIL (get-managed-person 'firstname "J-Lu")))))

(subtest "test-get-managed-person-restart"
  "Throw away the previous prevalence instance and start over,
  counting on a restore operation using the transaction log"
  (stop *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*))
  (let ((managed-person (get-object-at-%id *test-pool* 'managed-person *jlp*)))
    (ok (eq (class-of managed-person) (find-class 'managed-person)))
    (ok (equal (get-firstname managed-person) "Jean-Luc"))
    (ok (equal (get-lastname managed-person) "Picard"))
    (ok (equal (get-lastname (get-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (get-managed-person 'lastname "Picard")) "J-Lu")
    (ok (equal (get-lastname (get-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (get-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (ok (equal (get-firstname (get-managed-person 'lastname "Picard")) "Jean-Luc"))
    (ok (eq NIL (get-managed-person 'firstname "J-Lu")))))

(subtest "test-get-managed-person-snapshot"
  "Create a snapshot of our test pool"
  (snapshot *test-pool*)
  (let ((managed-person (get-object-at-%id *test-pool* 'managed-person *jlp*)))
    (ok (eq (class-of managed-person) (find-class 'managed-person)))
    (ok (equal (get-firstname managed-person) "Jean-Luc"))
    (ok (equal (get-lastname managed-person) "Picard"))
    (ok (equal (get-lastname (get-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (get-managed-person 'lastname "Picard")) "J-Lu")
    (ok (equal (get-lastname (get-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (get-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (ok (equal (get-firstname (get-managed-person 'lastname "Picard")) "Jean-Luc"))
    (ok (eq NIL (get-managed-person 'firstname "J-Lu")))))

(subtest "test-get-managed-person-restart-snapshot"
  "Throw away the previous prevalence instance and start over,
  counting on a restore operation using the snapshot"
  (stop *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*))
  (let ((managed-person (get-object-at-%id *test-pool* 'managed-person *jlp*)))
    (ok (eq (class-of managed-person) (find-class 'managed-person)))
    (ok (equal (get-firstname managed-person) "Jean-Luc"))
    (ok (equal (get-lastname managed-person) "Picard"))
    (ok (equal (get-lastname (get-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (get-managed-person 'lastname "Picard")) "J-Lu")
    (ok (equal (get-lastname (get-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (get-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (ok (equal (get-firstname (get-managed-person 'lastname "Picard")) "Jean-Luc"))
    (ok (eq NIL (get-managed-person 'firstname "J-Lu")))))

(subtest "test-create-managed-person-1"
  "Create another test managed-person"
  (let ((managed-person (make-managed-person 'firstname "Kathryn" 'lastname "Janeway")))
    (ok (eq (class-of managed-person) (find-class 'managed-person)))
    (ok (equal (get-firstname managed-person) "Kathryn"))
    (ok (equal (get-lastname managed-person) "Janeway"))
    (ok (equal (get-firstname (get-managed-person 'lastname "Janeway")) "Kathryn"))
    (ok (equal (get-lastname (get-managed-person 'firstname "Kathryn")) "Janeway"))
    (setf *kj* (%id managed-person))))

(subtest "test-get-managed-person-1"
  (let ((managed-person (get-object-at-%id *test-pool* 'managed-person *kj*)))
    (ok (eq (class-of managed-person) (find-class 'managed-person)))
    (ok (equal (get-firstname managed-person) "Kathryn"))
    (ok (equal (get-lastname managed-person) "Janeway"))
    (ok (equal (get-firstname (get-managed-person 'lastname "Janeway")) "Kathryn"))
    (ok (equal (get-lastname (get-managed-person 'firstname "Kathryn")) "Janeway"))))

(subtest "test-get-managed-person-restart-1"
  "Throw away the previous prevalence instance and start over,
   counting on a restore operation using both the snapshot and the transaction log"
  (stop *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*))
  (let ((managed-person (get-object-at-%id *test-pool* 'managed-person *jlp*)))
    (ok (eq (class-of managed-person) (find-class 'managed-person)))
    (ok (equal (get-firstname managed-person) "Jean-Luc"))
    (ok (equal (get-lastname managed-person) "Picard"))
    (ok (equal (get-lastname (get-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (get-managed-person 'lastname "Picard")) "J-Lu")
    (ok (equal (get-lastname (get-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (get-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (ok (equal (get-firstname (get-managed-person 'lastname "Picard")) "Jean-Luc"))
    (ok (eq NIL (get-managed-person 'firstname "J-Lu")))))

(subtest "test-get-managed-person-restart-2"
  (let ((managed-person (get-object-at-%id *test-pool* 'managed-person *kj*)))
    (ok (eq (class-of managed-person) (find-class 'managed-person)))
    (ok (equal (get-firstname managed-person) "Kathryn"))
    (ok (equal (get-lastname managed-person) "Janeway"))
    (ok (equal (get-firstname (get-managed-person 'lastname "Janeway")) "Kathryn"))
    (ok (equal (get-lastname (get-managed-person 'firstname "Kathryn")) "Janeway"))))

(subtest "test-managed-person-count"
  (mapcar #'(lambda (pair)
              (make-managed-person 'firstname (first pair) 'lastname (second pair)))
          '(("Benjamin" "Sisko") ("James T." "Kirk") ("Jonathan" "Archer")))
  (ok (= (length (find-objects *test-pool* 'managed-person)) 5))
  (mapcar #'(lambda (pair)
              (delete-managed-person (get-managed-person 'firstname (first pair))))
          '(("Benjamin" "Sisko") ("James T." "Kirk") ("Jonathan" "Archer")))
  (ok (= (length (find-objects *test-pool* 'managed-person)) 2)))

(subtest "managed-guard"
  (subtest "test-managed-guarded"
    "testing a managed-guarded prevalence system
   [Not sure that we need the below test here -- RRR]"
    (stop *test-pool*)
    (setf *test-pool* (make-pool *test-pool-directory*
                                 :pool-class 'guarded-pool))
    (setf (guard *test-pool*) #'managed-guard)
    (let (new-managed-person)
      (setf *managed-guard* nil)
      (setf new-managed-person (make-managed-person 'firstname "John" 'lastname "Doe"))
      (ok *managed-guard*)
      (setf *managed-guard* nil)
      (delete-managed-person new-managed-person)
      (ok *managed-guard*))))

(finalize)
