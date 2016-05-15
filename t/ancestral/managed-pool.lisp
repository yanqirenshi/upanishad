(defpackage :upanishad-test.ancestral.managed-pool
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.ancestral.managed-pool)

(defparameter *test-pool-directory* (test-pool-directory "ancestral.managed-pool"))

(defvar *test-pool* nil)


(defclass managed-person (meme)
  ((firstname :initarg :firstname :initform "" :accessor get-firstname)
   (lastname  :initarg :lastname  :initform "" :accessor get-lastname)))

(defun is-firstname (got-parson expected-firstname &optional (description "is firstname"))
  (is (and got-parson (get-firstname got-parson)) expected-firstname
      :test 'string=
      description))

(defun is-lastname (got-parson expected-lastname &optional (description "is lastname"))
  (is (and got-parson (get-lastname got-parson)) expected-lastname
      :test 'string=
      description))

(defun is-not-exist-person (got-parson &optional (description "is not exist person"))
  (ok (eq NIL got-parson) description))

(defun is-person (got &optional (description "is person"))
  (is (class-of got) (find-class 'managed-person)
      description))

(defun update-firstname (pool person value)
  (execute-transaction
   (tx-change-object-slots pool 'managed-person
                           (%id person)
                           `((firstname ,value))))
  person)

(defun pairify (list)
  (when list (concatenate 'list
                          (list (subseq list 0 2))
                          (pairify (rest (rest list))))))

(defun make-person (pool &rest slots)
  (let ((slots-and-values (pairify slots)))
    (execute-transaction
     (tx-create-object pool 'managed-person slots-and-values))))

(defun get-person (pool slot value)
  (first (find-objects pool 'managed-person :slot slot :value value)))

(defun delete-person (pool managed-person)
  (execute-transaction
   (tx-delete-object pool 'managed-person (%id managed-person))))

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
    (add-index *test-pool* 'managed-person '(firstname lastname) 'equal)))

(subtest "test-create-counter"
  "Create a new %id counter"
  (execute-transaction (tx-create-%id-counter *test-pool*))
  (ok (zerop (get-root-object *test-pool* :%id-counter))))

;;;
;;; 4. A place to store our test managed-person's id outside of the pool
;;;

(subtest "test-create-managed-person"
  "Create a new test managed-person"
  (let ((person (make-person *test-pool* 'firstname "Jean-Luc" 'lastname "Picard")))
    (is-person person)
    (is-firstname person "Jean-Luc")
    (is-lastname person "Picard")
    (is-lastname (get-person *test-pool* 'firstname "Jean-Luc") "Picard")

    (update-firstname *test-pool* (get-person *test-pool* 'lastname "Picard") "J-Lu")
    (is-lastname (get-person *test-pool* 'firstname "J-Lu") "Picard")

    (update-firstname *test-pool* (get-person *test-pool* 'firstname "J-Lu") "Jean-Luc")
    (is-firstname (get-person *test-pool* 'lastname "Picard") "Jean-Luc")

    (is-not-exist-person (get-person *test-pool* 'firstname "J-Lu"))

    (setf *jlp* (%id person))))

(subtest "test-get-managed-person"
  (let ((person (get-object-at-%id *test-pool* 'managed-person *jlp*)))
    (is-person person)
    (is-firstname person "Jean-Luc")
    (is-lastname person "Picard")
    (is-lastname (get-person *test-pool* 'firstname "Jean-Luc") "Picard")

    (update-firstname *test-pool* (get-person *test-pool* 'lastname "Picard") "J-Lu")
    (is-lastname (get-person *test-pool* 'firstname "J-Lu") "Picard")

    (update-firstname *test-pool* (get-person *test-pool* 'firstname "J-Lu") "Jean-Luc")
    (is-firstname (get-person *test-pool* 'lastname "Picard") "Jean-Luc")

    (is-not-exist-person (get-person *test-pool* 'firstname "J-Lu"))))

(subtest "test-get-managed-person-restart"
  "Throw away the previous prevalence instance and start over,
  counting on a restore operation using the transaction log"
  (stop *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*))
  (let ((person (get-object-at-%id *test-pool* 'managed-person *jlp*)))
    (is-person person)
    (is-firstname person "Jean-Luc")
    (is-lastname person "Picard")
    (is-lastname (get-person *test-pool* 'firstname "Jean-Luc") "Picard")

    (update-firstname *test-pool* (get-person *test-pool* 'lastname "Picard") "J-Lu")
    (is-lastname (get-person *test-pool* 'firstname "J-Lu") "Picard")

    (update-firstname *test-pool* (get-person *test-pool* 'firstname "J-Lu") "Jean-Luc")
    (is-firstname (get-person *test-pool* 'lastname "Picard") "Jean-Luc")

    (is-not-exist-person (get-person *test-pool* 'firstname "J-Lu"))))

(subtest "test-get-managed-person-snapshot"
  "Create a snapshot of our test pool"
  (snapshot *test-pool*)
  (let ((person (get-object-at-%id *test-pool* 'managed-person *jlp*)))
    (is-person person)
    (is-firstname person "Jean-Luc")
    (is-lastname person "Picard")
    (is-lastname (get-person *test-pool* 'firstname "Jean-Luc") "Picard")

    (update-firstname *test-pool* (get-person *test-pool* 'lastname "Picard") "J-Lu")
    (is-lastname (get-person *test-pool* 'firstname "J-Lu") "Picard")

    (update-firstname *test-pool* (get-person *test-pool* 'firstname "J-Lu") "Jean-Luc")
    (is-firstname (get-person *test-pool* 'lastname "Picard") "Jean-Luc")

    (is-not-exist-person (get-person *test-pool* 'firstname "J-Lu"))))

(subtest "test-get-managed-person-restart-snapshot"
  "Throw away the previous prevalence instance and start over,
  counting on a restore operation using the snapshot"
  (stop *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*))
  (let ((person (get-object-at-%id *test-pool* 'managed-person *jlp*)))
    (is-person person)
    (ok (equal (get-firstname person) "Jean-Luc"))
    (ok (equal (get-lastname person) "Picard"))
    (ok (equal (get-lastname (get-person *test-pool* 'firstname "Jean-Luc")) "Picard"))
    (update-firstname *test-pool* (get-person *test-pool* 'lastname "Picard") "J-Lu")
    (ok (equal (get-lastname (get-person *test-pool* 'firstname "J-Lu")) "Picard"))
    (update-firstname *test-pool* (get-person *test-pool* 'firstname "J-Lu") "Jean-Luc")
    (ok (equal (get-firstname (get-person *test-pool* 'lastname "Picard")) "Jean-Luc"))
    (ok (eq NIL (get-person *test-pool* 'firstname "J-Lu")))))

(subtest "test-create-managed-person-1"
  "Create another test managed-person"
  (let ((person (make-person *test-pool* 'firstname "Kathryn" 'lastname "Janeway")))
    (is-person person)
    (is-firstname person "Kathryn")
    (is-lastname person "Janeway")
    (is-firstname (get-person *test-pool* 'lastname "Janeway") "Kathryn")
    (is-lastname (get-person *test-pool* 'firstname "Kathryn") "Janeway")
    (setf *kj* (%id person))))

(subtest "test-get-managed-person-1"
  (let ((person (get-object-at-%id *test-pool* 'managed-person *kj*)))
    (is-person person)
    (is-firstname person "Kathryn")
    (is-lastname person "Janeway")
    (is-firstname (get-person *test-pool* 'lastname "Janeway") "Kathryn")
    (is-lastname (get-person *test-pool* 'firstname "Kathryn") "Janeway")))

(subtest "test-get-managed-person-restart-1"
  "Throw away the previous prevalence instance and start over,
   counting on a restore operation using both the snapshot and the transaction log"
  (stop *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*))
  (let ((managed-person (get-object-at-%id *test-pool* 'managed-person *jlp*)))
    (is-person managed-person)
    (ok (equal (get-firstname managed-person) "Jean-Luc"))
    (ok (equal (get-lastname managed-person) "Picard"))
    (ok (equal (get-lastname (get-person *test-pool* 'firstname "Jean-Luc")) "Picard"))
    (update-firstname *test-pool* (get-person *test-pool* 'lastname "Picard") "J-Lu")
    (ok (equal (get-lastname (get-person *test-pool* 'firstname "J-Lu")) "Picard"))
    (update-firstname *test-pool* (get-person *test-pool* 'firstname "J-Lu") "Jean-Luc")
    (ok (equal (get-firstname (get-person *test-pool* 'lastname "Picard")) "Jean-Luc"))
    (ok (eq NIL (get-person *test-pool* 'firstname "J-Lu")))))

(subtest "test-get-managed-person-restart-2"
  (let ((managed-person (get-object-at-%id *test-pool* 'managed-person *kj*)))
    (is-person managed-person)
    (is-firstname managed-person "Kathryn")
    (is-lastname managed-person "Janeway")
    (is-firstname (get-person *test-pool* 'lastname "Janeway") "Kathryn")
    (is-lastname (get-person *test-pool* 'firstname "Kathryn") "Janeway")))

(subtest "test-managed-person-count"
  (mapcar #'(lambda (pair)
              (make-person *test-pool* 'firstname (first pair) 'lastname (second pair)))
          '(("Benjamin" "Sisko") ("James T." "Kirk") ("Jonathan" "Archer")))
  (ok (= (length (find-objects *test-pool* 'managed-person)) 5))
  (mapcar #'(lambda (pair)
              (delete-person *test-pool* (get-person *test-pool* 'firstname (first pair))))
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
      (setf new-managed-person (make-person *test-pool* 'firstname "John" 'lastname "Doe"))
      (ok *managed-guard*)
      (setf *managed-guard* nil)
      (delete-person *test-pool* new-managed-person)
      (ok *managed-guard*))))

(finalize)
