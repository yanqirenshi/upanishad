(in-package :upanishad-test)

;;;
;;; preparation
;;;
(defparameter *test-pool-directory* (pathname "/tmp/test-up-pool/"))

(defvar *test-pool* nil)

(defclass person ()
  ((%id :initarg :%id :accessor %id)
   (firstname :initarg :firstname :accessor get-firstname)
   (lastname :initarg :lastname :accessor get-lastname)))

(defun tx-create-persons-root (pool)
  (setf (get-root-object pool :persons) (make-hash-table)))

(defun tx-create-person (pool firstname lastname)
  (let* ((persons (get-root-object pool :persons))
         (%id (next-%id pool))
         (person (make-instance 'person :%id %id :firstname firstname :lastname lastname)))
    (setf (gethash %id persons) person)))

(defun tx-delete-person (pool id)
  (let ((persons (get-root-object pool :persons)))
    (remhash id persons)))

(defvar *jlp*)

(defvar *kj*)

(defvar *guard*)

(defun guard-function (thunk)
  (setf *guard* t)
  (funcall thunk))

;;;
;;; Test
;;;
(plan 6)

(subtest "test-pool-start"
  "Create a new prevalence system for testing purposes"
  (let ((directory *test-pool-directory*))
    (clear-pool-datastor directory)
    (setf *test-pool* (make-pool directory))
    (ok *test-pool* "success created pool")))

(subtest "create-counter"
  "Test create a new id counter"
  (execute *test-pool* (make-transaction 'tx-create-%id-counter))
  (ok (zerop (get-root-object *test-pool* :%id-counter))
      "Created counter"))

(subtest "hash-table-test"
  "Create the hash-table holding all known persistent persons and mapping person id' to person objects"
  (execute *test-pool* (make-transaction 'tx-create-persons-root))
  (ok (hash-table-p (get-root-object *test-pool* :persons))))

;;;
;;; A place to store our test person's id outside of the pool
;;;
(subtest "A place to store our test person's id outside of the pool"
  (subtest "test-create-person"
    "Create a new test person"
    (let
        ((person (execute *test-pool* (make-transaction 'tx-create-person "Jean-Luc" "Picard"))))
      (ok (eq (class-of person) (find-class 'person)))
      (ok (equal (get-firstname person) "Jean-Luc"))
      (ok (equal (get-lastname person) "Picard"))
      (setf *jlp* (%id person))))

  (subtest "test-get-person" ;; :depends-on '(and test-create-person)
    (let ((person (gethash *jlp* (get-root-object *test-pool* :persons))))
      (ok (eq (class-of person) (find-class 'person)))
      (ok (equal (get-firstname person) "Jean-Luc"))
      (ok (equal (get-lastname person) "Picard"))))

  (subtest "test-get-person-restart"
    "Throw away the previous prevalence instance and start over,
   counting on a restore operation using the transaction log"
    (stop *test-pool*)
    (setf *test-pool* (make-pool *test-pool-directory*))
    (let ((person (gethash *jlp* (get-root-object *test-pool* :persons))))
      (ok (eq (class-of person) (find-class 'person)))
      (ok (equal (get-firstname person) "Jean-Luc"))
      (ok (equal (get-lastname person) "Picard"))))

  (subtest "test-get-person-snapshot"
    "Create a snapshot of our test pool"
    (snapshot *test-pool*)
    (let ((person (gethash *jlp* (get-root-object *test-pool* :persons))))
      (ok (eq (class-of person) (find-class 'person)))
      (ok (equal (get-firstname person) "Jean-Luc"))
      (ok (equal (get-lastname person) "Picard"))))

  (subtest "test-get-person-restart-snapshot"
    "Throw away the previous prevalence instance and start over,
   counting on a restore operation using the snapshot"
    (stop *test-pool*)
    (setf *test-pool* (make-pool *test-pool-directory*))
    (let ((person (gethash *jlp* (get-root-object *test-pool* :persons))))
      (ok (eq (class-of person) (find-class 'person)))
      (ok (equal (get-firstname person) "Jean-Luc"))
      (ok (equal (get-lastname person) "Picard")))))

;;;
;;; Create another test person
;;;
(subtest "Create another test person"
  (subtest "test-create-person-1"
    (let ((person (execute *test-pool* (make-transaction 'tx-create-person "Kathryn" "Janeway"))))
      (ok (eq (class-of person) (find-class 'person)))
      (ok (equal (get-firstname person) "Kathryn"))
      (ok (equal (get-lastname person) "Janeway"))
      (setf *kj* (%id person))))

  (subtest "test-get-person-1"
    (let ((person (gethash *kj* (get-root-object *test-pool* :persons))))
      (ok (eq (class-of person) (find-class 'person)))
      (ok (equal (get-firstname person) "Kathryn"))
      (ok (equal (get-lastname person) "Janeway"))))

  (subtest "test-get-person-restart-1"
    "Throw away the previous prevalence instance and start over,
  counting on a restore operation using both the snapshot and the transaction log"
    (stop *test-pool*)
    (setf *test-pool* (make-pool *test-pool-directory*))
    (let ((person (gethash *jlp* (get-root-object *test-pool* :persons))))
      (ok (eq (class-of person) (find-class 'person)))
      (ok (equal (get-firstname person) "Jean-Luc"))
      (ok (equal (get-lastname person) "Picard"))))

  (subtest "test-get-person-restart-2"
    (let ((person (gethash *kj* (get-root-object *test-pool* :persons))))
      (ok (eq (class-of person) (find-class 'person)))
      (ok (equal (get-firstname person) "Kathryn"))
      (ok (equal (get-lastname person) "Janeway"))))

  (subtest "test-person-count"
    (mapcar #'(lambda (pair)
                (execute *test-pool* (make-transaction 'tx-create-person (car pair) (cadr pair))))
            '(("Benjamin" "Sisko") ("James T." "Kirk") ("Jonathan" "Archer")))
    (ok (= (hash-table-count (get-root-object *test-pool* :persons)) 5))
    (mapcar #'(lambda (id)
                (execute *test-pool* (make-transaction 'tx-delete-person id)))
            '(2 3 4))
    (ok (= (hash-table-count (get-root-object *test-pool* :persons)) 2))))

(subtest "Test Guard"
  (subtest "test-guarded"
    "testing a guarded prevalence system"
    (stop *test-pool*)
    (setf *test-pool* (make-pool *test-pool-directory*
                                 :pool-class 'guarded-pool))
    (setf (guard *test-pool*) #'guard-function)
    (let (new-person)
      (setf *guard* nil)
      (setf new-person (execute *test-pool* (make-transaction 'tx-create-person "John" "Doe")))
      (ok *guard*)
      (setf *guard* nil)
      (execute *test-pool* (make-transaction 'tx-delete-person (%id new-person)))
      (ok *guard*))))

(finalize)
