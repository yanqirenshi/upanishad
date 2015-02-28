;;;;;
;;;;; Contents
;;;;;   1. A Test CLOS class
;;;;;   2. Some basic functions to construct transactions from
;;;;;   3. A place to store our test person's id outside of the pool
;;;;;

(in-package :upanishad-test)

(in-suite test-pool)

(defparameter *test-pool-directory* (pathname "/tmp/test-up-pool/"))

(defvar *test-pool* nil)

(test test-pool-start
  "Create a new prevalence system for testing purposes"
  (let ((directory *test-pool-directory*))
    ;; Throw away any xml files that we find: we want to start from scratch
    (when (probe-file directory)
      (dolist (pathname (directory (merge-pathnames "*.xml" directory)))
        (delete-file pathname)))
    (setf *test-pool* (make-pool directory))
    (is-true *test-pool*)))

;; 1. A Test CLOS class

(defclass person ()
  ((id :initarg :id :accessor get-id)
   (firstname :initarg :firstname :accessor get-firstname)
   (lastname :initarg :lastname :accessor get-lastname)))

;; 2. Some basic functions to construct transactions from

(defun tx-create-persons-root (pool)
  (setf (get-root-object pool :persons) (make-hash-table)))

(defun tx-create-person (pool firstname lastname)
  (let* ((persons (get-root-object pool :persons))
         (id (next-id pool))
         (person (make-instance 'person :id id :firstname firstname :lastname lastname)))
    (setf (gethash id persons) person)))

(defun tx-delete-person (pool id)
  (let ((persons (get-root-object pool :persons)))
    (remhash id persons)))

(test create-counter
  "Test create a new id counter"
  (execute *test-pool* (make-transaction 'tx-create-id-counter))
  (is (zerop (get-root-object *test-pool* :id-counter))))

(test hash-table-test
  "Create the hash-table holding all known persistent persons and mapping person id' to person objects"
  (execute *test-pool* (make-transaction 'tx-create-persons-root))
  (is (hash-table-p (get-root-object *test-pool* :persons))))

;; 3. A place to store our test person's id outside of the pool
(defvar *jlp*)

(test test-create-person
  "Create a new test person"
  (let
      ((person (execute *test-pool* (make-transaction 'tx-create-person "Jean-Luc" "Picard"))))
    (is (eq (class-of person) (find-class 'person)))
    (is (equal (get-firstname person) "Jean-Luc"))
    (is (equal (get-lastname person) "Picard"))
    (setf *jlp* (get-id person))))

(test test-get-person :depends-on '(and test-create-person)
      (let ((person (gethash *jlp* (get-root-object *test-pool* :persons))))
        (is (eq (class-of person) (find-class 'person)))
        (is (equal (get-firstname person) "Jean-Luc"))
        (is (equal (get-lastname person) "Picard"))))

(test test-get-person-restart
  "Throw away the previous prevalence instance and start over,
   counting on a restore operation using the transaction log"
  (close-open-streams *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*))
  (let ((person (gethash *jlp* (get-root-object *test-pool* :persons))))
    (is (eq (class-of person) (find-class 'person)))
    (is (equal (get-firstname person) "Jean-Luc"))
    (is (equal (get-lastname person) "Picard"))))

(test test-get-person-snapshot
  "Create a snapshot of our test pool"
  (snapshot *test-pool*)
  (let ((person (gethash *jlp* (get-root-object *test-pool* :persons))))
    (is (eq (class-of person) (find-class 'person)))
    (is (equal (get-firstname person) "Jean-Luc"))
    (is (equal (get-lastname person) "Picard"))))

(test test-get-person-restart-snapshot
  "Throw away the previous prevalence instance and start over,
   counting on a restore operation using the snapshot"
  (close-open-streams *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*))
  (let ((person (gethash *jlp* (get-root-object *test-pool* :persons))))
    (is (eq (class-of person) (find-class 'person)))
    (is (equal (get-firstname person) "Jean-Luc"))
    (is (equal (get-lastname person) "Picard"))))

;; Create another test person

(defvar *kj*)

(test test-create-person-1
  (let ((person (execute *test-pool* (make-transaction 'tx-create-person "Kathryn" "Janeway"))))
    (is (eq (class-of person) (find-class 'person)))
    (is (equal (get-firstname person) "Kathryn"))
    (is (equal (get-lastname person) "Janeway"))
    (setf *kj* (get-id person))))

(test test-get-person-1
  (let ((person (gethash *kj* (get-root-object *test-pool* :persons))))
    (is (eq (class-of person) (find-class 'person)))
    (is (equal (get-firstname person) "Kathryn"))
    (is (equal (get-lastname person) "Janeway"))))

(test test-get-person-restart-1
  "Throw away the previous prevalence instance and start over,
  counting on a restore operation using both the snapshot and the transaction log"
  (close-open-streams *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*))
  (let ((person (gethash *jlp* (get-root-object *test-pool* :persons))))
    (is (eq (class-of person) (find-class 'person)))
    (is (equal (get-firstname person) "Jean-Luc"))
    (is (equal (get-lastname person) "Picard"))))

(test test-get-person-restart-2
  (let ((person (gethash *kj* (get-root-object *test-pool* :persons))))
    (is (eq (class-of person) (find-class 'person)))
    (is (equal (get-firstname person) "Kathryn"))
    (is (equal (get-lastname person) "Janeway"))))

(test test-person-count
  (mapcar #'(lambda (pair)
              (execute *test-pool* (make-transaction 'tx-create-person (car pair) (cadr pair))))
          '(("Benjamin" "Sisko") ("James T." "Kirk") ("Jonathan" "Archer")))
  (is (= (hash-table-count (get-root-object *test-pool* :persons)) 5))
  (mapcar #'(lambda (id)
              (execute *test-pool* (make-transaction 'tx-delete-person id)))
          '(2 3 4))
  (is (= (hash-table-count (get-root-object *test-pool* :persons)) 2)))

(defvar *guard*)

(defun guard (thunk)
  (setf *guard* t)
  (funcall thunk))

(test test-guarded
  "testing a guarded prevalence system"
  (close-open-streams *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*
                               :pool-class 'guarded-pool))
  (setf (get-guard *test-pool*) #'guard)
  (let (new-person)
    (setf *guard* nil)
    (setf new-person (execute *test-pool* (make-transaction 'tx-create-person "John" "Doe")))
    (is-true *guard*)
    (setf *guard* nil)
    (execute *test-pool* (make-transaction 'tx-delete-person (get-id new-person)))
    (is-true *guard*)))



#|
-*- mode: Lisp -*-

$Id$

Testing Object Prevalence in Common Lisp

Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.

You are granted the rights to distribute and use this software as governed by the terms of the Lisp Lesser General Public License (http://opensource.franz.com/preamble.html), also known as the LLGPL.
|#

