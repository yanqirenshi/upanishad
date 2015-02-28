;;;;;
;;;;; Contents
;;;;;  1. A Test CLOS class
;;;;;  2. Convenience function
;;;;;  3. Some basic functions to construct transactions from
;;;;;  4. A place to store our test managed-person's id outside of the pool
;;;;;

(in-package :upanishad-test)

(in-suite test-managed-pool)

(defparameter *test-pool-directory* (pathname "/tmp/test-managed-pool/"))

(defvar *test-pool* nil)

(test test-managed-pool-start
  "Create a new prevalence pool for testing purposes"
  (let ((directory *test-pool-directory*))
    ;; Throw away any xml files that we find: we want to start from scratch
    (when (probe-file directory)
      (dolist (pathname (directory (merge-pathnames "*.xml" directory)))
        (delete-file pathname)))
    (setf *test-pool* (make-pool directory))
    (is-true *test-pool*)
    (index-on *test-pool* 'managed-person '(firstname lastname) 'equal)))

;; 1. A Test CLOS class

(defclass managed-person (meme)
  ((firstname :initarg :firstname :initform "" :accessor get-firstname)
   (lastname  :initarg :lastname  :initform "" :accessor get-lastname)))

(defmethod (setf get-firstname) (value (managed-person managed-person))
  (execute-transaction
   (tx-change-object-slots *test-pool* 'managed-person (get-id managed-person) (list (list 'firstname value)))))

(defmethod (setf get-lastname) (value (managed-person managed-person))
  (execute-transaction
   (tx-change-object-slots *test-pool* 'managed-person (get-id managed-person) (list (list 'lastname value)))))

;; 2. Convenience function

(defun pairify (list)
  (when list (concatenate 'list
                          (list (subseq list 0 2))
                          (pairify (rest (rest list))))))

;; 3. Some basic functions to construct transactions from

(defun make-managed-person (&rest slots)
  (let ((slots-and-values (pairify slots)))
    (execute-transaction
     (tx-create-object *test-pool* 'managed-person slots-and-values))))

(defun get-managed-person (slot value)
  (first (find-object-with-slot *test-pool* 'managed-person slot value)))

(defun delete-managed-person (managed-person)
  (execute-transaction
   (tx-delete-object *test-pool* 'managed-person (get-id managed-person))))

(test test-create-counter
  "Create a new id counter"
  (execute-transaction (tx-create-id-counter *test-pool*))
  (is (zerop (get-root-object *test-pool* :id-counter))))


;; 4. A place to store our test managed-person's id outside of the pool

(defvar *jlp*)

(test test-create-managed-person
  "Create a new test managed-person"
  (let ((managed-person (make-managed-person 'firstname "Jean-Luc" 'lastname "Picard")))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Jean-Luc"))
    (is (equal (get-lastname managed-person) "Picard"))
    (is (equal (get-lastname (get-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (get-managed-person 'lastname "Picard")) "J-Lu")
    (is (equal (get-lastname (get-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (get-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (is (equal (get-firstname (get-managed-person 'lastname "Picard")) "Jean-Luc"))
    (is (eq NIL (get-managed-person 'firstname "J-Lu")))
    (setf *jlp* (get-id managed-person))))

(test test-get-managed-person
  (let ((managed-person (get-object-with-id *test-pool* 'managed-person *jlp*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Jean-Luc"))
    (is (equal (get-lastname managed-person) "Picard"))
    (is (equal (get-lastname (get-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (get-managed-person 'lastname "Picard")) "J-Lu")
    (is (equal (get-lastname (get-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (get-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (is (equal (get-firstname (get-managed-person 'lastname "Picard")) "Jean-Luc"))
    (is (eq NIL (get-managed-person 'firstname "J-Lu")))))

(test test-get-managed-person-restart
  "Throw away the previous prevalence instance and start over,
  counting on a restore operation using the transaction log"
  (close-open-streams *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*))
  (let ((managed-person (get-object-with-id *test-pool* 'managed-person *jlp*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Jean-Luc"))
    (is (equal (get-lastname managed-person) "Picard"))
    (is (equal (get-lastname (get-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (get-managed-person 'lastname "Picard")) "J-Lu")
    (is (equal (get-lastname (get-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (get-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (is (equal (get-firstname (get-managed-person 'lastname "Picard")) "Jean-Luc"))
    (is (eq NIL (get-managed-person 'firstname "J-Lu")))))

(test test-get-managed-person-snapshot
  "Create a snapshot of our test pool"
  (snapshot *test-pool*)
  (let ((managed-person (get-object-with-id *test-pool* 'managed-person *jlp*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Jean-Luc"))
    (is (equal (get-lastname managed-person) "Picard"))
    (is (equal (get-lastname (get-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (get-managed-person 'lastname "Picard")) "J-Lu")
    (is (equal (get-lastname (get-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (get-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (is (equal (get-firstname (get-managed-person 'lastname "Picard")) "Jean-Luc"))
    (is (eq NIL (get-managed-person 'firstname "J-Lu")))))

(test test-get-managed-person-restart-snapshot
  "Throw away the previous prevalence instance and start over,
  counting on a restore operation using the snapshot"
  (close-open-streams *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*))
  (let ((managed-person (get-object-with-id *test-pool* 'managed-person *jlp*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Jean-Luc"))
    (is (equal (get-lastname managed-person) "Picard"))
    (is (equal (get-lastname (get-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (get-managed-person 'lastname "Picard")) "J-Lu")
    (is (equal (get-lastname (get-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (get-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (is (equal (get-firstname (get-managed-person 'lastname "Picard")) "Jean-Luc"))
    (is (eq NIL (get-managed-person 'firstname "J-Lu")))))

(defvar *kj*)

(test test-create-managed-person-1
  "Create another test managed-person"
  (let ((managed-person (make-managed-person 'firstname "Kathryn" 'lastname "Janeway")))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Kathryn"))
    (is (equal (get-lastname managed-person) "Janeway"))
    (is (equal (get-firstname (get-managed-person 'lastname "Janeway")) "Kathryn"))
    (is (equal (get-lastname (get-managed-person 'firstname "Kathryn")) "Janeway"))
    (setf *kj* (get-id managed-person))))

(test test-get-managed-person-1
  (let ((managed-person (get-object-with-id *test-pool* 'managed-person *kj*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Kathryn"))
    (is (equal (get-lastname managed-person) "Janeway"))
    (is (equal (get-firstname (get-managed-person 'lastname "Janeway")) "Kathryn"))
    (is (equal (get-lastname (get-managed-person 'firstname "Kathryn")) "Janeway"))))

(test test-get-managed-person-restart-1
  "Throw away the previous prevalence instance and start over,
   counting on a restore operation using both the snapshot and the transaction log"
  (close-open-streams *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*))
  (let ((managed-person (get-object-with-id *test-pool* 'managed-person *jlp*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Jean-Luc"))
    (is (equal (get-lastname managed-person) "Picard"))
    (is (equal (get-lastname (get-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (get-managed-person 'lastname "Picard")) "J-Lu")
    (is (equal (get-lastname (get-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (get-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (is (equal (get-firstname (get-managed-person 'lastname "Picard")) "Jean-Luc"))
    (is (eq NIL (get-managed-person 'firstname "J-Lu")))))

(test test-get-managed-person-restart-2
  (let ((managed-person (get-object-with-id *test-pool* 'managed-person *kj*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Kathryn"))
    (is (equal (get-lastname managed-person) "Janeway"))
    (is (equal (get-firstname (get-managed-person 'lastname "Janeway")) "Kathryn"))
    (is (equal (get-lastname (get-managed-person 'firstname "Kathryn")) "Janeway"))))

(test test-managed-person-count
  (mapcar #'(lambda (pair)
              (make-managed-person 'firstname (first pair) 'lastname (second pair)))
          '(("Benjamin" "Sisko") ("James T." "Kirk") ("Jonathan" "Archer")))
  (is (= (length (find-all-objects *test-pool* 'managed-person)) 5))
  (mapcar #'(lambda (pair)
              (delete-managed-person (get-managed-person 'firstname (first pair))))
          '(("Benjamin" "Sisko") ("James T." "Kirk") ("Jonathan" "Archer")))
  (is (= (length (find-all-objects *test-pool* 'managed-person)) 2)))

(defvar *managed-guard*)

(defun managed-guard (thunk)
  (setf *managed-guard* t)
  (funcall thunk))

(test test-managed-guarded
  "testing a managed-guarded prevalence system
   [Not sure that we need the below test here -- RRR]"
  (close-open-streams *test-pool*)
  (setf *test-pool* (make-pool *test-pool-directory*
                               :pool-class 'guarded-pool))
  (setf (get-guard *test-pool*) #'managed-guard)
  (let (new-managed-person)
    (setf *managed-guard* nil)
    (setf new-managed-person (make-managed-person 'firstname "John" 'lastname "Doe"))
    (is-true *managed-guard*)
    (setf *managed-guard* nil)
    (delete-managed-person new-managed-person)
    (is-true *managed-guard*)))



#|
-*- mode: Lisp -*-

$Id$

Testing Managed Object Prevalence in Common Lisp

Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
Altered for managed prevalence testing; Sept 2004, Randall Randall, RandallSquared

You are granted the rights to distribute and use this software as governed by the terms of the Lisp Lesser General Public License (http://opensource.franz.com/preamble.html), also known as the LLGPL.
|#
