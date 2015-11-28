(in-package :upanishad-test)

;;;
;;; 1. the master and client systems themselves
;;;

(defparameter *master-test-pool-directory* (pathname "/tmp/master-test-pool/"))

(defvar *master-test-pool* nil)

(defparameter *slave-test-pool-directory* (pathname "/tmp/slave-test-pool/"))

(defvar *slave-test-pool* nil)


;;;
;;; 2. a test object class
;;;
(defclass test-pool-user (meme)
  ((username :accessor get-username :initarg :username :initform nil)
   (password :accessor get-password :initarg :password :initform nil)))

(defvar *slave-server-name* nil)

(defvar *user-%id* nil)

;;;
;;; Test
;;;

(plan 4)

(subtest "test-master-slave-start"
  "setup both systems (clearing anything we find)
  setup the slave server and the master to slave connection"
  (when *master-test-pool*
    (totally-destroy *master-test-pool*))

  (setf *master-test-pool* (make-pool *master-test-pool-directory*))
  (ok *master-test-pool*)
  (totally-destroy *master-test-pool*)
  (execute-transaction (tx-create-%id-counter *master-test-pool*))

  (when *slave-test-pool*
    (totally-destroy *slave-test-pool*))
  (setf *slave-test-pool* (make-pool *slave-test-pool-directory*))
  (ok *slave-test-pool*)
  (totally-destroy *slave-test-pool*)
  (execute-transaction (tx-create-%id-counter *slave-test-pool*))
  (setf *slave-server-name* (start-slave-server *slave-test-pool*))
  (ok *slave-server-name*)

  (start-master-client *master-test-pool*)
  (let ((user (execute-transaction (tx-create-object *master-test-pool*
                                                     'test-pool-user
                                                     '((username "billg")
                                                       (password "windows"))))))
    (setf *user-%id* (%id user)))
  (ok *user-%id*)
  *user-%id*)


;;;
;;; 3. now do the test
;;;
(subtest "test-get-master-user"
  (let ((user (get-object-with-%id *master-test-pool* 'test-pool-user *user-%id*)))
    (ok (and (equal (get-username user) "billg")
             (equal (get-password user) "windows")))))

(subtest "test-get-slave-user" ;; :depends-on '(and test-get-master-user)
  ;; Plato Wu,2009/02/27: because it need time to transfer data from master to slave?
  (sleep 1)
  (let ((user (get-object-with-%id *slave-test-pool* 'test-pool-user *user-%id*)))
    (ok (and (equal (get-username user) "billg")
             (equal (get-password user) "windows")))))

(subtest "test-master-slave-end"
  " stop the master-slave connection and slave server
  tidy up a bit"
  (stop-master-client *master-test-pool*)
  (stop-slave-server *slave-server-name*)

  (close-open-streams *master-test-pool*)
  (close-open-streams *slave-test-pool*))

(finalize)
