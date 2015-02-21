(in-package :upanishad)

;;;
;;; Class Graph
;;; -----------
;;;
;;;   +---------+
;;;   | brahman |
;;;   |=========|
;;;   |---------|
;;;   +---------+
;;;        ^
;;;        |
;;;        +------------------+--------------------------+
;;;        |                  |                          |
;;;   +---------+       +-------------+     +-------------------------+
;;;   | atman   |       | transaction |     | pool                    |
;;;   |=========|       |=============|     |=========================|
;;;   |  id     |       |  args       |     |  directory              |
;;;   |---------|       |  function   |     |  root-objects           |
;;;   +---------+       |-------------|     |  options                |
;;;        ^            +-------------+     |  snapshot               |
;;;        |                                |  transaction-log        |
;;;   +-----------+                         |  transaction-log-stream |
;;;   | blob      |                         |  serializer             |
;;;   |===========|                         |  deserializer           |
;;;   | name      |                         |  file-extension         |
;;;   | size      |                         |  serialization-state    |
;;;   | mime-type |                         |  transaction-hook       |
;;;   | keywords  |                         |-------------------------|
;;;   |-----------|                         +-------------------------+
;;;   +-----------+                                      ^
;;;                                                      |
;;;                                               +--------------+
;;;                                               | guarded-pool |
;;;                                               |==============|
;;;                                               | guard        |
;;;                                               |--------------|
;;;                                               +--------------+
;;;

;;;
;;; Class
;;;
(defclass brahman () ()
  (:documentation "思想的なもの。今んところ意味はないけぇ。"))



(defclass atman (brahman)
  ((id :documentation "Return an external, unique, immutable identifier for object (typically an integer)"
       :reader  get-id
       :initarg :id
       :initform -1))
  (:documentation "Superclass for objects with an id"))



(defclass blob (atman)
  ((name :documentation "Return the descriptive name of blob. Set the descriptive name of blob."
         :accessor get-name
         :initarg :name
         :initform "untitled")
   (size :documentation "Return the size of blob in bytes. Set the mime-type string of blob."
         :reader get-size
         :initarg :size
         :initform -1)
   (mime-type :documentation "Return the mime-type of blob as a string. Set the keywords list of blob"
              :accessor get-mime-type
              :initarg :mime-type
              :initform "application/octet-stream")
   (keywords :documentation "Return the list of keywords associated with blob"
             :accessor get-keywords
             :initarg :keywords
             :initform '()))
  (:documentation "A blob is a file-like collection of bytes with related metadata"))



(defclass pool (brahman)
  ((directory ;; :type pathname
    :initarg :directory
    :accessor get-directory)
   (root-objects ;; :type hash-table
    :accessor get-root-objects
    :initform (make-hash-table :test 'eq))
   (options ;; :type hash-table
    :initform (make-hash-table :test 'eq))
   (snapshot ;; :type pathname
    :accessor get-snapshot)
   (transaction-log ;; :type pathname
    :accessor get-transaction-log)
   (transaction-log-stream ;; :type stream
    :accessor get-transaction-log-stream
    :initform nil)
   (serializer ;; type function
    :accessor get-serializer
    :initarg :serializer
    :initform #'serialize-xml)
   (deserializer ;; type function
    :accessor get-deserializer
    :initarg :deserializer
    :initform #'deserialize-xml)
   (file-extension ;; type string
    :accessor get-file-extension
    :initarg :file-extension
    :initform "xml")
   (serialization-state ;; type serialization-state
    :reader get-serialization-state
    :initform (make-serialization-state))
   (transaction-hook ;; type function
    :accessor get-transaction-hook
    :initarg :transaction-hook
    :initform #'identity))
  (:documentation "Base Prevalence system implementation object"))



(defclass guarded-pool (pool)
  ((guard ;; :type function
    :accessor get-guard
    :initform #'(lambda (thunk) (funcall thunk))))
  (:documentation "A Prevalence system with a guard thunk"))



(defclass transaction (brahman)
  ((args ;; :type cons
    :initarg :args
    :accessor get-args
    :initform nil)
   (function ;; :type symbol
    :initarg :function
    :accessor get-function
    :initform 'identity))
  (:documentation "A simple Transaction object joining a function and its arguments"))



;;;
;;; Getter / Setter
;;;
