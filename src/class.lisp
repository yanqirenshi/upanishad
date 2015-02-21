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
       :reader get-id
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
  ((directory 
    :documentation ":type pathname"
    :accessor get-directory
    :initarg :directory)
   (root-objects 
    :documentation ":type hash-table"
    :accessor get-root-objects
    :initform (make-hash-table :test 'eq))
   (options
    :documentation ":type hash-table"
    :initform (make-hash-table :test 'eq))
   (snapshot 
    :documentation ":type pathname"
    :accessor get-snapshot)
   (transaction-log
    :documentation ":type pathname"
    :accessor get-transaction-log)
   (transaction-log-stream
    :documentation ":type stream"
    :accessor get-transaction-log-stream
    :initform nil)
   (serializer
    :documentation ":type function"
    :accessor get-serializer
    :initarg :serializer
    :initform #'serialize-xml)
   (deserializer
    :documentation ":type function"
    :accessor get-deserializer
    :initarg :deserializer
    :initform #'deserialize-xml)
   (file-extension
    :documentation ":type string"
    :accessor get-file-extension
    :initarg :file-extension
    :initform "xml")
   (serialization-state
    :documentation ":type serialization-state"
    :reader get-serialization-state
    :initform (make-serialization-state))
   (transaction-hook
    :documentation ":type function"
    :accessor get-transaction-hook
    :initarg :transaction-hook
    :initform #'identity))
  (:documentation "Base Prevalence system implementation object"))



(defclass guarded-pool (pool)
  ((guard
    :documentation ":type function"
    :accessor get-guard
    :initform #'(lambda (thunk) (funcall thunk))))
  (:documentation "A Prevalence system with a guard thunk"))



(defclass transaction (brahman)
  ((args
    :documentation ":type cons"
    :accessor get-args
    :initarg :args
    :initform nil)
   (function
    :documentation ":type symbol"
    :accessor get-function
    :initarg :function
    :initform 'identity))
  (:documentation "A simple Transaction object joining a function and its arguments"))



;;;
;;; Getter / Setter
;;;
