;;;;;
;;;;; Defince Classes
;;;;;
;;;;; Contetns
;;;;;  1. Class Graph
;;;;;  2. Brahman
;;;;;  3. Atman
;;;;;  4. Blob
;;;;;  5. pool
;;;;;  6. guarded-pool
;;;;;  7. transaction
;;;;;  8. define Getter and Setter
;;;;;

(in-package :upanishad)


;;;
;;; 1. Class Graph
                                        ;
                                        ;   +---------+
                                        ;   | brahman |
                                        ;   |=========|
                                        ;   |---------|
                                        ;   +---------+
                                        ;        ^
                                        ;        |
                                        ;        +------------------+--------------------------+
                                        ;        |                  |                          |
                                        ;   +---------+       +-------------+     +-------------------------+
                                        ;   | atman   |       | transaction |     | pool                    |
                                        ;   |=========|       |=============|     |=========================|
                                        ;   |r id     |       |a args       |     |a directory              |
                                        ;   |---------|       |a function   |     |a root-objects           |
                                        ;   +---------+       |-------------|     |- options                |
                                        ;        ^            +-------------+     |a snapshot               |
                                        ;        |                                |a transaction-log        |
                                        ;   +------------+                        |a transaction-log-stream |
                                        ;   | blob       |                        |a serializer             |
                                        ;   |============|                        |a deserializer           |
                                        ;   |a name      |                        |a file-extension         |
                                        ;   |r size      |                        |r serialization-state    |
                                        ;   |a mime-type |                        |a transaction-hook       |
                                        ;   |a keywords  |                        |-------------------------|
                                        ;   |------------|                        +-------------------------+
                                        ;   +------------+                                     ^
                                        ;                                                      |
                                        ;                                               +--------------+
                                        ;                                               | guarded-pool |
                                        ;                                               |==============|
                                        ;                                               |a guard       |
                                        ;                                               |--------------|
                                        ;                                               +--------------+
                                        ;

;;;
;;; 2. Brahman
;;;
(defclass brahman () ()
  (:documentation "思想的なもの。今んところ意味はないけぇ。"))



;;;
;;; 3. Atman
;;;
(defclass atman (brahman)
  ((id :documentation "Return an external, unique, immutable identifier for object (typically an integer)"
       :reader id
       :initarg :id
       :initform -1))
  (:documentation "Superclass for objects with an id"))



;;;
;;; 4. Blob
;;;
(defclass blob (atman)
  ((name :documentation "Return the descriptive name of blob. Set the descriptive name of blob."
         :accessor name
         :initarg :name
         :initform "untitled")
   (size :documentation "Return the size of blob in bytes. Set the mime-type string of blob."
         :reader size
         :initarg :size
         :initform -1)
   (mime-type :documentation "Return the mime-type of blob as a string. Set the keywords list of blob"
              :accessor mime-type
              :initarg :mime-type
              :initform "application/octet-stream")
   (keywords :documentation "Return the list of keywords associated with blob"
             :accessor keywords
             :initarg :keywords
             :initform '()))
  (:documentation "A blob is a file-like collection of bytes with related metadata"))



;;;
;;; 5. pool
;;;
(defclass pool (brahman)
  ((directory
    :documentation ":type pathname"
    :accessor get-directory
    :initarg :directory)
   (root-objects
    :documentation ":type hash-table"
    :accessor root-objects
    :initform (make-hash-table :test 'eq))
   (options
    :documentation ":type hash-table"
    :initform (make-hash-table :test 'eq))
   (snapshot
    :documentation ":type pathname"
    :accessor get-snapshot)
   (transaction-log
    :documentation ":type pathname"
    :accessor transaction-log)
   (transaction-log-stream
    :documentation ":type stream"
    :accessor transaction-log-stream
    :initform nil)
   (serializer
    :documentation ":type function"
    :accessor serializer
    :initarg :serializer
    :initform #'serialize-xml)
   (deserializer
    :documentation ":type function"
    :accessor deserializer
    :initarg :deserializer
    :initform #'deserialize-xml)
   (file-extension
    :documentation ":type string"
    :accessor file-extension
    :initarg :file-extension
    :initform "xml")
   (serialization-state
    :documentation ":type serialization-state"
    :reader serialization-state
    :initform (make-serialization-state))
   (transaction-hook
    :documentation ":type function"
    :accessor transaction-hook
    :initarg :transaction-hook
    :initform #'identity))
  (:documentation "Base Prevalence system implementation object"))



;;;
;;; 6. guarded-pool
;;;
(defclass guarded-pool (pool)
  ((guard
    :documentation ":type function"
    :accessor guard
    :initform #'(lambda (thunk) (funcall thunk))))
  (:documentation "A Prevalence system with a guard thunk"))



;;;
;;; 7. transaction
;;;
(defclass transaction (brahman)
  ((args
    :documentation ":type cons"
    :accessor args
    :initarg :args
    :initform nil)
   (function
    :documentation ":type symbol"
    :accessor get-function
    :initarg :function
    :initform 'identity))
  (:documentation "A simple Transaction object joining a function and its arguments"))



;;;
;;; 8. define Getter and Setter
;;;

;; atman
(defreader atman id)

;; blob
(defreader blob name)
(defwriter blob name)
;; (defreader blob size)
(defreader blob mime-type)
(defwriter blob mime-type)
(defreader blob keywords)
(defwriter blob keywords)

;; pool
;; (defreader pool directory)
;; (defwriter pool directory)
(defreader pool root-objects)
(defwriter pool root-objects)
;; (defreader pool snapshot)
;; (defwriter pool snapshot)
(defreader pool transaction-log)
(defwriter pool transaction-log)
(defreader pool transaction-log-stream)
(defwriter pool transaction-log-stream)
(defreader pool serializer)
(defwriter pool serializer)
(defreader pool deserializer)
(defwriter pool deserializer)
(defreader pool file-extension)
(defwriter pool file-extension)
(defreader pool serialization-state)
(defreader pool transaction-hook)
(defwriter pool transaction-hook)

;; guarded-pool
(defreader pool guard)
(defwriter pool guard)

;; transaction
(defreader pool args)
(defwriter pool args)
;; (defreader pool function)
;; (defwriter pool function)
