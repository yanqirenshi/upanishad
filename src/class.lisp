;;;;;
;;;;; Defince Classes
;;;;;
;;;;; Contetns
;;;;;  1. Class Graph
;;;;;  2. Brahman
;;;;;  3. Atman
;;;;;  4. Meme
;;;;;  5. Blob
;;;;;  6. pool
;;;;;  7. guarded-pool
;;;;;  8. transaction
;;;;;  9. define Getter and Setter
;;;;;

(in-package :upanishad)

;;;
;;; 1. Class Graph
;;
;;   +---------+
;;   | brahman |
;;   |=========|
;;   |---------|
;;   +---------+
;;        ^
;;        |
;;        +--------------------------+-----------------------+
;;        |                          |                       |
;;   +---------+                     |                       |
;;   | atman   |                     |                       |
;;   |=========|                     |                       |
;;   |r id     |                     |                       |
;;   |---------|                     |                       |
;;   +---------+                     |                       |
;;        ^                          |                       |
;;        |                          |                       |
;;   +------------+     +-------------------------+    +-------------+
;;   | meme       |     | pool                    |    | transaction |
;;   |============|     |=========================|    |=============|
;;   |------------|     |a directory              |    |a args       |
;;   +------------+     |a root-objects           |    |a function   |
;;        ^             |- options                |    |-------------|
;;        |             |a snapshot               |    +-------------+
;;   +------------+     |a transaction-log        |
;;   | blob       |     |a transaction-log-stream |
;;   |============|     |a serializer             |
;;   |a name      |     |a deserializer           |
;;   |r size      |     |a file-extension         |
;;   |a mime-type |     |r serialization-state    |
;;   |a keywords  |     |a transaction-hook       |
;;   |------------|     |-------------------------|
;;   +------------+     +-------------------------+
;;                                   ^
;;                                   |
;;                            +--------------+
;;                            | guarded-pool |
;;                            |==============|
;;                            |a guard       |
;;                            |--------------|
;;                            +--------------+
;;

;;;
;;; 2. Brahman
;;;
(defclass brahman () ()
  (:documentation "思想的/象徴的なクラス。今んところ意味はないけぇ。"))



;;;
;;; 3. Atman
;;;
(defclass atman (brahman)
  ((%id :documentation "Return an external, unique, immutable identifier for object (typically an integer)"
        :reader %id
        :initarg :%id
        :initform -1))
  (:documentation "Superclass for objects with an id"))



;;;
;;; 4. Meme
;;;
(defclass meme (atman)
  ()
  (:documentation "このクラスも思想的/象徴的なクラスです。まぁぶっちゃけ不要なんですけど。"))



;;;
;;; 5. Blob
;;;
(defclass blob (meme)
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
;;; 6. pool
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
   (index-objects
    :documentation ":type hash-table"
    :accessor index-objects
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
;;; 7. guarded-pool
;;;
(defclass guarded-pool (pool)
  ((guard
    :documentation ":type function"
    :accessor guard
    :initform #'(lambda (thunk) (funcall thunk))))
  (:documentation "A Prevalence system with a guard thunk"))



;;;
;;; 8. transaction
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
