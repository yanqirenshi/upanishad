;;;;;
;;;;; Defince Classes
;;;;;
(in-package :upanishad)


;;;;;
;;;;; supser class
;;;;;
;;; Brahman
(defclass brahman () ()
  (:documentation "思想的/象徴的なクラス。今んところ意味はないけぇ。"))

;;; Atman
(defclass atman (brahman)
  ((%id :documentation "Return an external, unique, immutable identifier for object (typically an integer)"
        :reader %id
        :initarg :%id
        :initform -1))
  (:documentation "Superclass for objects with an id"))

;;; Meme
(defclass meme (atman)
  ()
  (:documentation "このクラスも思想的/象徴的なクラスです。まぁぶっちゃけ不要なんですけど。"))


;;;;;
;;;;; Blob
;;;;;
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



;;;;;
;;;;; pool
;;;;;
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
    :initform (make-hash-table :test 'eq))
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


;;; object
(defclass object (meme)
  ((contents :documentation ""
             :accessor contents
             :initarg :contents
             :initform nil)
   (%id-ht :documentation ""
           :accessor %id-ht
           :initarg :%id-ht
           :initform (make-hash-table))))


;;; index
(defclass index (meme)
  ((object-symbol :documentation ""
                  :accessor object-symbol
                  :initarg :object-symbol
                  :initform nil)
   (slot-symbol :documentation ""
                :accessor slot-symbol
                :initarg :slot-symbol
                :initform nil)
   (contents :documentation ""
             :accessor contents
             :initarg :contents
             :initform (make-hash-table :test 'equalp))))


;;;;;
;;;;; guarded-pool
;;;;;
(defclass guarded-pool (pool)
  ((guard
    :documentation ":type function"
    :accessor guard
    :initform #'(lambda (thunk) (funcall thunk))))
  (:documentation "A Prevalence system with a guard thunk"))


;;;;;
;;;;; transaction
;;;;;
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
