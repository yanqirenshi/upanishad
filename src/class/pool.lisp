(in-package :upanishad)

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
   (memes
    :documentation ":type hash-table"
    :accessor memes
    :initform (make-hash-table :test 'eq))
   (indexes
    :documentation ":type hash-table"
    :accessor indexes
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


;;;;;
;;;;; guarded-pool
;;;;;
(defclass guarded-pool (pool)
  ((guard
    :documentation ":type function"
    :accessor guard
    :initform #'(lambda (thunk) (funcall thunk))))
  (:documentation "A Prevalence system with a guard thunk"))
