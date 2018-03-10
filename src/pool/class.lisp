(in-package :upanishad.pool)

(defclass pool-core ()
  ((memes
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
   ;; 以下、廃棄予定スロット
   (root-objects
    :documentation ""
    :accessor root-objects
    :initform (make-hash-table :test 'eq)
    :type 'hash-table)
   (index-objects
    :documentation ":type hash-table"
    :accessor index-objects
    :initform (make-hash-table :test 'eq))))

(defclass pool-transaction ()
  ((transaction-hook
    :documentation ":type function"
    :accessor transaction-hook
    :initarg :transaction-hook
    :initform #'identity)
   (snapshot
    :documentation ":type pathname"
    :initform (make-hash-table :test 'eq))
   (transaction-log
    :documentation ":type pathname"
    :accessor transaction-log)
   (transaction-log-stream
    :documentation ":type stream"
    :accessor transaction-log-stream
    :initform nil)))

(defclass pool-persistence ()
  ((serializer
    :documentation ":type function"
    :accessor serializer
    :initarg :serializer
    :initform #'serialize-xml)
   (deserializer
    :documentation ":type function"
    :accessor deserializer
    :initarg :deserializer
    :initform #'deserialize-xml)
   (serialization-state
    :documentation ":type serialization-state"
    :reader serialization-state
    :initform (make-serialization-state))))

(defclass pool (brahman pool-core pool-transaction pool-persistence)
  ;; これらは分割しても良さそう。トランザクション用/永続化用で
  ((directory
    :documentation ":type pathname"
    :accessor get-directory
    :initarg :directory
    :type 'pathname)
   (file-extension
    :documentation ""
    :accessor file-extension
    :initarg :file-extension
    :initform "xml"
    :type 'string))
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
