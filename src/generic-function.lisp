;;;;;
;;;;; Defince Generic function
;;;;;
;;;;; Contents
;;;;;  1. Managed prevalence
;;;;;  2. Prevalence
;;;;;  3. blob
;;;;;

(in-package :upanishad)

;;;
;;; 1. Managed prevalence
;;;
(defgeneric get-preference (pool key)
  (:documentation "Retrieve the value of the persistent preference stored under key in pool"))

(defgeneric find-all-objects (pool class)
  (:documentation "Return an unordered collection of all objects in pool that are instances of class"))

;; TODO: この関数は廃止予定です。 下の get-object-with-id を利用するようにしてください。
(defgeneric find-object-with-id (pool class id)
  (:documentation "Find and return the object in pool of class with id, null if not found"))

(defgeneric get-object-with-id (pool class id)
  (:documentation "Find and return the object in pool of class with id, null if not found"))

(defgeneric find-object-with-slot-use-index (pool class index)
  (:documentation "執筆中"))

(defgeneric find-object-with-slot-full-scan (pool class slot value test)
  (:documentation "執筆中"))

(defgeneric find-object-with-slot (pool class slot value &optional test)
  (:documentation "Find and return the object in pool of class with slot equal to value, null if not found"))

(defgeneric next-id (pool)
  (:documentation "執筆中"))

(defgeneric all-preferences-keys (pool)
  (:documentation "Return a list of all persistent preference keys of pool"))

(defgeneric tx-remove-object-on-slot-index (pool atman slot-symbol)
  (:documentation "スロット・インデックスからオブジェクトを取り除きます。"))



;;;
;;; 2. Prevalence
;;;
(defgeneric execute (pool object)
  (:documentation "Ask for a transaction object to be executed on pool with ACID properties"))

(defgeneric execute-on (object pool)
  (:documentation "Ask for a transaction object to execute its changes in the context of pool"))

(defgeneric query (pool function &rest args)
  (:documentation "Ask for a query function to be executed on pool with args"))

(defgeneric snapshot (pool)
  (:documentation "Take a snapshot of a pool"))

(defgeneric restore (pool)
  (:documentation "Restore a pool from permanent storage"))

(defgeneric remove-root-object (pool name)
  (:documentation "Remove the root object by symbol name from pool"))

(defgeneric initiates-rollback (condition)
  (:documentation "Return true when a condition initiates a rollback when thrown from a transaction"))

(defgeneric backup (pool &key directory)
  (:documentation "Make backup copies of the current snapshot and transaction-log files"))

(defgeneric totally-destroy (pool &key abort)
  (:documentation "Totally destroy pool from permanent storage by deleting any files that we find"))

(defgeneric get-root-object (pool name)
  (:documentation "Retrieve a root object by symbol name from pool"))

(defgeneric (setf get-root-object) (value pool name)
  (:documentation "Set a symbol named root object of pool to value"))

(defgeneric get-option (pool name)
  (:documentation "Retrieve a named option from pool"))

(defgeneric (setf get-option) (value pool name)
  (:documentation "Set a named option of pool to value"))

(defgeneric close-open-streams (pool &key abort)
  (:documentation "執筆中"))

(defgeneric log-transaction (pool transaction)
  (:documentation "執筆中"))

(defgeneric get-transaction-log-filename (pool &optional suffix)
  (:documentation "執筆中"))

(defgeneric get-snapshot-filename (pool &optional suffix)
  (:documentation "執筆中"))



;;;
;;; 3. blob
;;;
(defgeneric get-file (blob)
  (:documentation "Return the pathname to the bytes of blob"))

(defgeneric get-size (blob)
  (:documentation "Return the pathname to the bytes of blob"))

(defgeneric fill-from-stream (blob binary-input-stream)
  (:documentation "Fill the blob's contents with the bytes from binary-input-stream"))

(defgeneric copy-to-stream (blob binary-output-stream)
  (:documentation "Copy the bytes from blob to binary-output-stream"))

(defgeneric fill-from-file (blob pathname)
  (:documentation "Fill the blob's contents with the bytes read from the binary file at pathname"))

(defgeneric destroy (blob)
  (:documentation "Completely destroy blob (removing its byte data file as well)"))

(defgeneric size-from-file (blob)
  (:documentation "執筆中"))

(defgeneric set-size-from-file (blob)
  (:documentation "執筆中"))

