;;;;;
;;;;; Defince Generic function
;;;;;
;;;;; Contents
;;;;;  1. Managed pool
;;;;;  2. Pool
;;;;;  3. blob
;;;;;

(in-package :upanishad)

;;;
;;; 1. Managed pool
;;;
(defgeneric get-preference (pool key)
  (:documentation "Retrieve the value of the persistent preference stored under key in pool"))

(defgeneric get-object-at-%id (pool class %id)
  (:documentation "Find and return the object in pool of class with %id, null if not found"))

(defgeneric find-object (pool class &key slot value test)
  (:documentation "執筆中"))

(defgeneric tx-create-index-for-objects-slot (pool class slot &optional test)
  (:documentation "Create an index for this object on this slot, with an optional test for the hash table (add existing objects)"))

(defgeneric tx-remove-objects-slot-index (pool class slot)
  (:documentation "Remove an index for this object on this slot"))

(defgeneric add-object-to-index (pool class slot object)
  (:documentation "インデックスにオブジェクトを登録する。"))

(defgeneric remove-object-from-index (pool class slot object)
  (:documentation "インデックスからオブジェクトを削除する。"))

(defgeneric add-index (pool class &optional slots test)
  (:documentation "Create indexes on each of the slots provided."))

(defgeneric drop-index (pool class &optional slots)
  (:documentation "Drop indexes on each of the slots provided"))

(defgeneric tx-create-object (pool class &optional slots-and-values)
  (:documentation "Create a new object of class in pool, assigning it a unique %id, optionally setting some slots and values"))

(defgeneric tx-delete-object (pool class %id)
  (:documentation "Delete the object of class with %id from the pool"))

(defgeneric tx-change-object-slots (pool class %id slots-and-values)
  (:documentation "Change some slots of the object of class with %id in pool using slots and values"))

(defgeneric tx-create-%id-counter (pool)
  (:documentation "Initialize the %id counter to 0"))

(defgeneric next-%id (pool)
  (:documentation "Increment and return the next %id"))

(defgeneric all-preferences-keys (pool)
  (:documentation "Return a list of all persistent preference keys of pool"))

(defgeneric tx-remove-object-on-index (pool atman slot-symbol)
  (:documentation "インデックスからオブジェクトを取り除きます。"))

(defgeneric class-%id-list (pool)
  (:documentation "poolに登録されているクラスの一覧(list)を返します。"))

(defgeneric root-list (pool)
  (:documentation "poolに登録されているルートオブジェクトの一覧(list)を返します。"))

(defgeneric get-object-list (pool symbol)
  (:documentation "poolで管理されている symbolクラスのオブジェクトの一覧(list)を返します。"))
(defgeneric print-root-list (pool &key stream)
  (:documentation ""))


;;;
;;; 2. Pool
;;;
(defgeneric poolp (pool)
  (:documentation ""))

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

(defgeneric stop (pool &key abort)
  (:documentation "Stop a pool"))

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

(defgeneric make-snapshot-filename (pool type &optional suffix)
  (:documentation "執筆中"))

(defgeneric make-snapshot-pathname (pool directory type &optional suffix)
  (:documentation "執筆中"))

(defgeneric make-transaction-log-filename (pool &optional suffix)
  (:documentation "執筆中"))

(defgeneric make-transaction-log-pathname (pool directory &optional suffix)
  (:documentation "執筆中"))

(defgeneric snapshot-pathnames (pool type)
  (:documentation "執筆中"))

(defgeneric (setf snapshot-pathnames) (value pool type)
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
