;;;;;
;;;;; Contents
;;;;;  1. A convience macro
;;;;;  2. We use a simple id counter to generate unique object identifiers
;;;;;  3. added iwasaki
;;;;;

(in-package :upanishad)

;;;
;;; 1. A convience macro
;;;
(defmacro execute-transaction (transaction-call)
  "Create a transaction object from transaction-call and execute it"
  `(execute ,(second transaction-call)
            (make-transaction ',(first transaction-call) ,@(rest (rest transaction-call)))))


(defgeneric get-preference (pool key)
  (:documentation "Retrieve the value of the persistent preference stored under key in pool")
  (:method ((pool pool) key)
    "Retrieve the value of the persistent preference stored under key in pool"
    (let ((preferences (get-root-object pool :preferences)))
      (when preferences
        (gethash key preferences)))))


(defun get-objects-root-name (class)
  "Return the keyword symbol naming the root of instances of class"
  (let ((classname (if (symbolp class) (string class) (class-name class))))
    (intern (concatenate 'string classname "-ROOT") :keyword)))


(defun get-objects-slot-index-name (class &optional (slot 'id))
  "Return the keyword symbol naming the specified index of instances of class."
  (let ((classname (if (symbolp class) (string class) (class-name class)))
        (slotname  (symbol-name slot)))
    (intern (concatenate 'string classname "-" slotname "-INDEX") :keyword)))


(defgeneric find-all-objects (system class)
  (:documentation "Return an unordered collection of all objects in system that are instances of class"))


(defmethod find-all-objects ((system pool) class)
  "Return an unordered collection of all objects in system that are instances of class"
  (let ((root-name (get-objects-root-name class)))
    (get-root-object system root-name)))


;; TODO: この関数は廃止予定です。 下の get-object-with-id を利用するようにしてください。
(defgeneric find-object-with-id (system class id)
  (:documentation "Find and return the object in system of class with id, null if not found"))
(defmethod find-object-with-id ((system pool) class id)
  "Find and return the object in system of class with id, null if not found"
  (let* ((index-name (get-objects-slot-index-name class 'id))
         (index (get-root-object system index-name)))
    (when index
      (gethash id index))))


(defgeneric get-object-with-id (system class id)
  (:documentation "Find and return the object in system of class with id, null if not found"))
(defmethod get-object-with-id ((system pool) class id)
  "Find and return the object in system of class with id, null if not found"
  (let* ((index-name (get-objects-slot-index-name class 'id))
         (index (get-root-object system index-name)))
    (when index
      (gethash id index))))


(defgeneric find-object-with-slot-use-index (system class index))
(defmethod find-object-with-slot-use-index ((system pool) class index)
  (when index
    (let* ((ids (alexandria:hash-table-values  index))
           (len (length ids)))
      (cond ((= len 0) nil)
            ((= len 1) (list (get-object-with-id system class (first ids))))
            (t (mapcar #'(lambda (id)
                           (get-object-with-id system class id))
                       ids))))))


(defgeneric find-object-with-slot-full-scan (system class slot value test))
(defmethod find-object-with-slot-full-scan ((system pool) class slot value test)
  "オブジェクトを全件検索します。"
  (remove-if #'(lambda (object)
                 (not (funcall test value (slot-value object slot))))
             (find-all-objects system class)))


(defgeneric find-object-with-slot (system class slot value &optional test)
  (:documentation "Find and return the object in system of class with slot equal to value, null if not found"))
(defmethod find-object-with-slot ((system pool) class slot value &optional (test #'equalp))
  "Find and return the object in system of class with slot equal to value, null if not found
オブジェクトのスロットの値を検索して、ヒツトしたものを返します。
対象のスロットにインデックスが貼られている場合はインデックスを利用して検索します。
インデックスが存在しない場合は 全件検索します。

返す値はリスト形式で返します。なもんで、存在しない場合は nil を返します。
"
  (let* ((index-name (get-objects-slot-index-name class slot))
         (index      (get-root-object system index-name)))
    (if index
        ;; index が存在した場合は index で検索する。
        (find-object-with-slot-use-index system class (gethash value index))
        ;; index が存在しない場合は全部検索します。
        (find-object-with-slot-full-scan system class slot value test))))


(defun tx-create-objects-slot-index (system class slot &optional (test #'equalp))
  "Create an index for this object on this slot, with an optional test for the hash table (add existing objects)"
  (let ((index-name (get-objects-slot-index-name class slot)))
    (unless (get-root-object system index-name)
      (let ((index (make-hash-table :test test)))
        (setf (get-root-object system index-name) index)
        (dolist (object (find-all-objects system class))
          (add-object-to-slot-index system class slot object))))))


(defun tx-remove-objects-slot-index (system class slot)
  "Remove an index for this object on this slot"
  (let ((index-name (get-objects-slot-index-name class slot)))
    (when (get-root-object system index-name)
      (remove-root-object system index-name))))


(defun slot-index-xxx-add (index object slot)
  (let ((id-map (gethash (slot-value object slot) index))
        (id     (get-id object)))
    ;; 最初の時ね。
    (when (null id-map)
      (setf id-map   (make-hash-table))
      (setf (gethash (slot-value object slot) index) id-map))
    ;; 既に存在するかチェックする。
    ;; 存在する場合は何もしない。
    (unless (gethash id id-map)
      ;; 存在しない場合は追加する。
      (setf (gethash id id-map) (get-id object)))))


(defun add-object-to-slot-index (system class slot object)
  "スロット・インデックスにオブジェクトを登録します。"
  (let* ((index-name (get-objects-slot-index-name class slot))
         (index (get-root-object system index-name)))
    (when (and index  (slot-boundp object slot))
      ;; 登録は実質こちらでやってます。
      (slot-index-xxx-add index object slot))))


(defun slot-index-xxx-remove (index object slot)
  (let ((id-map (gethash (slot-value object slot) index))
        (id     (get-id object)))
    ;; 既に存在するかチェックする。
    ;; 存在する場合は id-map から削除する。
    (when id-map ;;TODO: このケースって何じゃったっけ？
      (when (gethash id id-map)
        (remhash id id-map))
      ;; id-map が空になったら、index から削除する。
      ;; TODO: これ、削除する必要あるの？
      (when (= (hash-table-size id-map) 0)
        (remhash (slot-value object slot) index)))))


(defun remove-object-from-slot-index (system class slot object)
  "スロット・インデックスからオブジェクトを削除します。"
  (let* ((index-name (get-objects-slot-index-name class slot))
         (index (get-root-object system index-name)))
    (when (and index (slot-boundp object slot))
      ;; 削除は実質こちらでやってます。
      (slot-index-xxx-remove index object slot))))


(defun index-on (system class &optional slots (test 'equalp))
  "Create indexes on each of the slots provided."
  (dolist (slot slots)
    (execute-transaction (tx-create-objects-slot-index system class slot test))))


(defun drop-index-on (system class &optional slots)
  "Drop indexes on each of the slots provided"
  (dolist (slot slots)
    (execute-transaction (tx-remove-objects-slot-index system class slot))))


(defun slot-value-changed-p (object slot value)
  "Return true when slot in object is not eql to value (or when the slot was unbound)"
  (or (not (slot-boundp object slot))
      (not (eql (slot-value object slot) value))))


(defun tx-create-object (system class &optional slots-and-values)
  "Create a new object of class in system, assigning it a unique id, optionally setting some slots and values"
  (let* ((id (next-id system))
         (object (make-instance class :id id))
         (index-name (get-objects-slot-index-name class 'id))
         (index (or (get-root-object system index-name)
                    (setf (get-root-object system index-name) (make-hash-table)))))
    (push object (get-root-object system (get-objects-root-name class)))
    (setf (gethash id index) object)
    (tx-change-object-slots system class id slots-and-values)
    object))


(defun tx-delete-object (system class id)
  "Delete the object of class with id from the system"
  (let ((object (get-object-with-id system class id)))
    (if object
        (let ((root-name (get-objects-root-name class))
              (index-name (get-objects-slot-index-name class 'id)))
          (setf (get-root-object system root-name) (delete object (get-root-object system root-name)))
          (remhash id (get-root-object system index-name)))
        (error "no object of class ~a with id ~d found in ~s" class id system))))


(defun tx-change-object-slots (system class id slots-and-values)
  "Change some slots of the object of class with id in system using slots and values"
  (let ((object (get-object-with-id system class id)))
    (unless object (error "no object of class ~a with id ~d found in ~s" class id system))
    (loop :for (slot value) :in slots-and-values
       :do (when (slot-value-changed-p object slot value)
             (remove-object-from-slot-index system class slot object)
             (setf (slot-value object slot) value)
             (add-object-to-slot-index system class slot object)))))



;;;
;;; 2. We use a simple id counter to generate unique object identifiers
;;;
(defun tx-create-id-counter (system)
  "Initialize the id counter to 0"
  (setf (get-root-object system :id-counter) 0))


(defgeneric next-id (pool)
  (:method ((system pool))
    "Increment and return the next id"
    (incf (get-root-object system :id-counter))))


;;; A generic persistent preferences mechanism
(defun tx-set-preference (system key value)
  "Set the value of the persistent preference key in system"
  (let ((preferences (get-root-object system :preferences)))
    (when (not preferences)
      (setf preferences (make-hash-table)
            (get-root-object system :preferences) preferences))
    (setf (gethash key preferences) value)))


(defgeneric all-preferences-keys (system)
  (:documentation "Return a list of all persistent preference keys of system"))


(defmethod all-preferences-keys ((system pool))
  "Return a list of all persistent preference keys of system"
  (let ((preferences (get-root-object system :preferences)))
    (when preferences
      (let (keys)
        (maphash #'(lambda (key value)
                     (declare (ignore value))
                     (push key keys))
                 preferences)
        keys))))



;;;
;;; 3. added iwasaki
;;;
(defgeneric tx-remove-object-on-slot-index (pool atman slot-symbol)
  (:documentation "スロット・インデックスからオブジェクトを取り除きます。"))
(defmethod tx-remove-object-on-slot-index ((pool pool)
                                           (obj  atman)
                                           (slot-symbol symbol))
  (let* ((obj-class (class-name (class-of obj)))
         (index-name (get-objects-slot-index-name obj-class
                                                  slot-symbol))
         (index (get-root-object pool index-name)))
    (when (and index (gethash (slot-value obj slot-symbol) index))
      (remhash (get-id  obj)
               (gethash (slot-value obj slot-symbol) index)))))




#|
-*- mode: lisp -*-

$Id$

The code in this file adds another layer above plain object prevalence.
We manage objects with ids in an organized fashion, adding an id counter and preferences.

Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.

You are granted the rights to distribute and use this software
as governed by the terms of the Lisp Lesser General Public License
(http://opensource.franz.com/preamble.html), also known as the LLGPL.
|#
