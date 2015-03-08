;;;;;
;;;;; Contents
;;;;;  1. A convience macro
;;;;;  2. We use a simple id counter to generate unique object identifiers
;;;;;  3. A generic persistent preferences mechanism
;;;;;  4. added iwasaki
;;;;;

(in-package :upanishad)

;;;
;;; 1. A convience macro
;;;
(defmacro execute-transaction (transaction-call)
  "Create a transaction object from transaction-call and execute it"
  `(if (not (poolp ,(second transaction-call)))
       (error "第一引数が pool ではありません。第一引数=~a" ,(second transaction-call))
       (execute ,(second transaction-call)
                (make-transaction ',(first transaction-call) ,@(rest (rest transaction-call))))))


(defmethod get-preference ((pool pool) key)
  "Retrieve the value of the persistent preference stored under key in pool"
  (let ((preferences (get-root-object pool :preferences)))
    (when preferences
      (gethash key preferences))))


(defun get-objects-root-name (class)
  "Return the keyword symbol naming the root of instances of class"
  (let ((classname (if (symbolp class) (string class) (class-name class))))
    (intern (concatenate 'string classname "-ROOT") :keyword)))


(defun get-objects-slot-index-name (class &optional (slot 'id))
  "Return the keyword symbol naming the specified index of instances of class."
  (let ((classname (if (symbolp class) (string class) (class-name class)))
        (slotname  (symbol-name slot)))
    (intern (concatenate 'string classname "-" slotname "-INDEX") :keyword)))


(defmethod find-all-objects ((pool pool) class)
  "Return an unordered collection of all objects in pool that are instances of class"
  (let ((root-name (get-objects-root-name class)))
    (get-root-object pool root-name)))


;; TODO: この関数は廃止予定です。 下の get-object-with-id を利用するようにしてください。
(defmethod find-object-with-id ((pool pool) class id)
  "Find and return the object in pool of class with id, null if not found"
  (let* ((index-name (get-objects-slot-index-name class 'id))
         (index (get-root-object pool index-name)))
    (when index
      (gethash id index))))


(defmethod get-object-with-id ((pool pool) class id)
  "Find and return the object in pool of class with id, null if not found"
  (let* ((index-name (get-objects-slot-index-name class 'id))
         (index (get-root-object pool index-name)))
    (when index
      (gethash id index))))


(defmethod find-object-with-slot-use-index ((pool pool) class index)
  (when index
    (let* ((ids (alexandria:hash-table-values  index))
           (len (length ids)))
      (cond ((= len 0) nil)
            ((= len 1) (list (get-object-with-id pool class (first ids))))
            (t (mapcar #'(lambda (id)
                           (get-object-with-id pool class id))
                       ids))))))


(defmethod find-object-with-slot-full-scan ((pool pool) class slot value test)
  "オブジェクトを全件検索します。"
  (remove-if #'(lambda (object)
                 (not (funcall test value (slot-value object slot))))
             (find-all-objects pool class)))


(defmethod find-object-with-slot ((pool pool) class slot value &optional (test #'equalp))
  "Find and return the object in pool of class with slot equal to value, null if not found
オブジェクトのスロットの値を検索して、ヒツトしたものを返します。
対象のスロットにインデックスが貼られている場合はインデックスを利用して検索します。
インデックスが存在しない場合は 全件検索します。

返す値はリスト形式で返します。なもんで、存在しない場合は nil を返します。
"
  (let* ((index-name (get-objects-slot-index-name class slot))
         (index      (get-root-object pool index-name)))
    (if index
        ;; index が存在した場合は index で検索する。
        (find-object-with-slot-use-index pool class (gethash value index))
        ;; index が存在しない場合は全部検索します。
        (find-object-with-slot-full-scan pool class slot value test))))


(defmethod tx-create-objects-slot-index ((pool pool) class slot &optional (test #'equalp))
  (let ((index-name (get-objects-slot-index-name class slot)))
    (unless (get-root-object pool index-name)
      (let ((index (make-hash-table :test test)))
        (setf (get-root-object pool index-name) index)
        (dolist (object (find-all-objects pool class))
          (add-object-to-slot-index pool class slot object))))))


(defmethod tx-remove-objects-slot-index ((pool pool) class slot)
  (let ((index-name (get-objects-slot-index-name class slot)))
    (when (get-root-object pool index-name)
      (remove-root-object pool index-name))))


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


(defmethod add-object-to-slot-index ((pool pool) class slot object)
  (let* ((index-name (get-objects-slot-index-name class slot))
         (index (get-root-object pool index-name)))
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


(defmethod remove-object-from-slot-index ((pool pool) class slot object)
  (let* ((index-name (get-objects-slot-index-name class slot))
         (index (get-root-object pool index-name)))
    (when (and index (slot-boundp object slot))
      ;; 削除は実質こちらでやってます。
      (slot-index-xxx-remove index object slot))))


(defmethod index-on ((pool pool) class &optional slots (test 'equalp))
  (dolist (slot slots)
    (execute-transaction (tx-create-objects-slot-index pool class slot test))))


(defmethod drop-index-on ((pool pool) class &optional slots)
  (dolist (slot slots)
    (execute-transaction (tx-remove-objects-slot-index pool class slot))))


(defun slot-value-changed-p (object slot value)
  "Return true when slot in object is not eql to value (or when the slot was unbound)"
  (or (not (slot-boundp object slot))
      (not (eql (slot-value object slot) value))))


(defmethod tx-create-object ((pool pool) class &optional slots-and-values)
  (let* ((id (next-id pool))
         (object (make-instance class :id id))
         (index-name (get-objects-slot-index-name class 'id))
         (index (or (get-root-object pool index-name)
                    (setf (get-root-object pool index-name) (make-hash-table)))))
    (push object (get-root-object pool (get-objects-root-name class)))
    (setf (gethash id index) object)
    (tx-change-object-slots pool class id slots-and-values)
    object))


(defmethod tx-delete-object ((pool pool) class id)
  (let ((object (get-object-with-id pool class id)))
    (if object
        (let ((root-name (get-objects-root-name class))
              (index-name (get-objects-slot-index-name class 'id)))
          (setf (get-root-object pool root-name) (delete object (get-root-object pool root-name)))
          (remhash id (get-root-object pool index-name)))
        (error "no object of class ~a with id ~d found in ~s" class id pool))))


(defmethod tx-change-object-slots ((pool pool) class id slots-and-values)
  (let ((object (get-object-with-id pool class id)))
    (unless object (error "no object of class ~a with id ~d found in ~s" class id pool))
    (loop :for (slot value) :in slots-and-values
       :do (when (slot-value-changed-p object slot value)
             (remove-object-from-slot-index pool class slot object)
             (setf (slot-value object slot) value)
             (add-object-to-slot-index pool class slot object)))))



;;;
;;; 2. We use a simple id counter to generate unique object identifiers
;;;
(defmethod tx-create-id-counter ((pool pool))
  (setf (get-root-object pool :id-counter) 0))


(defmethod next-id ((pool pool))
  (incf (get-root-object pool :id-counter)))


;;;
;;; 3. A generic persistent preferences mechanism
;;;
(defmethod tx-set-preference ((pool pool) key value)
  "Set the value of the persistent preference key in pool"
  (let ((preferences (get-root-object pool :preferences)))
    (when (not preferences)
      (setf preferences (make-hash-table)
            (get-root-object pool :preferences) preferences))
    (setf (gethash key preferences) value)))


(defmethod all-preferences-keys ((pool pool))
  "Return a list of all persistent preference keys of pool"
  (let ((preferences (get-root-object pool :preferences)))
    (when preferences
      (let (keys)
        (maphash #'(lambda (key value)
                     (declare (ignore value))
                     (push key keys))
                 preferences)
        keys))))



;;;
;;; 4. added iwasaki
;;;
(defmethod tx-remove-object-on-slot-index ((pool pool)
                                           (obj  meme)
                                           (slot-symbol symbol))
  (let* ((obj-class (class-name (class-of obj)))
         (index-name (get-objects-slot-index-name obj-class
                                                  slot-symbol))
         (index (get-root-object pool index-name)))
    (when (and index (gethash (slot-value obj slot-symbol) index))
      (remhash (get-id  obj)
               (gethash (slot-value obj slot-symbol) index)))))


(defmethod get-at-id ((pool banshou) id)
  "もっと効率良いやりかたがありそうじゃけど。。。"
  (car
   (remove nil
           (mapcar #'(lambda (index)
                       (gethash id
                                (get-root-object pool index)))
                   (class-id-list pool)))))




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
