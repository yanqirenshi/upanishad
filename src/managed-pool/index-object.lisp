(in-package :upanishad)

;;;;; Image of INDEX
;;;;;
;;;;; ROOT OBJECTS
;;;;; +-----------------+-------------------------------------------+
;;;;; | key: index-name | value: index                              |+
;;;;; |      (symbol)   |        (hash-table)                       ||+
;;;;; |                 | INDEX                                     |||
;;;;; |                 | +-----------------+-------------------+   |||
;;;;; |                 | | key: slot-value | value: %id-map    |+  |||
;;;;; |                 | |                 |                   ||+ |||
;;;;; |                 | |                 | +----+--------+   ||| |||
;;;;; |                 | |                 | | id | object |+  ||| |||
;;;;; |                 | |                 | +----+--------+|+ ||| |||
;;;;; |                 | |                 |  +----+--------+| ||| |||
;;;;; |                 | |                 |   +----+--------+ ||| |||
;;;;; |                 | +-----------------+-------------------+|| |||
;;;;; |                 |  +-----------------+-------------------+| |||
;;;;; |                 |   +-----------------+-------------------+ |||
;;;;; +-----------------+-------------------------------------------+||
;;;;;  +-----------------+-------------------------------------------+|
;;;;;   +-----------------+-------------------------------------------+
;;;;;

(defvar *index-name-regex* "^(\\S)+-%ID-INDEX$")
(defvar *index-name-format* "~a-~a-INDEX")

(defun class-%id-indexp (symbol)
  (cl-ppcre:scan "^(\\S)+-%ID-INDEX$"
                 (symbol-name symbol)))

(defun get-objects-slot-index-name (class &optional (slot '%id))
  "Return the keyword symbol naming the specified index of instances of class."
  (let ((classname (if (symbolp class) (string class) (class-name class)))
        (slotname  (symbol-name slot)))
    (intern (concatenate 'string classname "-" slotname "-INDEX") :keyword)))

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

(defun %add-object-to-slot-index (index object slot)
  (let ((%id-map (gethash (slot-value object slot) index))
        (%id     (%id object)))
    ;; 最初の時ね。
    (when (null %id-map)
      (setf %id-map   (make-hash-table))
      (setf (gethash (slot-value object slot) index) %id-map))
    ;; 既に存在するかチェックする。
    ;; 存在する場合は何もしない。
    (unless (gethash %id %id-map)
      ;; 存在しない場合は追加する。
      (setf (gethash %id %id-map) (%id object)))))

(defmethod add-object-to-slot-index ((pool pool) class slot object)
  (let* ((index-name (get-objects-slot-index-name class slot))
         (index (get-root-object pool index-name)))
    (when (and index  (slot-boundp object slot))
      ;; 登録は実質こちらでやってます。
      (%add-object-to-slot-index index object slot))))

(defun %remove-object-from-slot-index (index object slot)
  (let ((%id-map (gethash (slot-value object slot) index))
        (%id     (%id object)))
    ;; 既に存在するかチェックする。
    ;; 存在する場合は %id-map から削除する。
    (when %id-map ;;TODO: このケースって何じゃったっけ？
      (when (gethash %id %id-map)
        (remhash %id %id-map))
      ;; %id-map が空になったら、index から削除する。
      ;; TODO: これ、削除する必要あるの？
      (when (= (hash-table-size %id-map) 0)
        (remhash (slot-value object slot) index)))))

(defmethod remove-object-from-slot-index ((pool pool) class slot object)
  (let* ((index-name (get-objects-slot-index-name class slot))
         (index (get-root-object pool index-name)))
    (when (and index (slot-boundp object slot))
      ;; 削除は実質こちらでやってます。
      (%remove-object-from-slot-index index object slot))))

(defmethod index-on ((pool pool) class &optional slots (test 'equalp))
  (dolist (slot slots)
    (execute-transaction (tx-create-objects-slot-index pool class slot test))))

(defmethod drop-index-on ((pool pool) class &optional slots)
  (dolist (slot slots)
    (execute-transaction (tx-remove-objects-slot-index pool class slot))))

(defmethod tx-remove-object-on-slot-index ((pool pool)
                                           (obj  meme)
                                           (slot-symbol symbol))
  (let* ((obj-class (class-name (class-of obj)))
         (index-name (get-objects-slot-index-name obj-class
                                                  slot-symbol))
         (index (get-root-object pool index-name)))
    (when (and index (gethash (slot-value obj slot-symbol) index))
      (remhash (%id  obj)
               (gethash (slot-value obj slot-symbol) index)))))
