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
  (not (null (cl-ppcre:scan *index-name-regex* (symbol-name symbol)))))

(defun classname-at (value)
  (if (symbolp value)
      (string value)
      (class-name value)))

(defun get-objects-slot-index-name (class &optional (slot '%id))
  "Return the keyword symbol naming the specified index of instances of class."
  (let ((classname (classname-at class))
        (slotname  (symbol-name slot)))
    (intern (format nil *index-name-format* classname slotname) :keyword)))

(defun index-at (pool &key name)
  (when pool
    (cond (name (get-index-object pool name))
          (t (error "Bad parameter")))))

(defun (setf index-at) (index pool &key name)
  (when pool
    (setf (get-index-object pool name) index)))

(defun slot-index-at (pool slot &key class object)
  (cond ((and class slot)
         (let ((index-name (get-objects-slot-index-name class slot)))
           (index-at pool :name index-name)))
        ((and object slot)
         (slot-index-at pool slot :class (class-name (class-of object))))
        (t "Bad parameter")))

(defun %add-object-to-slot-index (index object slot)
  (let ((%id-map (gethash (slot-value object slot) index))
        (%id     (%id object)))
    (when (null %id-map)
      (setf %id-map  (make-hash-table))
      (setf (gethash (slot-value object slot) index) %id-map))
    (unless (gethash %id %id-map)
      (setf (gethash %id %id-map) (%id object)))))

(defmethod add-object-to-slot-index ((pool pool) class slot object)
  (let ((index (slot-index-at pool slot :class class)))
    (when (and index (slot-boundp object slot))
      (%add-object-to-slot-index index object slot))))

(defun make-index (&key (test #'equalp))
  (make-hash-table :test test))

(defmethod tx-create-objects-slot-index ((pool pool) class slot &optional (test #'equalp))
  (let ((index-name (get-objects-slot-index-name class slot)))
    (unless (index-at pool :name index-name)
      (let ((index (make-index :test test)))
        (setf (index-at pool :name index-name) index)
        (dolist (object (find-all-objects pool class))
          (add-object-to-slot-index pool class slot object))))))

(defmethod index-on ((pool pool) class &optional slots (test 'equalp))
  (dolist (slot slots)
    (execute-transaction
     (tx-create-objects-slot-index pool class slot test))))

(defun %remove-object-from-slot-index (index object slot)
  (let ((%id-map (gethash (slot-value object slot) index))
        (%id     (%id object)))
    (when %id-map
      (when (gethash %id %id-map)
        (remhash %id %id-map))
      (when (= (hash-table-size %id-map) 0)
        (remhash (slot-value object slot) index)))))

(defmethod remove-object-from-slot-index ((pool pool) class slot object)
  (let ((index (slot-index-at pool slot :class class)))
    (when (and index (slot-boundp object slot))
      (%remove-object-from-slot-index index object slot))))

(defmethod tx-remove-object-on-slot-index ((pool pool)
                                           (obj  meme)
                                           (slot-symbol symbol))
  (let* ((index (slot-index-at pool slot-symbol :object obj))
         (%id-map (gethash (slot-value obj slot-symbol) index)))
    (when (and index %id-map)
      (remhash (%id  obj)
               (gethash (slot-value obj slot-symbol) index)))))

(defmethod tx-remove-objects-slot-index ((pool pool) class slot)
  (let ((index-name (get-objects-slot-index-name class slot)))
    (when (get-index-object pool index-name)
      (remove-root-object pool index-name))))

(defmethod drop-index-on ((pool pool) class &optional slots)
  (dolist (slot slots)
    (execute-transaction
     (tx-remove-objects-slot-index pool class slot))))
