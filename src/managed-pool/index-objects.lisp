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

(defun get-index-name (class &optional (slot '%id))
  "Return the keyword symbol naming the specified index of instances of class."
  (let ((classname (classname-at class))
        (slotname  (symbol-name slot)))
    (intern (format nil *index-name-format* classname slotname) :keyword)))

(defun make-index (&key (test #'equalp))
  (make-hash-table :test test))

(defun make-%id-map ()
  (make-hash-table))

(defun %index-at (pool name class slot)
  (cond ((and class slot)
         (get-index-object pool (get-index-name class slot)))
        (name (get-index-object pool name))
        (t (error "Bad parameter"))))

(defun index-at (pool &key name class slot (ensure nil))
  (assert pool)
  (let ((index (%index-at pool name class slot)))
    (or index
        (when ensure
          (setf (index-at pool :name name :class class :slot slot)
                (make-hash-table))))))

(defun (setf index-at) (index pool &key name class slot)
  (assert pool)
  (cond ((and class slot)
         (setf (index-at pool :name (get-index-name class slot))
               index))
        (name
         (setf (get-index-object pool name) index))
        (t (error "Bad parameter"))))

(defun slot-index-at (pool slot &key class object)
  (cond ((and class slot)
         (let ((index-name (get-index-name class slot)))
           (index-at pool :name index-name)))
        ((and object slot)
         (slot-index-at pool slot :class (class-name (class-of object))))
        (t (error "Bad parameter"))))

(defun %add-object-to-index (index object slot)
  (let ((%id-map (gethash (slot-value object slot) index))
        (%id     (%id object)))
    (when (null %id-map)
      (setf %id-map  (make-hash-table))
      (setf (gethash (slot-value object slot) index) %id-map))
    (unless (gethash %id %id-map)
      (setf (gethash %id %id-map) (%id object)))))

(defmethod add-object-to-index ((pool pool) class slot object)
  (let ((index (slot-index-at pool slot :class class)))
    (when (and index (slot-boundp object slot))
      (%add-object-to-index index object slot))))

(defun %remove-object-from-index (index object slot)
  (let ((%id-map (gethash (slot-value object slot) index))
        (%id     (%id object)))
    (when %id-map
      (when (gethash %id %id-map)
        (remhash %id %id-map))
      (when (= (hash-table-size %id-map) 0)
        (remhash (slot-value object slot) index)))))

(defmethod remove-object-from-index ((pool pool) class slot object)
  (let ((index (slot-index-at pool slot :class class)))
    (when (and index (slot-boundp object slot))
      (%remove-object-from-index index object slot))))

(defmethod tx-remove-object-on-slot-index ((pool pool)
                                           (obj  meme)
                                           (slot-symbol symbol))
  (let* ((index (slot-index-at pool slot-symbol :object obj))
         (%id-map (gethash (slot-value obj slot-symbol) index)))
    (when (and index %id-map)
      (remhash (%id  obj)
               (gethash (slot-value obj slot-symbol) index)))))

(defmethod tx-create-index-for-objects-slot ((pool pool) class slot &optional (test #'equalp))
  (let ((index-name (get-index-name class slot)))
    (unless (index-at pool :name index-name)
      (let ((index (make-index :test test)))
        (setf (index-at pool :name index-name) index)
        (dolist (object (find-all-objects pool class))
          (add-object-to-index pool class slot object))))))

(defmethod index-on ((pool pool) class &optional slots (test 'equalp))
  (dolist (slot slots)
    (execute-transaction
     (tx-create-index-for-objects-slot pool class slot test))))

(defmethod tx-remove-objects-slot-index ((pool pool) class slot)
  (let ((index-name (get-index-name class slot)))
    (when (get-index-object pool index-name)
      (remove-root-object pool index-name))))

(defmethod drop-index ((pool pool) class &optional slots)
  (dolist (slot slots)
    (execute-transaction
     (tx-remove-objects-slot-index pool class slot))))

;;;
;;; Index
;;;   - create : (create-index pool object-class slot)
;;;   - get    : (get-index    pool object-class slot)
;;;   - drop   : (drop-index   pool object-class slot)
;;; Pool
;;;   - add    : (add-index    pool Index)
;;; Object
;;;   - add    : (add    pool Index object)
;;;   - remove : (remove pool Index object)

