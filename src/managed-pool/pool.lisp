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
(defmethod get-preference ((pool pool) key)
  "Retrieve the value of the persistent preference stored under key in pool"
  (let ((preferences (get-root-object pool :preferences)))
    (when preferences
      (gethash key preferences))))


(defun get-objects-root-name (class)
  "Return the keyword symbol naming the root of instances of class"
  (let ((classname (if (symbolp class) (string class) (class-name class))))
    (intern (concatenate 'string classname "-ROOT") :keyword)))


(defmethod get-object-at-%id ((pool pool) class %id)
  "Find and return the object in pool of class with %id, null if not found"
  (cond ((eq class :all)
         (car (remove nil
                      (mapcar #'(lambda (index)
                                  (gethash %id
                                           (get-root-object pool index)))
                              (class-%id-list pool)))))
        ((symbolp class)
         (let* ((index-name (get-objects-slot-index-name class '%id))
                (index (get-root-object pool index-name)))
           (when index
             (gethash %id index))))
        (t (error "Bad class. class=~A" class))))


(defun find-all-objects (pool class)
  "Return an unordered collection of all objects in pool that are instances of class"
  (let ((root-name (get-objects-root-name class)))
    (copy-list (get-root-object pool root-name))))

(defun find-objects-with-slot-use-index (pool class index)
  (when index
    (let* ((%ids (alexandria:hash-table-values  index))
           (len (length %ids)))
      (cond ((= len 0) nil)
            ((= len 1) (list (get-object-at-%id pool class (first %ids))))
            (t (mapcar #'(lambda (id)
                           (get-object-at-%id pool class id))
                       %ids))))))

(defun find-objects-with-slot-full-scan (pool class slot value test)
  "オブジェクトを全件検索します。"
  (remove-if #'(lambda (object)
                 (not (funcall test value (slot-value object slot))))
             (find-all-objects pool class)))

(defmethod find-objects-with-slot ((pool pool) class slot value &optional (test #'equalp))
  (let* ((index-name (get-objects-slot-index-name class slot))
         (index      (get-root-object pool index-name)))
    (if index
        (find-objects-with-slot-use-index pool class (gethash value index))
        (find-objects-with-slot-full-scan pool class slot value test))))

(defmethod find-objects ((pool pool) (class symbol)
                         &key (slot nil) (value nil) (test #'equalp))
  (if slot
      (find-objects-with-slot pool class slot value test)
      (find-all-objects pool class)))

(defun slot-value-changed-p (object slot value)
  "Return true when slot in object is not eql to value (or when the slot was unbound)"
  (or (not (slot-boundp object slot))
      (not (eql (slot-value object slot) value))))


(defmethod tx-create-object ((pool pool) class &optional slots-and-values)
  (let* ((%id (next-%id pool))
         (object (make-instance class :%id %id))
         (index-name (get-objects-slot-index-name class '%id))
         (index (or (get-root-object pool index-name)
                    (setf (get-root-object pool index-name) (make-hash-table)))))
    (push object (get-root-object pool (get-objects-root-name class)))
    (setf (gethash %id index) object)
    (tx-change-object-slots pool class %id slots-and-values)
    object))


(defmethod tx-delete-object ((pool pool) class %id)
  (let ((object (get-object-at-%id pool class %id)))
    (if object
        (let ((root-name (get-objects-root-name class))
              (index-name (get-objects-slot-index-name class '%id)))
          (setf (get-root-object pool root-name) (delete object (get-root-object pool root-name)))
          (remhash %id (get-root-object pool index-name)))
        (error "no object of class ~a with %id ~d found in ~s" class %id pool))))


(defmethod tx-change-object-slots ((pool pool) class %id slots-and-values)
  (let ((object (get-object-at-%id pool class %id)))
    (unless object (error "no object of class ~a with %id ~d found in ~s" class %id pool))
    (loop :for (slot value) :in slots-and-values
          :do (when (slot-value-changed-p object slot value)
                (remove-object-from-slot-index pool class slot object)
                (setf (slot-value object slot) value)
                (add-object-to-slot-index pool class slot object)))
    object))



;;;
;;; 2. We use a simple %id counter to generate unique object identifiers
;;;
(defmethod tx-create-%id-counter ((pool pool))
  (setf (get-root-object pool :%id-counter) 0))


(defmethod next-%id ((pool pool))
  (incf (get-root-object pool :%id-counter)))


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
      (remhash (%id  obj)
               (gethash (slot-value obj slot-symbol) index)))))


(defun class-rootp (symbol)
  (cl-ppcre:scan "^(\\S)+-ROOT$"
                 (symbol-name symbol)))


(defmethod class-%id-list ((pool pool))
  (remove-if (complement #'class-%id-indexp)
             (alexandria:hash-table-keys
              (root-objects pool))))


(defmethod root-list ((pool pool))
  (remove-if (complement #'class-rootp)
             (alexandria:hash-table-keys
              (root-objects pool))))


(defun object-root-name (symbol)
  (get-objects-root-name symbol))


(defmethod get-object-list ((pool pool) (class-symbol symbol))
  (get-root-object pool
                   (get-objects-root-name class-symbol)))


(defmethod print-root-list ((pool pool) &key (stream t))
  (mapcar #'(lambda (root)
              (format stream "~10a : count=~a~%"
                      root
                      (length (get-root-object pool root))))
          (root-list pool))
  pool)
