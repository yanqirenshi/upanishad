(in-package :upanishad)

(defun class-rootp (symbol)
  (cl-ppcre:scan "^(\\S)+-ROOT$"
                 (symbol-name symbol)))

(defun get-objects-root-name (class)
  (let ((classname (if (symbolp class) (string class) (class-name class))))
    (intern (concatenate 'string classname "-ROOT") :keyword)))

(defmethod get-object-at-%id ((pool pool) class %id)
  (cond ((eq class :all)
         (car (remove nil
                      (mapcar #'(lambda (index)
                                  (gethash %id
                                           (get-root-object pool index)))
                              (class-%id-list pool)))))
        ((symbolp class)
         (let ((index (index-at pool :class class :slot '%id)))
           (when index
             (gethash %id index))))
        (t (error "Bad class. class=~A" class))))

(defun find-all-objects (pool class)
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
  (remove-if #'(lambda (object)
                 (not (funcall test value (slot-value object slot))))
             (find-all-objects pool class)))

(defmethod find-objects-with-slot ((pool pool) class slot value &optional (test #'equalp))
  (let ((index (slot-index-at pool slot :class class)))
    (if index
        (find-objects-with-slot-use-index pool class (gethash value index))
        (find-objects-with-slot-full-scan pool class slot value test))))

(defmethod find-objects ((pool pool) (class symbol)
                         &key (slot nil) (value nil) (test #'equalp))
  (if slot
      (find-objects-with-slot pool class slot value test)
      (find-all-objects pool class)))

(defun slot-value-changed-p (object slot value)
  (or (not (slot-boundp object slot))
      (not (eql (slot-value object slot) value))))

(defmethod tx-create-object ((pool pool) class &optional slots-and-values)
  (let* ((%id (next-%id pool))
         (object (make-instance class :%id %id))
         (index (index-at pool :class class :slot '%id :ensure t)))
    (push object (get-root-object pool (get-objects-root-name class)))
    (setf (gethash %id index) object)
    (tx-change-object-slots pool class %id slots-and-values)
    object))

(defmethod tx-delete-object ((pool pool) class %id)
  (let ((object (get-object-at-%id pool class %id)))
    (if object
        (let ((root-name (get-objects-root-name class))
              (index (index-at pool :class class :slot '%id)))
          (setf (get-root-object pool root-name)
                (delete object (get-root-object pool root-name)))
          (remhash %id index))
        (error "no object of class ~a with %id ~d found in ~s" class %id pool))))

(defmethod tx-change-object-slots ((pool pool) class %id slots-and-values)
  (let ((object (get-object-at-%id pool class %id)))
    (unless object (error "no object of class ~a with %id ~d found in ~s" class %id pool))
    (loop :for (slot value) :in slots-and-values
          :do (when (slot-value-changed-p object slot value)
                (remove-object-from-slot-index pool class slot object)
                (setf (slot-value object slot) value)
                (add-object-to-index pool class slot object)))
    object))

;;;
;;; 4. added iwasaki
;;;

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
