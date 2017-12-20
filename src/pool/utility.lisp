(in-package :upanishad.pool)

(defun getter-name (slot-name)
  (intern (string-upcase (format nil "get-~a" slot-name))))


(defmacro defreader (class-name slot-name &key (documentation ""))
  `(defgeneric ,(getter-name slot-name) (obj)
     (:documentation ,documentation)
     (:method ((obj ,class-name))
       (slot-value obj ',slot-name))))


(defmacro defwriter (class-name slot-name &key (documentation ""))
  `(defgeneric (setf ,(getter-name slot-name)) (value obj)
     (:documentation ,documentation)
     (:method (value (obj ,class-name))
       (setf (slot-value obj ',slot-name) value))))

