(in-package :upanishad.index)

(defclass index ()
  ()
  (:documentation "インデックスのスーパークラスです。"))

(defclass slot-index (index)
  ((class-symbol :documentation ""
                 :accessor class-symbol
                 :initarg :class-symbol
                 :initform nil)
   (slot-symbol :documentation ""
                :accessor slot-symbol
                :initarg :slot-symbol
                :initform nil)
   (%id->value :documentation ""
               :accessor %id->value
               :initarg :%id->value
               :initform (make-hash-table :test 'eq))))

(defclass slot-index-unique (slot-index)
  ((value->object :documentation "value:object=1:1"
                  :accessor value->object
                  :initarg :value->object
                  :initform (make-hash-table :test 'equalp)))
  (:documentation ""))

(defclass slot-index-multiple (slot-index)
  ((value->objects :documentation "value:object=1:n"
                   :accessor value->objects
                   :initarg :value->objects
                   :initform (make-hash-table :test 'equalp)))
  (:documentation ""))
