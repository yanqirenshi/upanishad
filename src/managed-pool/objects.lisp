(in-package :upanishad.objects)

(defclass objects (meme)
  ((class-symbol
    :documentation ""
    :accessor class-symbol
    :initarg :class-symbol)
   (contents :documentation ""
             :accessor contents
             :initarg :contents
             :initform nil)
   (%id-ht :documentation ""
           :accessor %id-ht
           :initarg :%id-ht
           :initform (make-hash-table))))

(defgeneric add-object (objects object)
  (:method ((objects objects) (object upanishad:meme))
    (let ((%id-ht (%id-ht objects))
          (%id object))
      (when (gethash %id %id-ht)
        (error "Aledy exist object"))
      (setf (gethash %id %id-ht) object)
      (push object (contents objects)))))

(defgeneric remove-object (objects object)
  (:method ((objects objects) (object up:meme))
    (let ((%id-ht (%id-ht objects))
          (%id object))
      (unless (gethash %id %id-ht)
        (error "Not exist object"))
      (remhash %id %id-ht)
      (remove object objects))))

(defun make-objects (object-symbol &key objects)
  (let ((new-objects (make-instance 'objects :object-symbol object-symbol)))
    (dolist (object objects)
      (add-object new-objects object))))
