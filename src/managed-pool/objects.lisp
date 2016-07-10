(in-package :upanishad.objects)

(defclass objects (meme)
  ((contents :documentation ""
             :accessor contents
             :initarg :contents
             :initform nil)
   (%id-ht :documentation ""
           :accessor %id-ht
           :initarg :%id-ht
           :initform (make-hash-table))))

(defun make-objects-%id-ht (objects &optional (%id-ht (make-hash-table)))
  (when objects 
    (let ((object (car objects)))
      ;; TODO: check meme instance object
      (setf (gethash (up:%id object) %id-ht) object)
      (make-objects-%id-ht (cdr objects) %id-ht))))

(defun make-objects (&key objects)
  (make-instance 'objects
                 :contents objects
                 :%id-ht (make-objects-%id-ht objects)))

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
    
