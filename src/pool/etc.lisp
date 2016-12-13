(in-package :upanishad)

;;;
;;; 7. from the serialization package
;;;
(defmethod reset-known-slots ((pool pool) &optional class)
  (reset-known-slots (serialization-state pool) class))



;;;
;;; 8. extra documentation
;;;

(setf (documentation 'guard 'function) "Access the guard function of a sytem")

#-allegro
(setf (documentation '(setf guard) 'function) "Set the guard function of a pool")
