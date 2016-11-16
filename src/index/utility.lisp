(in-package :upanishad.index)

(defun get-slot-index-class (type)
  (ecase type
    (:unique 'slot-index-unique)
    (:multiple 'slot-index-multiple)))

(defun assert-class (class meme)
  (unless (eq class (type-of meme))
    (error "index is not meme's slot index")))
