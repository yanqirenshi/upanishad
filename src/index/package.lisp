(in-package :cl-user)

(defpackage :upanishad.index
  (:use :cl)
  (:nicknames :up.index)
  (:import-from #:upanishad.meme
                #:%id)
  (:export #:index)
  (:export #:slot-index
           #:class-symbol
           #:slot-symbol)
  (:export #:slot-index-unique
           #:value->object)
  (:export #:slot-index-multiple
           #:value->objects)
  (:export #:make-slot-index
           #:get-index-key
           #:get-at-value
           #:add-object
           #:add-objects
           #:remove-object
           #:change-object)
  (:documentation ""))
