(in-package :upanishad.index)

#|

# Contents

1. Index
1. Slot Index
1. Slot Index Unique
1. Slot Index multiple

|#

;;;;;
;;;;; Index
;;;;;
(defclass index () ())

(defgeneric get-index-key (index))
(defgeneric add-object (index object))
(defgeneric add-objects (index objects))
(defgeneric remove-object (index object))
