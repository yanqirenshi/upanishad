(in-package :cl-user)

(defpackage :upanishad.pool.secretary
  (:use #:cl
        #:s-serialization)
  (:nicknames :up.pool.secretary)
  (:documentation "")
  (:export #:no-rollback-error
           #:execute-on
           #:execute
           #:execute-transaction))
(in-package :upanishad.pool.secretary)
