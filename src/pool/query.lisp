(in-package :upanishad.pool)

(defmethod query ((pool pool) function &rest args)
  "Execute an exclusive query function on a sytem"
  (apply function (cons pool args)))
