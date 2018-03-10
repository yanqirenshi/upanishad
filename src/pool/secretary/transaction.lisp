(in-package :upanishad.pool)

(defun make-transaction (function &rest args)
  "Create and return a new transaction specifying a function name and
  an argument list. The function should accept the pool instance
  prepended to the argument list as arguments and implement the actual
  transaction in a re-entrant way."
  (make-instance 'transaction :function function :args args))

(defmacro execute-transaction (transaction-call)
  `(if (not (poolp ,(second transaction-call)))
       (error "第一引数が pool ではありません。第一引数=~a" ,(second transaction-call))
       (execute ,(second transaction-call)
                (make-transaction ',(first transaction-call) ,@(rest (rest transaction-call))))))
