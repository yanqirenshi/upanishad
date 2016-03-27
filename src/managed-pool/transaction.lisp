(in-package :upanishad)

(defmacro execute-transaction (transaction-call)
  "Create a transaction object from transaction-call and execute it"
  `(if (not (poolp ,(second transaction-call)))
       (error "第一引数が pool ではありません。第一引数=~a" ,(second transaction-call))
       (execute ,(second transaction-call)
                (make-transaction ',(first transaction-call) ,@(rest (rest transaction-call))))))


