(in-package :upanishad)

(defun start-master-client (pool &key (host "localhost") (port 7651))
  "Start a connection to host:port to deliver transactions from pool"
  (stop-master-client pool)
  (let ((out (s-sysdeps:open-socket-stream host port)))
    (setf (get-transaction-hook pool)
          #'(lambda (transaction)
              (funcall (get-serializer pool)
                       transaction
                       out
                       (get-serialization-state pool))
              (finish-output out)
              (when (eq transaction :stop)
                (close out)))))
  t)


(defun stop-master-client (pool)
  "Stop a connection from pool"
  (with-slots (transaction-hook)
      pool
    (when transaction-hook
      (funcall transaction-hook :stop)
      (setf transaction-hook #'identity))))


(defun start-slave-server (pool &key (port 7651))
  "Start a server on port accepting transactions to be executed on pool"
  (s-sysdeps:start-standard-server
   :port port
   :name "upanishad-pool-slave-server"
   :connection-handler #'(lambda (stream)
                           (loop
                             (let ((transaction (funcall (get-deserializer pool)
                                                         stream
                                                         (get-serialization-state pool))))
                               (if (or (null transaction)
                                       (eq transaction :stop))
                                   (return)
                                   (execute pool transaction)))))))


(defun stop-slave-server (server)
  ;; Plato Wu,2009/02/26: stop-server need be exported in s-sysdeps.
  (s-sysdeps::stop-server (caar server)))
