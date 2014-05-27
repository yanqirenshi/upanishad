;;;; -*- mode: lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; The master-slave system keeps one prevalence system in sync with another
;;;; by sending transactions over a socket
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

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

(defun stop-master-client (prevalence-sytem)
  "Stop a connection from pool"
  (with-slots (transaction-hook)
      prevalence-sytem
    (when transaction-hook
      (funcall transaction-hook :stop)
      (setf transaction-hook #'identity))))

(defun start-slave-server (pool &key (port 7651))
  "Start a server on port accepting transactions to be executed on pool"
  (s-sysdeps:start-standard-server
   :port port
   :name "prevalence-slave-server"
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
  (s-sysdeps::stop-server (caar server))
  )

;;;; eof
