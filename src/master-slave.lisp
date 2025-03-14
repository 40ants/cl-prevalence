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

(in-package :cl-prevalence)

(defun start-master-client (prevalence-system &key (host "localhost") (port 7651))
  "Start a connection to host:port to deliver transactions from prevalence-system"
  (stop-master-client prevalence-system)
  (let ((out (s-sysdeps:open-socket-stream host port)))
    (setf (get-transaction-hook prevalence-system)
          #'(lambda (transaction)
              (funcall (get-serializer prevalence-system) 
                       transaction 
                       out 
                       (get-serialization-state prevalence-system))
              (finish-output out)
              (when (eq transaction :stop)
                (close out)))))
  t)

(defun stop-master-client (prevalence-sytem)
  "Stop a connection from prevalence-system"
  (with-slots (transaction-hook)
      prevalence-sytem
    (when transaction-hook
      (funcall transaction-hook :stop)
      (setf transaction-hook #'identity))))

(defun start-slave-server (prevalence-system &key (port 7651))
  "Start a server on port accepting transactions to be executed on prevalence-system
   Returns a thread object"
  #+ccl
  (error "On CCL master/slave does not work because of this error: BASIC-TCP-STREAM ISO-8859-1 (SOCKET/17) #x30200181FD9D> is not of the expected type (AND CCL::CHARACTER-STREAM CCL:OUTPUT-STREAM")
  
  (s-sysdeps:start-standard-server 
   :port port
   :name "prevalence-slave-server"
   :connection-handler #'(lambda (stream)
                           (loop 
                             (let ((transaction (funcall (get-deserializer prevalence-system)
                                                         stream
                                                         (get-serialization-state prevalence-system))))
                               (if (or (null transaction)
                                       (eq transaction :stop))
                                   (return)
                                   (execute prevalence-system transaction)))))))

(defun stop-slave-server (server-thread)
  (s-sysdeps:stop-process (bt2:thread-name server-thread))
  (values))

;;;; eof
