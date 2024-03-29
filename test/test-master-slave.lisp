;;;; -*- mode: Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Testing a master-slave connection between two prevalence system (on the same host)
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :cl-prevalence-test)

(def-suite test-master-slave :in :cl-prevalence)

(in-suite test-master-slave)

;; the master and client systems themselves

(defparameter *master-test-system-directory* (pathname "/tmp/master-test-prevalence-system/"))

(defvar *master-test-system* nil)

(defparameter *slave-test-system-directory* (pathname "/tmp/slave-test-prevalence-system/"))

(defvar *slave-test-system* nil)

;; a test object class

(defclass test-system-user (object-with-id)
  ((username :accessor get-username :initarg :username :initform nil)
   (password :accessor get-password :initarg :password :initform nil)))

(defvar *slave-server-name* nil)

(defvar *user-id* nil)

(defun start-master-slave ()
  "Setup both master and slave systems (clearing anything we find)."
  (let* (;; (usocket:*wildcard-host*
         ;;   ;; S-SYSDEPS:START-STANDARD-SERVER, used by START-SLAVE-SERVER
         ;;   ;; uses USOCKET:*WILDCARD-HOST* which is 0.0.0.0 by default,
         ;;   ;; but we don't want to use it in unittests:
         ;;   #(127 0 0 1))
         (port (find-port:find-port
                :interface "0.0.0.0")))
    (when *master-test-system* 
      (totally-destroy *master-test-system*))

    (setf *master-test-system* (make-prevalence-system *master-test-system-directory*))
    (is-true *master-test-system*)
    (totally-destroy *master-test-system*)
    (execute-transaction (tx-create-id-counter *master-test-system*))

    (when *slave-test-system* 
      (totally-destroy *slave-test-system*))
    (setf *slave-test-system* (make-prevalence-system *slave-test-system-directory*))
    (is-true *slave-test-system*)
    (totally-destroy *slave-test-system*)
    (execute-transaction (tx-create-id-counter *slave-test-system*))
    (setf *slave-server-name* (start-slave-server *slave-test-system*
                                                  :port port))
    (is-true *slave-server-name*)

    (start-master-client *master-test-system*
                         :port port
                         :host "0.0.0.0")
    (let ((user (execute-transaction (tx-create-object *master-test-system* 
                                                       'test-system-user
                                                       '((username "billg")
                                                         (password "windows"))))))
      (setf *user-id* (get-id user)))
    (is-true *user-id*)
    *user-id*))

(defun stop-master-slave ()
  "Stop the master-slave connection and slave server tidy up a bit"
  (stop-master-client *master-test-system*)
  (stop-slave-server *slave-server-name*)

  (close-open-streams *master-test-system*)
  (close-open-streams *slave-test-system*))

;; The value #<BASIC-TCP-STREAM ISO-8859-1 (SOCKET/20) #x3020018035CD> is not of the expected type (AND CCL::CHARACTER-STREAM CCL:OUTPUT-STREAM)
(defmacro master-slave-test (test-name &body body)
  `(test ,test-name
     #+ccl
     (fiveam:skip "Skipped because on CCL master/slave fails with this error: The value #<BASIC-TCP-STREAM ISO-8859-1 (SOCKET/20) #x3020018035CD> is not of the expected type (AND CCL::CHARACTER-STREAM CCL:OUTPUT-STREAM)")

     #-ccl
     (progn 
       (start-master-slave)
       ;; Plato Wu,2009/02/27: because it need time to transfer data from master to slave?
       (sleep 1)
       (unwind-protect (progn ,@body)
         (stop-master-slave)))))

;; now do the test

(master-slave-test test-get-master-user
  (let ((user (find-object-with-id *master-test-system* 'test-system-user *user-id*)))
    (is (and user
             (equal (get-username user) "billg")
	     (equal (get-password user) "windows")))))

(master-slave-test test-get-slave-user
  (let ((user (find-object-with-id *slave-test-system* 'test-system-user *user-id*)))
    (is (and user
             (equal (get-username user) "billg")
             (equal (get-password user) "windows")))))

;;;; eof
