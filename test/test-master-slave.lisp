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

(in-package :cl-prevalence)

;; the master and client systems themselves

(defparameter *master-test-system-directory* (pathname "/tmp/master-test-prevalence-system/"))

(defvar *master-test-system* nil)

(defparameter *slave-test-system-directory* (pathname "/tmp/slave-test-prevalence-system/"))

(defvar *slave-test-system* nil)

;; a test object class

(defclass test-system-user (object-with-id)
  ((username :accessor get-username :initarg :username :initform nil)
   (password :accessor get-password :initarg :password :initform nil)))

;; setup both systems (clearing anything we find)

(when *master-test-system* 
  (totally-destroy *master-test-system*))

(setf *master-test-system* (make-prevalence-system *master-test-system-directory*))
(totally-destroy *master-test-system*)
(execute-transaction (tx-create-id-counter *master-test-system*))

(when *slave-test-system* 
  (totally-destroy *slave-test-system*))

(setf *slave-test-system* (make-prevalence-system *slave-test-system-directory*))
(totally-destroy *slave-test-system*)
(execute-transaction (tx-create-id-counter *slave-test-system*))

;; setup the slave server and the master to slave connection

(defvar *slave-server-name* nil)

(setf *slave-server-name* (start-slave-server *slave-test-system*))

(start-master-client *master-test-system*)

;; now do the test

(defvar *user-id* nil)

(let ((user (execute-transaction (tx-create-object *master-test-system* 
                                                   'test-system-user
                                                   '((username "billg")
                                                     (password "windows"))))))
  (setf *user-id* (get-id user)))

*user-id*

(let ((user (find-object-with-id *master-test-system* 'test-system-user *user-id*)))
  (assert (and (equal (get-username user) "billg")
               (equal (get-password user) "windows"))))

(sleep 1)

(let ((user (find-object-with-id *slave-test-system* 'test-system-user *user-id*)))
  (assert (and (equal (get-username user) "billg")
               (equal (get-password user) "windows"))))

;; stop the master-slave connection and slave server

(stop-master-client *master-test-system*)
(stop-slave-server *slave-server-name*)

;; tidy up a bit

(close-open-streams *master-test-system*)
(close-open-streams *slave-test-system*)

;;;; eof
