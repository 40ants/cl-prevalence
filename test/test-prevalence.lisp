;;;; -*- mode: Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Testing Object Prevalence in Common Lisp
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :cl-prevalence)

(defparameter *test-system-directory* (pathname "/tmp/test-prevalence-system/"))

(defvar *test-system* nil)

;; Create a new prevalence system for testing purposes

(let ((directory *test-system-directory*))
  ;; Throw away any xml files that we find: we want to start from scratch
  (when (probe-file directory)
    (dolist (pathname (directory (merge-pathnames "*.xml" directory)))
      (delete-file pathname)))
  (setf *test-system* (make-prevalence-system directory)))

;; A Test CLOS class

(defclass person ()
  ((id :initarg :id :accessor get-id)
   (firstname :initarg :firstname :accessor get-firstname)
   (lastname :initarg :lastname :accessor get-lastname)))

;; Some basic functions to construct transactions from

(defun tx-create-persons-root (system)
  (setf (get-root-object system :persons) (make-hash-table)))

(defun tx-create-person (system firstname lastname)
  (let* ((persons (get-root-object system :persons))
	 (id (tx-get-next-id system))
	 (person (make-instance 'person :id id :firstname firstname :lastname lastname)))
    (setf (gethash id persons) person)))

(defun tx-delete-person (system id)
  (let ((persons (get-root-object system :persons)))
    (remhash id persons)))

;; Create a new id counter

(execute *test-system* (make-transaction 'tx-create-id-counter))

(assert (zerop (get-root-object *test-system* :id-counter)))

;; Create the hash-table holding all known persistent persons and mapping person id' to person objects

(execute *test-system* (make-transaction 'tx-create-persons-root))

(assert (hash-table-p (get-root-object *test-system* :persons)))

;; A place to store our test person's id outside of the system

(defvar *jlp*)

;; Create a new test person

(let ((person (execute *test-system* (make-transaction 'tx-create-person "Jean-Luc" "Picard"))))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Jean-Luc"))
  (assert (equal (get-lastname person) "Picard"))
  (setf *jlp* (get-id person)))

(let ((person (gethash *jlp* (get-root-object *test-system* :persons))))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Jean-Luc"))
  (assert (equal (get-lastname person) "Picard")))

;; Throw away the previous prevalence instance and start over,
;; counting on a restore operation using the transaction log

(close-open-streams *test-system*)
(setf *test-system* (make-prevalence-system *test-system-directory*))

(let ((person (gethash *jlp* (get-root-object *test-system* :persons))))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Jean-Luc"))
  (assert (equal (get-lastname person) "Picard")))

;; Create a snapshot of our test system

(snapshot *test-system*)

(let ((person (gethash *jlp* (get-root-object *test-system* :persons))))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Jean-Luc"))
  (assert (equal (get-lastname person) "Picard")))

;; Throw away the previous prevalence instance and start over,
;; counting on a restore operation using the snapshot

(close-open-streams *test-system*)
(setf *test-system* (make-prevalence-system *test-system-directory*))

(let ((person (gethash *jlp* (get-root-object *test-system* :persons))))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Jean-Luc"))
  (assert (equal (get-lastname person) "Picard")))

;; Create another test person

(defvar *kj*)

(let ((person (execute *test-system* (make-transaction 'tx-create-person "Kathryn" "Janeway"))))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Kathryn"))
  (assert (equal (get-lastname person) "Janeway"))
  (setf *kj* (get-id person)))

(let ((person (gethash *kj* (get-root-object *test-system* :persons))))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Kathryn"))
  (assert (equal (get-lastname person) "Janeway")))

;; Throw away the previous prevalence instance and start over,
;; counting on a restore operation using both the snapshot and the transaction log

(close-open-streams *test-system*)
(setf *test-system* (make-prevalence-system *test-system-directory*))

(let ((person (gethash *jlp* (get-root-object *test-system* :persons))))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Jean-Luc"))
  (assert (equal (get-lastname person) "Picard")))

(let ((person (gethash *kj* (get-root-object *test-system* :persons))))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Kathryn"))
  (assert (equal (get-lastname person) "Janeway")))

(mapcar #'(lambda (pair)
	    (execute *test-system* (make-transaction 'tx-create-person (car pair) (cadr pair))))
	'(("Benjamin" "Sisko") ("James T." "Kirk") ("Jonathan" "Archer")))

(assert (= (hash-table-count (get-root-object *test-system* :persons)) 5))

;;; testing a guarded prevalence system

(defvar *guard*)

(defun guard (thunk)
  (setf *guard* t)
  (funcall thunk))

(close-open-streams *test-system*)
(setf *test-system* (make-prevalence-system *test-system-directory* 
                                            :prevalence-system-class 'guarded-prevalence-system))
(setf (get-guard *test-system*) #'guard)

(let (new-person)
  (setf *guard* nil)
  (setf new-person (execute *test-system* (make-transaction 'tx-create-person "John" "Doe")))
  (assert *guard*)
  (setf *guard* nil)
  (execute *test-system* (make-transaction 'tx-delete-person (get-id new-person)))
  (assert *guard*))

;;; eof
