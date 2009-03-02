;;;; -*- mode: Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Testing Managed Object Prevalence in Common Lisp
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;; Altered for managed prevalence testing; Sept 2004, 
;;;;  Randall Randall, RandallSquared
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
(in-package :cl-prevalence-test)

(def-suite test-managed-prevalence :in cl-prevalence-test)

(in-suite test-managed-prevalence)

(defparameter *test-system-directory* (pathname "/tmp/test-managed-prevalence-system/"))

(defvar *test-system* nil)

(test test-managed-prevalence-start
 "Create a new prevalence system for testing purposes"
 (let ((directory *test-system-directory*))
   ;; Throw away any xml files that we find: we want to start from scratch
   (when (probe-file directory)
     (dolist (pathname (directory (merge-pathnames "*.xml" directory)))
       (delete-file pathname)))
   (setf *test-system* (make-prevalence-system directory))
   (is-true *test-system*)
   (index-on *test-system* 'managed-person '(firstname lastname) 'equal)))

;; A Test CLOS class

(defclass managed-person (object-with-id)
  ((firstname :initarg :firstname :initform "" :accessor get-firstname)
   (lastname  :initarg :lastname  :initform "" :accessor get-lastname)))

(defmethod (setf get-firstname) (value (managed-person managed-person))
  (execute-transaction 
   (tx-change-object-slots *test-system* 'managed-person (get-id managed-person) (list (list 'firstname value)))))

(defmethod (setf get-lastname) (value (managed-person managed-person))
  (execute-transaction 
   (tx-change-object-slots *test-system* 'managed-person (get-id managed-person) (list (list 'lastname value)))))

;; convenience function

(defun pairify (list)
  (when list (concatenate 'list 
                          (list (subseq list 0 2)) 
                          (pairify (rest (rest list))))))

;; Some basic functions to construct transactions from

(defun make-managed-person (&rest slots)
  (let ((slots-and-values (pairify slots)))
    (execute-transaction 
     (tx-create-object *test-system* 'managed-person slots-and-values))))

(defun find-managed-person (slot value)
  (find-object-with-slot *test-system* 'managed-person slot value))

(defun delete-managed-person (managed-person)
  (execute-transaction 
   (tx-delete-object *test-system* 'managed-person (get-id managed-person))))

(test test-create-counter
  "Create a new id counter"
  (execute-transaction (tx-create-id-counter *test-system*))
  (is (zerop (get-root-object *test-system* :id-counter))))
    

;; A place to store our test managed-person's id outside of the system

(defvar *jlp*)

(test test-create-managed-person
  "Create a new test managed-person"
  (let ((managed-person (make-managed-person 'firstname "Jean-Luc" 'lastname "Picard")))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Jean-Luc"))
    (is (equal (get-lastname managed-person) "Picard"))
    (is (equal (get-lastname (find-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (find-managed-person 'lastname "Picard")) "J-Lu")
    (is (equal (get-lastname (find-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (find-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (is (equal (get-firstname (find-managed-person 'lastname "Picard")) "Jean-Luc"))
    (is (eq NIL (find-managed-person 'firstname "J-Lu")))
    (setf *jlp* (get-id managed-person))))

(test test-get-managed-person
  (let ((managed-person (find-object-with-id *test-system* 'managed-person *jlp*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Jean-Luc"))
    (is (equal (get-lastname managed-person) "Picard"))
    (is (equal (get-lastname (find-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (find-managed-person 'lastname "Picard")) "J-Lu")
    (is (equal (get-lastname (find-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (find-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (is (equal (get-firstname (find-managed-person 'lastname "Picard")) "Jean-Luc"))
    (is (eq NIL (find-managed-person 'firstname "J-Lu")))))

(test test-find-managed-person-restart
  "Throw away the previous prevalence instance and start over,
  counting on a restore operation using the transaction log"
  (close-open-streams *test-system*)
  (setf *test-system* (make-prevalence-system *test-system-directory*))
  (let ((managed-person (find-object-with-id *test-system* 'managed-person *jlp*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Jean-Luc"))
    (is (equal (get-lastname managed-person) "Picard"))
    (is (equal (get-lastname (find-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (find-managed-person 'lastname "Picard")) "J-Lu")
    (is (equal (get-lastname (find-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (find-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (is (equal (get-firstname (find-managed-person 'lastname "Picard")) "Jean-Luc"))
    (is (eq NIL (find-managed-person 'firstname "J-Lu")))))

(test test-find-managed-person-snapshot
  "Create a snapshot of our test system"
  (snapshot *test-system*)
  (let ((managed-person (find-object-with-id *test-system* 'managed-person *jlp*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Jean-Luc"))
    (is (equal (get-lastname managed-person) "Picard"))
    (is (equal (get-lastname (find-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (find-managed-person 'lastname "Picard")) "J-Lu")
    (is (equal (get-lastname (find-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (find-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (is (equal (get-firstname (find-managed-person 'lastname "Picard")) "Jean-Luc"))
    (is (eq NIL (find-managed-person 'firstname "J-Lu")))))

(test test-find-managed-person-restart-snapshot
  "Throw away the previous prevalence instance and start over,
  counting on a restore operation using the snapshot"
  (close-open-streams *test-system*)
  (setf *test-system* (make-prevalence-system *test-system-directory*))
  (let ((managed-person (find-object-with-id *test-system* 'managed-person *jlp*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Jean-Luc"))
    (is (equal (get-lastname managed-person) "Picard"))
    (is (equal (get-lastname (find-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (find-managed-person 'lastname "Picard")) "J-Lu")
    (is (equal (get-lastname (find-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (find-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (is (equal (get-firstname (find-managed-person 'lastname "Picard")) "Jean-Luc"))
    (is (eq NIL (find-managed-person 'firstname "J-Lu")))))

(defvar *kj*)

(test test-create-managed-person-1
  "Create another test managed-person"
  (let ((managed-person (make-managed-person 'firstname "Kathryn" 'lastname "Janeway")))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Kathryn"))
    (is (equal (get-lastname managed-person) "Janeway"))
    (is (equal (get-firstname (find-managed-person 'lastname "Janeway")) "Kathryn"))
    (is (equal (get-lastname (find-managed-person 'firstname "Kathryn")) "Janeway"))
    (setf *kj* (get-id managed-person))))

(test test-find-managed-person-1
  (let ((managed-person (find-object-with-id *test-system* 'managed-person *kj*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Kathryn"))
    (is (equal (get-lastname managed-person) "Janeway"))
    (is (equal (get-firstname (find-managed-person 'lastname "Janeway")) "Kathryn"))
    (is (equal (get-lastname (find-managed-person 'firstname "Kathryn")) "Janeway"))))

(test test-find-managed-person-restart-1
  "Throw away the previous prevalence instance and start over,
   counting on a restore operation using both the snapshot and the transaction log"
  (close-open-streams *test-system*)
  (setf *test-system* (make-prevalence-system *test-system-directory*))
  (let ((managed-person (find-object-with-id *test-system* 'managed-person *jlp*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Jean-Luc"))
    (is (equal (get-lastname managed-person) "Picard"))
    (is (equal (get-lastname (find-managed-person 'firstname "Jean-Luc")) "Picard"))
    (setf (get-firstname (find-managed-person 'lastname "Picard")) "J-Lu")
    (is (equal (get-lastname (find-managed-person 'firstname "J-Lu")) "Picard"))
    (setf (get-firstname (find-managed-person 'firstname "J-Lu")) "Jean-Luc")
    (is (equal (get-firstname (find-managed-person 'lastname "Picard")) "Jean-Luc"))
    (is (eq NIL (find-managed-person 'firstname "J-Lu")))))

(test test-find-managed-person-restart-2
  (let ((managed-person (find-object-with-id *test-system* 'managed-person *kj*)))
    (is (eq (class-of managed-person) (find-class 'managed-person)))
    (is (equal (get-firstname managed-person) "Kathryn"))
    (is (equal (get-lastname managed-person) "Janeway"))
    (is (equal (get-firstname (find-managed-person 'lastname "Janeway")) "Kathryn"))
    (is (equal (get-lastname (find-managed-person 'firstname "Kathryn")) "Janeway"))))

(test test-managed-person-count
  (mapcar #'(lambda (pair)
	      (make-managed-person 'firstname (first pair) 'lastname (second pair)))
	  '(("Benjamin" "Sisko") ("James T." "Kirk") ("Jonathan" "Archer")))
  (is (= (length (find-all-objects *test-system* 'managed-person)) 5))
  (mapcar #'(lambda (pair)
	      (delete-managed-person (find-managed-person 'firstname (first pair))))
	  '(("Benjamin" "Sisko") ("James T." "Kirk") ("Jonathan" "Archer")))
  (is (= (length (find-all-objects *test-system* 'managed-person)) 2)))

(defvar *managed-guard*)

(defun managed-guard (thunk)
  (setf *managed-guard* t)
  (funcall thunk))

(test test-managed-guarded
  "testing a managed-guarded prevalence system
   [Not sure that we need the below test here -- RRR]"
  (close-open-streams *test-system*)
  (setf *test-system* (make-prevalence-system *test-system-directory* 
                                            :prevalence-system-class 'guarded-prevalence-system))
  (setf (get-guard *test-system*) #'managed-guard)
  (let (new-managed-person)
    (setf *managed-guard* nil)
    (setf new-managed-person (make-managed-person 'firstname "John" 'lastname "Doe"))
    (is-true *managed-guard*)
    (setf *managed-guard* nil)
    (delete-managed-person new-managed-person)
    (is-true *managed-guard*)))

;;; eof
