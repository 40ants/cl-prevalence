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

(in-package :cl-prevalence)

(defparameter *test-system-directory* (pathname "/tmp/test-managed-prevalence-system/"))

(defvar *test-system* nil)

;; Create a new prevalence system for testing purposes

(let ((directory *test-system-directory*))
  ;; Throw away any xml files that we find: we want to start from scratch
  (when (probe-file directory)
    (dolist (pathname (directory (merge-pathnames "*.xml" directory)))
      (delete-file pathname)))
  (setf *test-system* (make-prevalence-system directory)))

;; A Test CLOS class

(defclass person (object-with-id)
  ((firstname :initarg :firstname :initform "" :accessor get-firstname)
   (lastname  :initarg :lastname  :initform "" :accessor get-lastname)))

(defmethod (setf get-firstname) (value (person person))
  (execute-transaction 
   (tx-change-object-slots *test-system* 'person (get-id person) (list (list 'firstname value)))))

(defmethod (setf get-lastname) (value (person person))
  (execute-transaction 
   (tx-change-object-slots *test-system* 'person (get-id person) (list (list 'lastname value)))))

(index-on *test-system* 'person '(firstname lastname) 'equal)

;; convenience function

(defun pairify (list)
  (when list (concatenate 'list 
                          (list (subseq list 0 2)) 
                          (pairify (rest (rest list))))))

;; Some basic functions to construct transactions from

(defun make-person (&rest slots)
  (let ((slots-and-values (pairify slots)))
    (execute-transaction 
     (tx-create-object *test-system* 'person slots-and-values))))

(defun find-person (slot value)
  (find-object-with-slot *test-system* 'person slot value))

(defun delete-person (person)
  (execute-transaction 
   (tx-delete-object *test-system* 'person (get-id person))))

;; Create a new id counter

(execute-transaction (tx-create-id-counter *test-system*))

(assert (zerop (get-root-object *test-system* :id-counter)))

;; A place to store our test person's id outside of the system

(defvar *jlp*)

;; Create a new test person

(let ((person (make-person 'firstname "Jean-Luc" 'lastname "Picard")))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Jean-Luc"))
  (assert (equal (get-lastname person) "Picard"))
  (assert (equal (get-lastname (find-person 'firstname "Jean-Luc")) "Picard"))
  (setf (get-firstname (find-person 'lastname "Picard")) "J-Lu")
  (assert (equal (get-lastname (find-person 'firstname "J-Lu")) "Picard"))
  (setf (get-firstname (find-person 'firstname "J-Lu")) "Jean-Luc")
  (assert (equal (get-firstname (find-person 'lastname "Picard")) "Jean-Luc"))
  (assert (eq NIL (find-person 'firstname "J-Lu")))
  (setf *jlp* (get-id person)))

(let ((person (find-object-with-id *test-system* 'person *jlp*)))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Jean-Luc"))
  (assert (equal (get-lastname person) "Picard"))
  (assert (equal (get-lastname (find-person 'firstname "Jean-Luc")) "Picard"))
  (setf (get-firstname (find-person 'lastname "Picard")) "J-Lu")
  (assert (equal (get-lastname (find-person 'firstname "J-Lu")) "Picard"))
  (setf (get-firstname (find-person 'firstname "J-Lu")) "Jean-Luc")
  (assert (equal (get-firstname (find-person 'lastname "Picard")) "Jean-Luc"))
  (assert (eq NIL (find-person 'firstname "J-Lu"))))

;; Throw away the previous prevalence instance and start over,
;; counting on a restore operation using the transaction log

(close-open-streams *test-system*)
(setf *test-system* (make-prevalence-system *test-system-directory*))

(let ((person (find-object-with-id *test-system* 'person *jlp*)))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Jean-Luc"))
  (assert (equal (get-lastname person) "Picard"))
  (assert (equal (get-lastname (find-person 'firstname "Jean-Luc")) "Picard"))
  (setf (get-firstname (find-person 'lastname "Picard")) "J-Lu")
  (assert (equal (get-lastname (find-person 'firstname "J-Lu")) "Picard"))
  (setf (get-firstname (find-person 'firstname "J-Lu")) "Jean-Luc")
  (assert (equal (get-firstname (find-person 'lastname "Picard")) "Jean-Luc"))
  (assert (eq NIL (find-person 'firstname "J-Lu"))))

;; Create a snapshot of our test system

(snapshot *test-system*)

(let ((person (find-object-with-id *test-system* 'person *jlp*)))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Jean-Luc"))
  (assert (equal (get-lastname person) "Picard"))
  (assert (equal (get-lastname (find-person 'firstname "Jean-Luc")) "Picard"))
  (setf (get-firstname (find-person 'lastname "Picard")) "J-Lu")
  (assert (equal (get-lastname (find-person 'firstname "J-Lu")) "Picard"))
  (setf (get-firstname (find-person 'firstname "J-Lu")) "Jean-Luc")
  (assert (equal (get-firstname (find-person 'lastname "Picard")) "Jean-Luc"))
  (assert (eq NIL (find-person 'firstname "J-Lu"))))

;; Throw away the previous prevalence instance and start over,
;; counting on a restore operation using the snapshot

(close-open-streams *test-system*)
(setf *test-system* (make-prevalence-system *test-system-directory*))

(let ((person (find-object-with-id *test-system* 'person *jlp*)))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Jean-Luc"))
  (assert (equal (get-lastname person) "Picard"))
  (assert (equal (get-lastname (find-person 'firstname "Jean-Luc")) "Picard"))
  (setf (get-firstname (find-person 'lastname "Picard")) "J-Lu")
  (assert (equal (get-lastname (find-person 'firstname "J-Lu")) "Picard"))
  (setf (get-firstname (find-person 'firstname "J-Lu")) "Jean-Luc")
  (assert (equal (get-firstname (find-person 'lastname "Picard")) "Jean-Luc"))
  (assert (eq NIL (find-person 'firstname "J-Lu"))))

;; Create another test person

(defvar *kj*)

(let ((person (make-person 'firstname "Kathryn" 'lastname "Janeway")))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Kathryn"))
  (assert (equal (get-lastname person) "Janeway"))
  (assert (equal (get-firstname (find-person 'lastname "Janeway")) "Kathryn"))
  (assert (equal (get-lastname (find-person 'firstname "Kathryn")) "Janeway"))
  (setf *kj* (get-id person)))

(let ((person (find-object-with-id *test-system* 'person *kj*)))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Kathryn"))
  (assert (equal (get-lastname person) "Janeway"))
  (assert (equal (get-firstname (find-person 'lastname "Janeway")) "Kathryn"))
  (assert (equal (get-lastname (find-person 'firstname "Kathryn")) "Janeway")))

;; Throw away the previous prevalence instance and start over,
;; counting on a restore operation using both the snapshot and the transaction log

(close-open-streams *test-system*)
(setf *test-system* (make-prevalence-system *test-system-directory*))

(let ((person (find-object-with-id *test-system* 'person *jlp*)))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Jean-Luc"))
  (assert (equal (get-lastname person) "Picard"))
  (assert (equal (get-lastname (find-person 'firstname "Jean-Luc")) "Picard"))
  (setf (get-firstname (find-person 'lastname "Picard")) "J-Lu")
  (assert (equal (get-lastname (find-person 'firstname "J-Lu")) "Picard"))
  (setf (get-firstname (find-person 'firstname "J-Lu")) "Jean-Luc")
  (assert (equal (get-firstname (find-person 'lastname "Picard")) "Jean-Luc"))
  (assert (eq NIL (find-person 'firstname "J-Lu"))))

(let ((person (find-object-with-id *test-system* 'person *kj*)))
  (assert (eq (class-of person) (find-class 'person)))
  (assert (equal (get-firstname person) "Kathryn"))
  (assert (equal (get-lastname person) "Janeway"))
  (assert (equal (get-firstname (find-person 'lastname "Janeway")) "Kathryn"))
  (assert (equal (get-lastname (find-person 'firstname "Kathryn")) "Janeway")))

(mapcar #'(lambda (pair)
           (make-person 'firstname (first pair) 'lastname (second pair)))
	'(("Benjamin" "Sisko") ("James T." "Kirk") ("Jonathan" "Archer")))

(assert (= (length (find-all-objects *test-system* 'person)) 5))


;;; testing a guarded prevalence system
;;; [Not sure that we need the below test here -- RRR]

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
  (setf new-person (make-person 'firstname "John" 'lastname "Doe"))
  (assert *guard*)
  (setf *guard* nil)
  (delete-person new-person)
  (assert *guard*))

;;; eof
