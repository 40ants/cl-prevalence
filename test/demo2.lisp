;;;; -*- mode: Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; A Common Lisp version of the the Java Prevalyer demo2 example
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :cl-prevalence)

;;; Domain Model

(defclass bank ()
  ((accounts-by-number :accessor get-accounts-by-number :initform (make-hash-table :test 'eql))
   (next-account-number :accessor get-next-account-number :initform 1)))

(defclass account ()
  ((number :accessor get-number :initarg :number :initform -1)
   (holder :accessor get-holder :initarg :holder :initform "Unspecified")
   (balance :accessor get-balance :initform 0)
   (transaction-history :accessor get-transaction-history :initform nil)))

(defmethod print-object ((account account) stream)
  (with-slots (number holder balance) account
    (format stream "#<ACCOUNT ~d '~a' $~d>" number holder balance)))

(defclass account-entry ()
  ((amount :accessor get-amount :initarg :amount)
   (timestamp :accessor get-timestamp :initarg :timestamp)))

(defun date-time->string (universal-time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (format nil
	    "~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d"
	    year month date hour minute second)))

(defmethod print-object ((account-entry account-entry) stream)
  (with-slots (timestamp amount) account-entry
    (format stream "#<ACCOUNT-ENTRY ~a ~@d>" (date-time->string timestamp) amount)))

;;; Prevalence System

(defparameter *bank-system-location* (pathname "/tmp/demo2-prevalence-system/"))

(defun tx-create-bank (system)
  (setf (get-root-object system :bank) (make-instance 'bank)))

(defun init-bank-system (system)
  (unless (get-root-object system :bank)
    (execute system (make-transaction 'tx-create-bank)))
  system)

(defvar *bank-system*
  (let ((system (make-prevalence-system *bank-system-location*)))
    (init-bank-system system)))

(defun tx-create-account (system holder)
  (let* ((bank (get-root-object system :bank))
	 (account-number (get-next-account-number bank))
	 (new-account (make-instance 'account
				     :holder holder
				     :number account-number)))
    (setf (gethash account-number (get-accounts-by-number bank)) new-account) 
    (incf (get-next-account-number bank))
    new-account))

(define-condition bank-error (error) ())

(define-condition unknown-account (bank-error)
  ((account-number :reader unknown-account-number :initarg :account-number))
  (:report (lambda (condition stream)
	     (format stream "Unknown account ~a"
		     (unknown-account-number condition)))))

(define-condition overdrawn-account (bank-error)
  ((account :reader overdrawn-account-account :initarg :account)
   (amount :reader overdrawn-account-amount :initarg :amount))
  (:report (lambda (condition stream)
	     (format stream "You cannot withdraw ~d from account ~a"
		     (overdrawn-account-amount condition)
		     (overdrawn-account-account condition)))))

(defun get-account (system account-number)
  (let* ((bank (get-root-object system :bank))
	 (account (gethash account-number (get-accounts-by-number bank))))
    (if account
	account
      (error 'unknown-account :account-number account-number))))

(defun tx-delete-account (system account-number)
  (when (get-account system account-number)
    (remhash account-number (get-accounts-by-number (get-root-object system :bank)))))

(defun tx-change-account-holder (system account-number new-holder)
  (let ((account (get-account system account-number)))
    (setf (get-holder account) new-holder)
    account))

(defun tx-deposit (system account-number amount timestamp)
  (let ((account (get-account system account-number)))
    (incf (get-balance account) amount)
    (push (make-instance 'account-entry :amount amount :timestamp timestamp)
	  (get-transaction-history account))
    account))

(defun tx-withdraw (system account-number amount timestamp)
  (let ((account (get-account system account-number)))
    (if (< (get-balance account) amount)
	(error 'overdrawn-account :account account :amount amount)
      (decf (get-balance account) amount))
    (push (make-instance 'account-entry :amount (- amount) :timestamp timestamp)
	  (get-transaction-history account))
    account))

(defun tx-transfer (system from-account-number to-account-number amount timestamp)
  (let* ((from-account (get-account system from-account-number))
	 (to-account (get-account system to-account-number)))
    (cond ((< (get-balance from-account) amount)
	   (error 'overdrawn-account :amount amount :account from-account))
	  (t (decf (get-balance from-account) amount)
	     (incf (get-balance to-account) amount)
	     (push (make-instance 'account-entry :amount (- amount) :timestamp timestamp)
		   (get-transaction-history from-account))
	     (push (make-instance 'account-entry :amount amount :timestamp timestamp)
		   (get-transaction-history to-account))))
    amount))

(defun get-bank-balance (system)
  (let ((bank (get-root-object system :bank))
	(total 0))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (incf total (get-balance value)))
	     (get-accounts-by-number bank))
    total))

;;; Client Interface

(defun create-account (holder)
  (execute *bank-system* (make-transaction 'tx-create-account holder)))

(defun delete-account (account-number)
  (execute *bank-system* (make-transaction 'tx-delete-account account-number)))

(defun change-account-holder (account-number new-holder)
  (execute *bank-system* (make-transaction 'tx-change-account-holder account-number new-holder)))

(defun deposit (account-number amount)
  (execute *bank-system* (make-transaction 'tx-deposit
					   account-number amount (get-universal-time))))

(defun withdraw (account-number amount)
  (execute *bank-system* (make-transaction 'tx-withdraw
					   account-number amount (get-universal-time))))

(defun transfer (from-account-number to-account-number amount)
  (execute *bank-system* (make-transaction 'tx-transfer
					   from-account-number to-account-number amount (get-universal-time))))

(defun find-account (account-number)
  (let ((bank (get-root-object *bank-system* :bank)))
    (gethash account-number (get-accounts-by-number bank))))

(defun list-all-accounts ()
  (let ((bank (get-root-object *bank-system* :bank))
	accounts)
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push value accounts))
	     (get-accounts-by-number bank))
    accounts))

;;; Now some test code

(defun bank-test-1 ()
  (let ((test-1 (get-number (create-account "Test Account 1")))
	(test-2 (get-number (create-account "Test Account 2"))))
    (assert (zerop (get-balance (find-account test-1))))
    (assert (zerop (get-balance (find-account test-2))))
    (deposit test-1 1000)
    (deposit test-2 2000)
    (withdraw test-2 500)
    (transfer test-2 test-1 500)
    (withdraw test-1 500)
    (assert (= 1000 (get-balance (find-account test-1))))
    (assert (= 1000 (get-balance (find-account test-2))))
    (delete-account test-1)
    (delete-account test-2)
    (print-transaction-log *bank-system*)
    (snapshot *bank-system*)
    (print-snapshot *bank-system*)))

(defun bank-test-2 ()
  (let ((test-1 (get-number (create-account "Test Account 1")))
	(test-2 (get-number (create-account "Test Account 2")))
	now)
    (assert (zerop (get-balance (find-account test-1))))
    (assert (zerop (get-balance (find-account test-2))))
    (deposit test-1 1000)
    (deposit test-2 2000)
    (withdraw test-2 500)
    (transfer test-2 test-1 500)
    (withdraw test-1 500)
    (assert (= 1000 (get-balance (find-account test-1))))
    (assert (= 1000 (get-balance (find-account test-2))))
    (sleep 1)
    (setf now (get-universal-time))
    (restore *bank-system*)
    (let ((account-1 (find-account test-1))
	  (account-2 (find-account test-2)))
      (dolist (account-entry (get-transaction-history account-1))
	(assert (< (get-timestamp account-entry) now)))
      (dolist (account-entry (get-transaction-history account-2))
	(assert (< (get-timestamp account-entry) now))))
    (delete-account test-1)
    (delete-account test-2))
  t)

(defun bank-test-3 ()
  (let ((system (make-prevalence-system *bank-system-location*
					:prevalence-system-class 'guarded-prevalence-system)))
    (query system #'get-bank-balance)
    (close-open-streams system)))

(defmethod initiates-rollback ((bank-error bank-error))
  nil)

(defun tx-bogus-withdraw (system account-number amount)
  (let* ((bank (get-root-object system :bank))
	 (account (gethash account-number (get-accounts-by-number bank))))
    (if (null account)
	(error 'unknown-account :account-number account-number)
      (progn
	;; this is intentionally wrong: we modify before we test
	(decf (get-balance account) amount)
	;; if negative throw a hard error (could initiate rollback)
	(when (< (get-balance account) 0)
	  (error "Account ~a went below zero!" account))))))

(defun bank-test-4 ()
  (let ((account-number (get-number (create-account "bank-test4"))))
    ;; --------------------------------------------------------------
    (format t "Part 1~%")
    ;; disable the rollback option (off by default)
    (setf (get-option *bank-system* :rollback-on-error) nil)
    ;; put 10 bucks on the account
    (deposit account-number 10)
    ;; check that we have 10 bucks
    (assert (= 10 (get-balance (find-account account-number))))
    ;; try to withdraw 20 bucks from the account
    (ignore-errors
      ;; this will fail with an overdrawn-account error
      ;; BEFORE the system is modified (nothing was logged)
      (withdraw account-number 20))
    ;; check that nothing changed
    (assert (= 10 (get-balance (find-account account-number))))
    ;; try to with withdraw 20 bucks using the bogus-withdraw tx
    (ignore-errors
      ;; this will fail with a regular error
      ;; AFTER the system is modified (nothing was logged)
      (execute *bank-system* (make-transaction 'tx-bogus-withdraw account-number 20)))
    ;; check that the change went through
    (assert (= -10 (get-balance (find-account account-number))))
    ;; --------------------------------------------------------------
    (format t "Part 2~%")
    ;; enable the rollback option (off by default)
    (setf (get-option *bank-system* :rollback-on-error) t)
    ;; start over
    (delete-account account-number)
    (setf account-number (get-number (create-account "bank-test4")))
    ;; put 20 bucks on the account
    (deposit account-number 10)
    ;; check that we have 10 bucks 
    (assert (= 10 (get-balance (find-account account-number))))
    ;; try to withdraw 20 bucks from the account
    (ignore-errors
      ;; this will fail with an overdrawn-account error
      ;; BEFORE the system is modified (nothing was logged)
      ;; NO rollback (condition does not initiate a rollback)
      (withdraw account-number 20))
    ;; check that nothing changed
    (assert (= 10 (get-balance (find-account account-number))))
    ;; try to with withdraw 20 bucks using the bogus-withdraw tx
    (ignore-errors
      ;; this will fail with a regular error
      ;; AFTER the system is modified (nothing was logged)
      ;; rollback IS executed (condition does initiate a rollback)
      (execute *bank-system* (make-transaction 'tx-bogus-withdraw account-number 20)))
    ;; check that the rollback took place and nothing changed
    (assert (= 10 (get-balance (find-account account-number))))
    ;; --------------------------------------------------------------
    ;; reset
    (delete-account account-number)
    (setf (get-option *bank-system* :rollback-on-error) nil)))

;;; a multi-processing example

(defparameter *bank-system-lock*
  (make-process-lock "bank-system-lock"))

(defun bank-system-guard (thunk)
  (with-process-lock (*bank-system-lock*) (funcall thunk)))

(defun spawn-process (name function)
  (run-process name function))

(defun bank-test-5-setup ()
  (when *bank-system* (close-open-streams *bank-system*))
  (setf *bank-system* (make-prevalence-system *bank-system-location*
					      :prevalence-system-class 'guarded-prevalence-system))
  (setf (get-guard *bank-system*) #'bank-system-guard)
  (mapcar #'(lambda (account)
	      (delete-account (get-number account)))
	  (list-all-accounts))
  (dotimes (i 10)
    (deposit (get-number (create-account (format nil "bank-test-5-account-~d" i))) 100))
  (assert (= (get-bank-balance *bank-system*) 1000)))

(defparameter *worker-output* *standard-output*)

(defun bank-test-5-worker ()
  (dotimes (i 10)
    (let* ((accounts (list-all-accounts))
	   (from-account (elt accounts (random (length accounts))))
	   (to-account (elt (remove from-account accounts) (random (1- (length accounts)))))
	   (amount (random 100)))
      (catch 'trap-overdraw
	(handler-bind ((overdrawn-account (lambda (condition)
					    (format t "Transfer cancelled (~a)~%" condition)
					    (throw 'trap-overdraw :ignore))))
	  (format *worker-output* "Tranfering ~d from ~a to ~a~%" amount from-account to-account)
	  (transfer (get-number from-account)
		    (get-number to-account)
		    amount))))))

(defun bank-test-5-invariant ()
  (dotimes (i 10)
    (assert (= (query *bank-system* 'get-bank-balance) 1000))))

(defun bank-test-5 ()
  (bank-test-5-setup)
  (spawn-process "invariant" #'bank-test-5-invariant)
  (dotimes (i 10)
    (spawn-process (format nil "bank-test-5-worker-~d" i)
		   #'bank-test-5-worker)
    (spawn-process "invariant" #'bank-test-5-invariant))
  (spawn-process "invariant" #'bank-test-5-invariant)
  (sleep 1)
  (spawn-process "invariant" #'bank-test-5-invariant))
  
;;;; eof