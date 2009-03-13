;;;; -*- mode: Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; A Common Lisp version of the the Java Prevalyer demo1 example
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :cl-prevalence)

(defun prime-p (n)
  "Prime predicate copied from Java code"
  (cond ((< n 2) nil)
	((= n 2) t)
	((evenp n) nil)
	(t (let ((factor 3)
		 (square (ceiling (sqrt n))))
	     (loop
	      (unless (<= factor square)
		(return-from prime-p t))
	      (if (zerop (mod n factor))
		  (return-from prime-p nil)
		(incf factor 2)))))))

(defclass numbers ()
  ((numbers-list :accessor get-numbers-list :initform nil))
  (:documentation "Object to hold our list of numbers"))

(defun tx-create-numbers-root (system)
  "Transaction function to create a numbers instance as a root object"
  (setf (get-root-object system :numbers) (make-instance 'numbers)))

(defun tx-add-number (system number)
  "Transaction function to add a number to the numbers list"
  (let ((numbers (get-root-object system :numbers)))
    (push number (get-numbers-list numbers))))

(defparameter *system-location* (pathname "/tmp/demo1-prevalence-system/")
  "Filesystem location of the prevalence system")

(defun demo1 ()
  "Run the demo1 loop, computing primes and making the list of primes found persistent"
  (let ((system (make-prevalence-system *system-location*)))
    (unwind-protect
	(let* ((numbers (or (get-root-object system :numbers)
			    (execute system (make-transaction 'tx-create-numbers-root))))
	       (numbers-list (get-numbers-list numbers))
	       (candidate (if numbers-list (1+ (first numbers-list)) 0))
	       (largest 0))
	  (loop
	   (when (> candidate (min most-positive-fixnum 16777215))
	     (return))
	   (when (prime-p candidate)
	     (execute system (make-transaction 'tx-add-number candidate))
	     (setf largest candidate)
	     (format t "Primes found: ~d. Largest: ~d~%" (length (get-numbers-list numbers)) largest))
	   (incf candidate)))
      (close-open-streams system))))

(defun benchmark1 ()
  (let (system)
    (setf system (make-prevalence-system *system-location*))
    (totally-destroy system)
    (execute system (make-transaction 'tx-create-numbers-root))
    (time (dotimes (i 10000) (execute system (make-transaction 'tx-add-number i))))
    (close-open-streams system)
    (setf system (time (make-prevalence-system *system-location*)))
    (close-open-streams system)))

(defun benchmark2 ()
  (let (system)
    (setf system (make-prevalence-system *system-location* 
                                         :init-args '(:serializer serialize-sexp 
                                                      :deserializer deserialize-sexp 
                                                      :file-extension "sexp")))
    (totally-destroy system)
    (execute system (make-transaction 'tx-create-numbers-root))
    (time (dotimes (i 10000) (execute system (make-transaction 'tx-add-number i))))
    (close-open-streams system)
    (setf system (time (make-prevalence-system *system-location*)))
    (close-open-streams system)))

;;;; eof
