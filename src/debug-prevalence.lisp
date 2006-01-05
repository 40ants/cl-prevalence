;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id$
;;;;
;;;; Some debugging routines for CL-PREVALENCE
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :cl-prevalence)

;; the code for #'s-xml::echo-xml is in "echo.lisp" in S-XML's test code

(defun print-transaction-log (system)
  "Echo the XML making up the transaction log of system to t"
  (with-open-file (in (get-transaction-log system) :direction :input)
    (loop
     (let ((transaction (s-xml::echo-xml in *standard-output*)))
       (when (null transaction) (return)))))
  t)

(defun show-transaction-log (system)
  "Print the transaction objects making up the transaction log of system to t"
  (with-open-file (in (get-transaction-log system) :direction :input)
    (loop
     (let ((transaction (deserialize-xml in (get-serialization-state system))))
       (if (null transaction)
	   (return)
	 (format t "~a~%" transaction)))))
  t)

(defun print-snapshot (system)
  "Echo the XML making up the snapshot of system to t"
  (with-open-file (in (get-snapshot system) :direction :input)
    (s-xml::echo-xml in *standard-output*))
  t)

(defun transaction-log-tail (system &optional (count 8))
  "Return a list of the count last transaction objects of system"
  (let (transactions)
    (with-open-file (in (get-transaction-log system) :direction :input)
      (loop
       (let ((transaction (deserialize-xml in (get-serialization-state system))))
	 (if (null transaction)
	     (return)
	   (push transaction transactions)))))
    (setf transactions (nreverse transactions))
    (nthcdr (max 0 (- (length transactions) count)) transactions)))

;;;; eof