;;;; -*- mode: lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; The code in this file adds another layer above plain object prevalence.
;;;; We manage objects with ids in an organized fashion, adding an id counter and preferences.
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :cl-prevalence)

;; A convience macro

(defmacro execute-transaction (transaction-call)
  "Create a transaction object from transaction-call and execute it"
  `(execute ,(second transaction-call)
	    (make-transaction ',(first transaction-call) ,@(rest (rest transaction-call)))))

;; A generic object prevalence protocol handling objects with id

(defclass object-with-id ()
  ((id :reader get-id :initarg :id :initform -1))
  (:documentation "Superclass for objects with an id"))

(defgeneric get-id (object)
  (:documentation "Return an external, unique, immutable identifier for object (typically an integer)"))

(defun get-objects-root-name (class)
  "Return the keyword symbol naming the root of instances of class"
  (let ((classname (if (symbolp class) (string class) (class-name class))))
    (intern (concatenate 'string classname "-ROOT") :keyword)))

(defun get-objects-slot-index-name (class &optional (slot 'id))
  "Return the keyword symbol naming the specified index of instances of class."
  (let ((classname (if (symbolp class) (string class) (class-name class)))
        (slotname  (symbol-name slot)))
    (intern (concatenate 'string classname "-" slotname "-INDEX") :keyword)))

(defgeneric find-all-objects (system class)
  (:documentation "Return an unordered collection of all objects in system that are instances of class"))

(defmethod find-all-objects ((system prevalence-system) class)
  "Return an unordered collection of all objects in system that are instances of class"
  (let ((root-name (get-objects-root-name class)))
    (get-root-object system root-name)))

(defgeneric find-object-with-id (system class id)
  (:documentation "Find and return the object in system of class with id, null if not found"))

(defmethod find-object-with-id ((system prevalence-system) class id)
  "Find and return the object in system of class with id, null if not found"
  (let* ((index-name (get-objects-slot-index-name class 'id))
	 (index (get-root-object system index-name)))
    (when index
      (gethash id index))))

(defgeneric find-object-with-slot (system class slot value &optional test)
  (:documentation "Find and return the object in system of class with slot equal to value, null if not found"))

(defmethod find-object-with-slot ((system prevalence-system) class slot value &optional (test #'equalp))
  "Find and return the object in system of class with slot equal to value, null if not found"
  (let* ((index-name (get-objects-slot-index-name class slot))
	 (index (get-root-object system index-name)))
    (if index
        (find-object-with-id system class (gethash value index))
      (find value (find-all-objects system class) 
            :key #'(lambda (object) (slot-value object slot)) :test test))))

(defun tx-create-objects-slot-index (system class slot &optional (test #'equalp))
  "Create an index for this object on this slot, with an optional test for the hash table (add existing objects)"
  (let ((index-name (get-objects-slot-index-name class slot)))
    (unless (get-root-object system index-name)
      (let ((index (make-hash-table :test test)))
        (setf (get-root-object system index-name) index)
        (dolist (object (find-all-objects system class))
          (add-object-to-slot-index system class slot object))))))
  
(defun tx-remove-objects-slot-index (system class slot)
  "Remove an index for this object on this slot"
  (let ((index-name (get-objects-slot-index-name class slot)))
    (unless (get-root-object system index-name)
      (remove-root-object system index-name))))

(defun add-object-to-slot-index (system class slot object)
  "Add an index entry using this slot to this object"
  (let* ((index-name (get-objects-slot-index-name class slot))
	 (index (get-root-object system index-name)))
    (when (and index  (slot-boundp object slot))
      (setf (gethash (slot-value object slot) index) (get-id object)))))

(defun remove-object-from-slot-index (system class slot object)
  "Remove the index entry using this slot to this object"
  (let* ((index-name (get-objects-slot-index-name class slot))
	 (index (get-root-object system index-name)))
    (when (and index (slot-boundp object slot))
      (remhash (slot-value object slot) index))))

(defun index-on (system class &optional slots (test 'equalp))
  "Create indexes on each of the slots provided."
  (dolist (slot slots)
    (execute-transaction (tx-create-objects-slot-index system class slot test))))

(defun drop-index-on (system class &optional slots)
  "Drop indexes on each of the slots provided"
  (dolist (slot slots)
    (execute-transaction (tx-remove-objects-slot-index system class slot))))

(defun slot-value-changed-p (object slot value)
  "Return true when slot in object is not eql to value (or when the slot was unbound)"
  (or (not (slot-boundp object slot))
      (not (eql (slot-value object slot) value)))) 

(defun tx-create-object (system class &optional slots-and-values)
  "Create a new object of class in system, assigning it a unique id, optionally setting some slots and values"
  (let* ((id (next-id system))
	 (object (make-instance class :id id))
	 (index-name (get-objects-slot-index-name class 'id))
	 (index (or (get-root-object system index-name)
		    (setf (get-root-object system index-name) (make-hash-table)))))
    (push object (get-root-object system (get-objects-root-name class)))
    (setf (gethash id index) object)
    (tx-change-object-slots system class id slots-and-values)
    object))

(defun tx-delete-object (system class id)
  "Delete the object of class with id from the system"
  (let ((object (find-object-with-id system class id)))
    (if object
	(let ((root-name (get-objects-root-name class))
	      (index-name (get-objects-slot-index-name class 'id)))
	  (setf (get-root-object system root-name) (delete object (get-root-object system root-name)))
	  (remhash id (get-root-object system index-name)))
      (error "no object of class ~a with id ~d found in ~s" class id system))))

(defun tx-change-object-slots (system class id slots-and-values)
  "Change some slots of the object of class with id in system using slots and values"
  (let ((object (find-object-with-id system class id)))
    (unless object (error "no object of class ~a with id ~d found in ~s" class id system))
    (loop :for (slot value) :in slots-and-values
          :do (when (slot-value-changed-p object slot value)
                (remove-object-from-slot-index system class slot object)
                (setf (slot-value object slot) value)
                (add-object-to-slot-index system class slot object)))))
                
;; We use a simple id counter to generate unique object identifiers

(defun tx-create-id-counter (system)
  "Initialize the id counter to 0"
  (setf (get-root-object system :id-counter) 0))

(defmethod next-id ((system prevalence-system))
  "Increment and return the next id"
  (incf (get-root-object system :id-counter)))

;;; A generic persistent preferences mechanism

(defgeneric get-preference (system key)
  (:documentation "Retrieve the value of the persistent preference stored under key in system"))

(defmethod get-preference ((system prevalence-system) key)
  "Retrieve the value of the persistent preference stored under key in system"
  (let ((preferences (get-root-object system :preferences)))
    (when preferences
      (gethash key preferences)))) 

(defun tx-set-preference (system key value)
  "Set the value of the persistent preference key in system"
  (let ((preferences (get-root-object system :preferences)))
    (when (not preferences)
      (setf preferences (make-hash-table)
	    (get-root-object system :preferences) preferences))
    (setf (gethash key preferences) value)))

(defgeneric all-preferences-keys (system)
  (:documentation "Return a list of all persistent preference keys of system"))

(defmethod all-preferences-keys ((system prevalence-system))
  "Return a list of all persistent preference keys of system"
  (let ((preferences (get-root-object system :preferences)))
    (when preferences
      (let (keys)
        (maphash #'(lambda (key value)
                     (declare (ignore value))
                     (push key keys))
                 preferences)
        keys))))

;;;; eof