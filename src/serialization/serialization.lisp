;;;; -*- mode: Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; XML and S-Expression based Serialization for Common Lisp and CLOS
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-serialization)


;;; PUBLIC API

(defgeneric serializable-slots (object)
  (:documentation "Return a list of slot names that need serialization"))

(defgeneric reset-known-slots (serialization-state &optional class)
  (:documentation "Clear the caching of known slots for class, or for all classes if class is nil"))

(defun make-serialization-state ()
  "Create a reusable serialization state to pass as optional argument to [de]serialize-xml"
  (make-instance 'serialization-state))


;;;; IMPLEMENTATION

;;; state
(defclass serialization-state ()
  ((xml-parser-state :initform nil)
   (counter :accessor get-counter :initform 0)
   (hashtable :reader get-hashtable :initform (make-hash-table :test 'eq :size 1024 :rehash-size 2.0))
   (known-slots :initform (make-hash-table))))

(defmethod get-xml-parser-state ((serialization-state serialization-state))
  (with-slots (xml-parser-state) serialization-state
    (or xml-parser-state
        (setf xml-parser-state (make-instance 's-xml:xml-parser-state
					      :new-element-hook #'deserialize-xml-new-element
					      :finish-element-hook #'deserialize-xml-finish-element
					      :text-hook #'deserialize-xml-text)))))

(defmethod reset ((serialization-state serialization-state))
  (with-slots (hashtable counter) serialization-state
    (clrhash hashtable)
    (setf counter 0)))

(defmethod reset-known-slots ((serialization-state serialization-state) &optional class)
  (with-slots (known-slots) serialization-state
    (if class
        (remhash (if (symbolp class) class (class-name class)) known-slots)
      (clrhash known-slots))))

(defmethod known-object-id ((serialization-state serialization-state) object)
  (gethash object (get-hashtable serialization-state)))

(defmethod set-known-object ((serialization-state serialization-state) object)
  (setf (gethash object (get-hashtable serialization-state))
        (incf (get-counter serialization-state))))

;;; shared utilities

;; when printing symbols we always add the package and treat the symbol as internal
;; so that the serialization is independent of future change in export status 
;; we handling symbols in the common-lisp and keyword package more efficiently
;; some hacking to handle unprintable symbols is involved

(defconstant +cl-package+ (find-package :cl))

(defconstant +keyword-package+ (find-package :keyword))

(defmethod serializable-slots ((object structure-object))
  #+openmcl
  (let* ((sd (gethash (class-name (class-of object)) ccl::%defstructs%))
	 (slots (if sd (ccl::sd-slots sd))))
    (mapcar #'car (if (symbolp (caar slots)) slots (cdr slots))))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object)))
  #+lispworks
  (structure:structure-class-slot-names (class-of object))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #+clisp
  (mapcar #'clos:slot-definition-name (ext:structure-slots (type-of object)))
  #-(or openmcl cmu lispworks allegro sbcl clisp)
  (error "not yet implemented"))

(defmethod serializable-slots ((object standard-object))
  #+openmcl
  (mapcar #'ccl:slot-definition-name
	  (#-openmcl-native-threads ccl:class-instance-slots
	   #+openmcl-native-threads ccl:class-slots
	   (class-of object)))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object)))
  #+lispworks
  (mapcar #'hcl:slot-definition-name (hcl:class-slots (class-of object)))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #+clisp
  (mapcar #'clos:slot-definition-name (clos:class-slots (class-of object)))
  #-(or openmcl cmu lispworks allegro sbcl clisp)
  (error "not yet implemented"))

(defmethod get-serializable-slots ((serialization-state serialization-state) object)
  (with-slots (known-slots) serialization-state
    (let* ((class (class-name (class-of object)))
	   (slots (gethash class known-slots)))
      (when (not slots)
	(setf slots (serializable-slots object))
	(setf (gethash class known-slots) slots))
      slots)))

(defun sequence-type-and-length (sequence)
  (if (listp sequence)
      (handler-case
          (let ((length (list-length sequence)))
            (if length
                (values :proper-list length)
                (values :circular-list nil)))
        (type-error ()
          (values :dotted-list nil)))
      (values :proper-sequence (length sequence))))

