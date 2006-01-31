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

;;; Public API

(defgeneric serializable-slots (object)
  (:documentation "Return a list of slot names that need serialization"))

(defun serialize-xml (object stream &optional (serialization-state (make-serialization-state)))
  "Write a serialized version of object to stream using XML, optionally reusing a serialization-state"
  (reset serialization-state)
  (serialize-xml-internal object stream serialization-state))

(defun serialize-sexp (object stream &optional (serialization-state (make-serialization-state)))
  "Write a serialized version of object to stream using s-expressions, optionally reusing a serialization-state"
  (reset serialization-state)
  (serialize-sexp-internal object stream serialization-state))

(defgeneric serialize-xml-internal (object stream serialization-state)
  (:documentation "Write a serialized version of object to stream using XML"))

(defgeneric serialize-sexp-internal (object stream serialization-state)
  (:documentation "Write a serialized version of object to stream using s-expressions"))

(defun deserialize-xml (stream &optional (serialization-state (make-serialization-state)))
  "Read and return an XML serialized version of a lisp object from stream, optionally reusing a serialization state"
  (reset serialization-state)
  (let ((*deserialized-objects* (get-hashtable serialization-state)))
    (declare (special *deserialized-objects*))
    (car (s-xml:start-parse-xml stream (get-xml-parser-state serialization-state)))))

(defun deserialize-sexp (stream &optional (serialization-state (make-serialization-state)))
  "Read and return an s-expression serialized version of a lisp object from stream, optionally reusing a serialization state"
  (reset serialization-state)
  (let ((sexp (read stream nil :eof)))
    (if (eq sexp :eof) 
        nil
      (deserialize-sexp-internal sexp (get-hashtable serialization-state)))))

(defun make-serialization-state ()
  "Create a reusable serialization state to pass as optional argument to [de]serialize-xml"
  (make-instance 'serialization-state))

(defgeneric reset-known-slots (serialization-state &optional class)
  (:documentation "Clear the caching of known slots for class, or for all classes if class is nil"))

;;; Implementation

;; State and Support

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

;; when printing symbols we always add the package and treat the symbol as internal
;; so that the serialization is independent of future change in export status 
;; we handling symbols in the common-lisp and keyword package more efficiently
;; some hacking to handle unprintable symbols is involved

(defconstant +cl-package+ (find-package :cl))

(defconstant +keyword-package+ (find-package :keyword))

(defun print-symbol-xml (symbol stream)
  (let ((package (symbol-package symbol))
	(name (prin1-to-string symbol)))
    (cond ((eq package +cl-package+) (write-string "CL:" stream))
	  ((eq package +keyword-package+) (write-char #\: stream))
	  (t (s-xml:print-string-xml (package-name package) stream)
	     (write-string "::" stream)))
    (if (char= (char name (1- (length name))) #\|)
        (s-xml:print-string-xml name stream :start (position #\| name))
      (s-xml:print-string-xml name stream :start (1+ (or (position #\: name :from-end t) -1))))))

(defun print-symbol (symbol stream)
  (let ((package (symbol-package symbol))
	(name (prin1-to-string symbol)))
    (cond ((eq package +cl-package+) (write-string "CL:" stream))
	  ((eq package +keyword-package+) (write-char #\: stream))
	  (t (s-xml:print-string-xml (package-name package) stream)
	     (write-string "::" stream)))
    (if (char= (char name (1- (length name))) #\|)
        (write-string name stream :start (position #\| name))
      (write-string name stream :start (1+ (or (position #\: name :from-end t) -1))))))

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
  #-(or openmcl cmu lispworks allegro sbcl)
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
  #-(or openmcl cmu lispworks allegro sbcl)
  (error "not yet implemented"))

(defmethod get-serializable-slots ((serialization-state serialization-state) object)
  (with-slots (known-slots) serialization-state
    (let* ((class (class-name (class-of object)))
	   (slots (gethash class known-slots)))
      (when (not slots)
	(setf slots (serializable-slots object))
	(setf (gethash class known-slots) slots))
      slots)))

;; Serializers

(defmethod serialize-xml-internal ((object integer) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "<INT>" stream)
  (prin1 object stream)
  (write-string "</INT>" stream))

(defmethod serialize-xml-internal ((object ratio) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "<RATIO>" stream)
  (prin1 object stream)
  (write-string "</RATIO>" stream))

(defmethod serialize-xml-internal ((object float) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "<FLOAT>" stream)
  (prin1 object stream)
  (write-string "</FLOAT>" stream))

(defmethod serialize-xml-internal ((object complex) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "<COMPLEX>" stream)
  (prin1 object stream)
  (write-string "</COMPLEX>" stream))

(defmethod serialize-sexp-internal ((object number) stream serialize-sexp-internal)
  (declare (ignore serialize-sexp-internal))
  (prin1 object stream))

(defmethod serialize-xml-internal ((object null) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "<NULL/>" stream))

(defmethod serialize-xml-internal ((object (eql 't)) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "<TRUE/>" stream))

(defmethod serialize-xml-internal ((object string) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "<STRING>" stream)
  (s-xml:print-string-xml object stream)
  (write-string "</STRING>" stream))

(defmethod serialize-xml-internal ((object character) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "<CHARACTER>" stream)
  (s-xml:print-string-xml (princ-to-string object) stream)
  (write-string "</CHARACTER>" stream))

(defmethod serialize-xml-internal ((object symbol) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "<SYMBOL>" stream)
  (print-symbol-xml object stream)
  (write-string "</SYMBOL>" stream))

(defmethod serialize-sexp-internal ((object null) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "NIL" stream))

(defmethod serialize-sexp-internal ((object (eql 't)) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "T" stream))

(defmethod serialize-sexp-internal ((object string) stream serialization-state)
  (declare (ignore serialization-state))
  (prin1 object stream))

(defmethod serialize-sexp-internal ((object character) stream serialization-state)
  (declare (ignore serialization-state))
  (prin1 object stream))

(defmethod serialize-sexp-internal ((object symbol) stream serialization-state)
  (declare (ignore serialization-state))
  (print-symbol object stream))

(defun sequence-type-and-length(sequence)
  (if (listp sequence)
      (handler-case
          (let ((length (list-length sequence)))
            (if length
                (values :proper-list length)
                (values :circular-list nil)))
        (type-error ()
          (values :dotted-list nil)))
      (values :proper-sequence (length sequence))))

(defmethod serialize-xml-internal ((object sequence) stream serialization-state)
  (flet ((proper-sequence (length)
           (let ((id (set-known-object serialization-state object)))
             (write-string "<SEQUENCE ID=\"" stream)
             (prin1 id stream)
             (write-string "\" CLASS=\"" stream)
             (print-symbol-xml (etypecase object (list 'list) (vector 'vector)) stream)
             (write-string "\" SIZE=\"" stream)
             (prin1 length stream)
             (write-string "\">" stream)
             (map nil
                  #'(lambda (element)
                      (serialize-xml-internal element stream serialization-state))
                  object)
             (write-string "</SEQUENCE>" stream)))
         (improper-list ()
           (let ((id (set-known-object serialization-state object)))
             (write-string "<CONS ID=\"" stream)        
             (prin1 id stream)
             (write-string "\">" stream)        
             (serialize-xml-internal (car object) stream serialization-state)
             (write-char #\Space stream)                
             (serialize-xml-internal (cdr object) stream serialization-state)
             (write-string "</CONS>" stream))))
    (let ((id (known-object-id serialization-state object)))
      (if id
          (progn
            (write-string "<REF ID=\"" stream)
            (prin1 id stream)
            (write-string "\"/>" stream))          
          (multiple-value-bind (seq-type length) (sequence-type-and-length object)
            (ecase seq-type
              ((:proper-sequence :proper-list) (proper-sequence length))
              ((:dotted-list :circular-list) (improper-list))))))))

(defmethod serialize-sexp-internal ((object sequence) stream serialization-state)
  (flet ((proper-sequence (length)
           (let ((id (set-known-object serialization-state object)))
             (write-string "(:SEQUENCE " stream)
             (prin1 id stream)
             (write-string " :CLASS " stream)
             (print-symbol (etypecase object (list 'list) (vector 'vector)) stream)
             (write-string " :SIZE " stream)
             (prin1 length stream)
             (unless (zerop length)
               (write-string " :ELEMENTS (" stream)
               (map nil
                    #'(lambda (element) 
                        (write-string " " stream)
                        (serialize-sexp-internal element stream serialization-state))
                    object))
             (write-string " ) )" stream)))
         (improper-list ()           
           (let ((id (set-known-object serialization-state object)))
             (write-string "(:CONS " stream)
             (prin1 id stream)
             (write-char #\Space stream)        
             (serialize-sexp-internal (car object) stream serialization-state)
             (write-char #\Space stream)                
             (serialize-sexp-internal (cdr object) stream serialization-state)
             (write-string " ) " stream))))
    (let ((id (known-object-id serialization-state object)))
      (if id
          (progn
            (write-string "(:REF . " stream)
            (prin1 id stream)
            (write-string ")" stream))
          (multiple-value-bind (seq-type length) (sequence-type-and-length object)
            (ecase seq-type
              ((:proper-sequence :proper-list) (proper-sequence length))
              ((:dotted-list :circular-list) (improper-list))))))))

(defmethod serialize-xml-internal ((object hash-table) stream serialization-state)
  (let ((id (known-object-id serialization-state object)))
    (if id
	(progn
	  (write-string "<REF ID=\"" stream)
	  (prin1 id stream)
	  (write-string "\"/>" stream))
        (progn
          (setf id (set-known-object serialization-state object))
          (write-string "<HASH-TABLE ID=\"" stream)
          (prin1 id stream)
          (write-string "\" TEST=\"" stream)
          (print-symbol-xml (hash-table-test object) stream)
          (write-string "\" SIZE=\"" stream)
          (prin1 (hash-table-size object) stream)
          (write-string "\">" stream)
          (maphash #'(lambda (key value)
                       (write-string "<ENTRY><KEY>" stream)
                       (serialize-xml-internal key stream serialization-state)
                       (write-string "</KEY><VALUE>" stream)
                       (serialize-xml-internal value stream serialization-state)
                       (princ "</VALUE></ENTRY>" stream))
                   object)
          (write-string "</HASH-TABLE>" stream)))))

(defmethod serialize-sexp-internal ((object hash-table) stream serialization-state)
  (let ((id (known-object-id serialization-state object)))
    (if id
	(progn
	  (write-string "(:REF . " stream)
	  (prin1 id stream)
	  (write-string ")" stream))
        (let ((count (hash-table-count object)))
          (setf id (set-known-object serialization-state object))
          (write-string "(:HASH-TABLE " stream)
          (prin1 id stream)
          (write-string " :TEST " stream)
          (print-symbol (hash-table-test object) stream)
          (write-string " :SIZE " stream)
          (prin1 (hash-table-size object) stream)
          (write-string " :REHASH-SIZE " stream)
          (prin1 (hash-table-rehash-size object) stream)
          (write-string " :REHASH-THRESHOLD " stream)
          (prin1 (hash-table-rehash-threshold object) stream)
          (unless (zerop count)
            (write-string " :ENTRIES (" stream)
            (maphash #'(lambda (key value)
                         (write-string " (" stream)
                         (serialize-sexp-internal key stream serialization-state)
                         (write-string " . " stream)
                         (serialize-sexp-internal value stream serialization-state)
                         (princ ")" stream))
                     object))
          (write-string " ) )" stream)))))

(defmethod serialize-xml-internal ((object structure-object) stream serialization-state)
  (let ((id (known-object-id serialization-state object)))
    (if id
	(progn
	  (write-string "<REF ID=\"" stream)
	  (prin1 id stream)
	  (write-string "\"/>" stream))
      (progn
	(setf id (set-known-object serialization-state object))
	(write-string "<STRUCT ID=\"" stream)
	(prin1 id stream)
	(write-string "\" CLASS=\"" stream)
	(print-symbol-xml (class-name (class-of object)) stream)
	(write-string "\">" stream)
	(mapc #'(lambda (slot)
		  (write-string "<SLOT NAME=\"" stream)
		  (print-symbol-xml slot stream)
		  (write-string "\">" stream)
		  (serialize-xml-internal (slot-value object slot) stream serialization-state)
		  (write-string "</SLOT>" stream))
	      (get-serializable-slots serialization-state object))
	(write-string "</STRUCT>" stream)))))

(defmethod serialize-sexp-internal ((object structure-object) stream serialization-state)
  (let ((id (known-object-id serialization-state object)))
    (if id
	(progn
	  (write-string "(:REF . " stream)
	  (prin1 id stream)
	  (write-string ")" stream))
      (let ((serializable-slots (get-serializable-slots serialization-state object)))
	(setf id (set-known-object serialization-state object))
	(write-string "(:STRUCT " stream)
	(prin1 id stream)
	(write-string " :CLASS " stream)
	(print-symbol (class-name (class-of object)) stream)
        (when serializable-slots
          (write-string " :SLOTS (" stream)
          (mapc #'(lambda (slot)
                    (write-string " (" stream)
                    (print-symbol slot stream)
                    (write-string " . " stream)
                    (serialize-sexp-internal (slot-value object slot) stream serialization-state)
                    (write-string ")" stream))
                serializable-slots))
	(write-string " ) )" stream)))))

(defmethod serialize-xml-internal ((object standard-object) stream serialization-state)
  (let ((id (known-object-id serialization-state object)))
    (if id
	(progn
	  (write-string "<REF ID=\"" stream)
	  (prin1 id stream)
	  (write-string "\"/>" stream))
      (progn
	(setf id (set-known-object serialization-state object))
	(write-string "<OBJECT ID=\"" stream)
	(prin1 id stream)
	(write-string "\" CLASS=\"" stream)
	(print-symbol-xml (class-name (class-of object)) stream)
	(princ "\">" stream)
	(loop :for slot :in (get-serializable-slots serialization-state object)
              :do (when (slot-boundp object slot)
                    (write-string "<SLOT NAME=\"" stream)
                    (print-symbol-xml slot stream)
                    (write-string "\">" stream)
                    (serialize-xml-internal (slot-value object slot) stream serialization-state)
                    (write-string "</SLOT>" stream)))
	(write-string "</OBJECT>" stream)))))

(defmethod serialize-sexp-internal ((object standard-object) stream serialization-state)
  (let ((id (known-object-id serialization-state object)))
    (if id
	(progn
	  (write-string "(:REF . " stream)
	  (prin1 id stream)
	  (write-string ")" stream))
      (let ((serializable-slots (get-serializable-slots serialization-state object)))
	(setf id (set-known-object serialization-state object))
	(write-string "(:OBJECT " stream)
	(prin1 id stream)
	(write-string " :CLASS " stream)
	(print-symbol (class-name (class-of object)) stream)
        (when serializable-slots
          (princ " :SLOTS (" stream)
          (loop :for slot :in serializable-slots
                :do (when (slot-boundp object slot)
                      (write-string " (" stream)
                      (print-symbol slot stream)
                      (write-string " . " stream)
                      (serialize-sexp-internal (slot-value object slot) stream serialization-state)
                      (write-string ")" stream))))
	(write-string " ) )" stream)))))

;;; Deserialize CLOS instances and Lisp primitives from the XML representation

(defun get-attribute-value (name attributes)
  (cdr (assoc name attributes :test #'eq)))

(defun deserialize-xml-new-element (name attributes seed)
  (declare (ignore seed) (special *deserialized-objects*))
  (case name
    (:sequence (let ((id (parse-integer (get-attribute-value :id attributes)))
		     (class (read-from-string (get-attribute-value :class attributes)))
		     (size (parse-integer (get-attribute-value :size attributes))))
		 (setf (gethash id *deserialized-objects*)
		       (make-sequence class size))))
    (:object (let ((id (parse-integer (get-attribute-value :id attributes)))
		   (class (read-from-string (get-attribute-value :class attributes))))
	       (setf (gethash id *deserialized-objects*)
		     (make-instance class))))
    (:cons (setf (gethash (parse-integer (get-attribute-value :id attributes))
                          *deserialized-objects*)
                 (cons nil nil)))
    (:struct (let ((id (parse-integer (get-attribute-value :id attributes)))
		   (class (read-from-string (get-attribute-value :class attributes))))
	       (setf (gethash id *deserialized-objects*)
		     (funcall (intern (concatenate 'string "MAKE-" (symbol-name class)) (symbol-package class))))))
    (:hash-table (let ((id (parse-integer (get-attribute-value :id attributes)))
		       (test (read-from-string (get-attribute-value :test attributes)))
		       (size (parse-integer (get-attribute-value :size attributes))))
		   (setf (gethash id *deserialized-objects*)
			 (make-hash-table :test test :size size)))))
  '())

(defun deserialize-xml-finish-element (name attributes parent-seed seed)
  (declare (special *deserialized-objects*))
  (cons (case name
	  (:int (parse-integer seed))
	  ((:float :ratio :complex :symbol) (read-from-string seed))
	  (:null nil)
	  (:true t)
	  (:string (or seed ""))
          (:character (char seed 0))
	  (:key (car seed))
	  (:value (car seed))
	  (:entry (nreverse seed))
	  (:slot (let ((name (read-from-string (get-attribute-value :name attributes))))
		   (cons name (car seed))))
	  (:sequence (let* ((id (parse-integer (get-attribute-value :id attributes)))
			    (sequence (gethash id *deserialized-objects*)))
		       (map-into sequence #'identity (nreverse seed))))
          (:cons (let* ((id (parse-integer (get-attribute-value :id attributes)))
                        (cons-pair (gethash id *deserialized-objects*)))
                   (rplaca cons-pair (second seed))
                   (rplacd cons-pair (first seed))))
          (:object (let* ((id (parse-integer (get-attribute-value :id attributes)))
                          (object (gethash id *deserialized-objects*)))
                     (dolist (pair seed object)
                       (when (slot-exists-p object (car pair))
                         (setf (slot-value object (car pair)) (cdr pair))))))
          (:struct (let* ((id (parse-integer (get-attribute-value :id attributes)))
                          (object (gethash id *deserialized-objects*)))
                     (dolist (pair seed object)
                       (when (slot-exists-p object (car pair))
                         (setf (slot-value object (car pair)) (cdr pair))))))
          (:hash-table (let* ((id (parse-integer (get-attribute-value :id attributes)))
                              (hash-table (gethash id *deserialized-objects*)))
                         (dolist (pair seed hash-table)
                           (setf (gethash (car pair) hash-table) (cadr pair)))))
          (:ref (let ((id (parse-integer (get-attribute-value :id attributes))))
                  (gethash id *deserialized-objects*))))
        parent-seed))

(defun deserialize-xml-text (string seed)
  (declare (ignore seed))
  string)

(defun deserialize-sexp-internal (sexp deserialized-objects)
  (if (atom sexp) 
      sexp
      (ecase (first sexp)
        (:sequence (destructuring-bind (id &key class size elements) (rest sexp)
                     (let ((sequence (make-sequence class size)))
                       (setf (gethash id deserialized-objects) sequence)
                       (map-into sequence 
                                 #'(lambda (x) (deserialize-sexp-internal x deserialized-objects)) 
                                 elements))))
        (:hash-table (destructuring-bind (id &key test size rehash-size rehash-threshold entries) (rest sexp)
                       (let ((hash-table (make-hash-table :size size 
                                                          :test test 
                                                          :rehash-size rehash-size 
                                                          :rehash-threshold rehash-threshold)))
                         (setf (gethash id deserialized-objects) hash-table)
                         (dolist (entry entries)
                           (setf (gethash (deserialize-sexp-internal (first entry) deserialized-objects) hash-table)
                                 (deserialize-sexp-internal (rest entry) deserialized-objects)))
                         hash-table)))
        (:object (destructuring-bind (id &key class slots) (rest sexp)
                   (let ((object (make-instance class)))
                     (setf (gethash id deserialized-objects) object)
                     (dolist (slot slots)
                       (when (slot-exists-p object (first slot))
                         (setf (slot-value object (first slot)) 
                               (deserialize-sexp-internal (rest slot) deserialized-objects))))
                     object)))
        (:struct (destructuring-bind (id &key class slots) (rest sexp)
                   (let ((object (funcall (intern (concatenate 'string "MAKE-" (symbol-name class)) 
                                                  (symbol-package class)))))
                     (setf (gethash id deserialized-objects) object)
                     (dolist (slot slots)
                       (when (slot-exists-p object (first slot))
                         (setf (slot-value object (first slot)) 
                               (deserialize-sexp-internal (rest slot) deserialized-objects))))
                     object)))
        (:cons (destructuring-bind (id cons-car cons-cdr) (rest sexp)
                 (let ((conspair (cons nil nil)))
                   (setf (gethash id deserialized-objects)
                         conspair)                   
                   (rplaca conspair (deserialize-sexp-internal cons-car deserialized-objects))
                   (rplacd conspair (deserialize-sexp-internal cons-cdr deserialized-objects)))))
        (:ref (gethash (rest sexp) deserialized-objects)))))

;;;; eof
