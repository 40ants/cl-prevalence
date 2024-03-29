
(in-package :s-serialization)

(defun serialize-sexp (object stream &optional (serialization-state (make-serialization-state)))
  "Write a serialized version of object to stream using s-expressions, optionally reusing a serialization-state"
  (reset serialization-state)
  (serialize-sexp-internal object stream serialization-state))

(defun deserialize-sexp (stream &optional (serialization-state (make-serialization-state)))
  "Read and return an s-expression serialized version of a lisp object from stream, optionally reusing a serialization state"
  (reset serialization-state)
  (let ((sexp (read stream nil :eof)))
    (if (eq sexp :eof)
        nil
      (deserialize-sexp-internal sexp (get-hashtable serialization-state)))))

(defgeneric serialize-sexp-internal (object stream serialization-state)
  (:documentation "Write a serialized version of object to stream using s-expressions"))

(defgeneric serialize-sexp-slot (object slot-name stream serialization-state)
  (:documentation "Write a serialized version of OBJECT's SLOT-NAME to STREAM using
s-expressions."))

(defmethod serialize-sexp-slot ((object standard-object) slot-name stream serialization-state)
  (serialize-sexp-internal (slot-value object slot-name)
                           stream serialization-state))

(defun print-symbol (symbol stream)
  (let ((package (symbol-package symbol))
	(name (prin1-to-string symbol)))
    (cond ((eq package +cl-package+) (write-string "CL:" stream))
	  ((eq package +keyword-package+) (write-char #\: stream))
	  (package (s-xml:print-string-xml (package-name package) stream)
                   (write-string "::" stream))
          (t (write-string "#:" stream)))
    (if (char= (char name (1- (length name))) #\|)
        (write-string name stream :start (position #\| name))
      (write-string name stream :start (1+ (or (position #\: name :from-end t) -1))))))


;;;; SERIALIZATION

;;; basic serializers
(defmethod serialize-sexp-internal ((object null) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "NIL" stream))

(defmethod serialize-sexp-internal ((object (eql 't)) stream serialization-state)
  (declare (ignore serialization-state))
  (write-string "T" stream))

(defmethod serialize-sexp-internal ((object pathname) stream serialization-state)
  (declare (ignore serialization-state))
  (prin1 object stream))

(defmethod serialize-sexp-internal ((object string) stream serialization-state)
  (declare (ignore serialization-state))
  (prin1 object stream))

(defmethod serialize-sexp-internal ((object character) stream serialization-state)
  (declare (ignore serialization-state))
  (prin1 object stream))

(defmethod serialize-sexp-internal ((object symbol) stream serialization-state)
  (declare (ignore serialization-state))
  (print-symbol object stream))

;;; generic sequences
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

;;; hash tables
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
                     object)
            (write-string " )" stream))
          (write-string " )" stream)))))

;;; structures
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

;;; objects
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
                      (serialize-sexp-slot object slot stream serialization-state)
                      (write-string ")" stream))))
	(write-string " ) )" stream)))))


;;;; DESERIALIZATION

(defun deserialize-sexp-internal (sexp deserialized-objects)
  (if (atom sexp)
      sexp
      (ecase (first sexp)
        (:sequence (destructuring-bind (id &key class size elements) (rest sexp)
                     (cond
                       ((not class)
                        (error "Unknown sequence class"))
                       ((not size)
                        (error "Unknown sequence size"))
                       (t
                        (let ((sequence (make-sequence class size)))
                          (declare (ignorable sequence))
                          (setf (gethash id deserialized-objects) sequence)
                          (map-into sequence
                                    #'(lambda (x) (deserialize-sexp-internal x deserialized-objects))
                                  elements))))))
        (:hash-table (destructuring-bind (id &key test size rehash-size rehash-threshold entries) (rest sexp)
                       (cond
                         ((not test)
                          (error "Test function is unknown"))
                         ((not size)
                          (error "Hash table size is unknown"))
                         ((not rehash-size)
                          (error "Hash table's rehash-size is unknown"))
                         ((not rehash-threshold)
                          (error "Hash table's rehash-threshold is unknown"))
                         (t
                          (let ((hash-table (make-hash-table :size size
                                                             :test test
                                                             :rehash-size rehash-size
                                                             :rehash-threshold rehash-threshold)))
                            (setf (gethash id deserialized-objects) hash-table)
                            (dolist (entry entries)
                              (setf (gethash (deserialize-sexp-internal (first entry) deserialized-objects) hash-table)
                                    (deserialize-sexp-internal (rest entry) deserialized-objects)))
                            hash-table)))))
        (:object (destructuring-bind (id &key class slots) (rest sexp)
                   (let ((object (deserialize-class class slots deserialized-objects)))
                     (setf (gethash id deserialized-objects) object)
                     object)))
        (:struct (destructuring-bind (id &key class slots) (rest sexp)
                   (let ((object (deserialize-struct class slots deserialized-objects)))
                     (setf (gethash id deserialized-objects) object)
                     (dolist (slot slots)
                       (when (slot-exists-p object (first slot))
                         (setf (slot-value object (first slot))
                               (deserialize-sexp-slot object (first slot) (rest slot) deserialized-objects))))
                     object)))
        (:cons (destructuring-bind (id cons-car cons-cdr) (rest sexp)
                 (let ((conspair (cons nil nil)))
                   (setf (gethash id deserialized-objects)
                         conspair)
                   (rplaca conspair (deserialize-sexp-internal cons-car deserialized-objects))
                   (rplacd conspair (deserialize-sexp-internal cons-cdr deserialized-objects)))))
        (:ref (gethash (rest sexp) deserialized-objects)))))

(defgeneric deserialize-class (class-symbol slots deserialized-objects)
  (:documentation "Read and return an the instance corresponding to CLASS-SYMBOL with SLOTS.
SLOTS is a list of pairs: the first element is the slot name (a symbol) and the
second element its value."))

(defmethod deserialize-class ((class-symbol t) slots deserialized-objects)
  (let* ((no-initarg-slots '())
         (instance (apply 'make-instance
                          class-symbol
                          (apply
                           'append
                           (mapcar
                            (lambda (slot-name+value)
                              (let* ((slot-name (first slot-name+value))
                                     (slot-val (rest slot-name+value)))
                                (when (find slot-name (mopu:slot-names class-symbol))
                                  (let ((initarg (first (getf (mopu:slot-properties class-symbol slot-name) :initargs))))
                                    (if initarg
                                        (list initarg
                                              (deserialize-sexp-slot (find-class class-symbol) slot-name slot-val deserialized-objects))
                                        (progn
                                          (push slot-name+value no-initarg-slots)
                                          nil))))))
                            slots)))))
    (dolist (slot-name+value no-initarg-slots)
      (let ((slot-name (first slot-name+value))
            (slot-val (rest slot-name+value)))
        (when (slot-exists-p instance slot-name)
          (let ((writer (first (getf (mopu:slot-properties class-symbol slot-name) :writers)))
                (value (deserialize-sexp-slot instance slot-name slot-val deserialized-objects)))
            (if writer
                (funcall (fdefinition writer) value instance)
                (setf (slot-value instance slot-name) value))))))
    instance))

(defgeneric deserialize-struct (struct-symbol slots deserialized-objects)
  (:documentation "Read and return an the instance corresponding to STRUCT-SYMBOL with SLOTS."))

(defmethod deserialize-struct ((struct-symbol t) slots deserialized-objects)
  (let* ((constructor (intern (concatenate 'string "MAKE-" (symbol-name struct-symbol))
                              (symbol-package struct-symbol)))
         (object (if (fboundp constructor)
                     (funcall constructor)
                     #+(or sbcl ccl ecl clisp)
                     (make-instance struct-symbol)
                     #-(or sbcl ccl ecl clisp) ; ABCL and LispWorks don't support make-instance for structs.
                     (error "Do not know how to deserialize struct ~S with non-default constructor"
                            struct-symbol))))
    object))

(defgeneric deserialize-sexp-slot (class slot-name slot-value deserialized-objects)
  (:documentation "Read and return SLOT-VALUE, which corresponds to CLASS's SLOT-NAME."))

(defmethod deserialize-sexp-slot ((target t) slot-name slot-value deserialized-objects)
  (declare (ignore target slot-name))
  (deserialize-sexp-internal slot-value deserialized-objects))
