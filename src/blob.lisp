;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id$
;;;;
;;;; Blobs represent collections of bytes of a certain mime type,
;;;; where the bytes themselves are stored automatically in an ordinary file.
;;;; This helps to save memory for large collections of binary data.
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :cl-prevalence)

(defclass blob (object-with-id)
  ((name :accessor get-name :initarg :name :initform "untitled")
   (size :reader get-size :initarg :size :initform -1)
   (mime-type :accessor get-mime-type :initarg :mime-type :initform "application/octet-stream")
   (keywords :accessor get-keywords :initarg :keywords :initform '()))
  (:documentation "A blob is a file-like collection of bytes with related metadata"))

(defgeneric get-name (blob)
  (:documentation "Return the descriptive name of blob"))

(defgeneric (setf get-name) (name blob)
  (:documentation "Set the descriptive name of blob"))

(defgeneric get-size (blob)
  (:documentation "Return the size of blob in bytes"))

(defgeneric get-mime-type (blob)
  (:documentation "Return the mime-type of blob as a string"))

(defgeneric (setf get-mime-type) (mime-type blob)
  (:documentation "Set the mime-type string of blob"))

(defgeneric get-keywords (blob)
  (:documentation "Return the list of keywords associated with blob"))

(defgeneric (setf get-keywords) (keywords blob)
  (:documentation "Set the keywords list of blob"))

(defmethod print-object ((blob blob) stream)
  (print-unreadable-object
   (blob stream :type t :identity t)
   (with-slots (id name mime-type) blob
   (format stream "#~d \"~a\" ~a" id name mime-type))))

(defvar *blob-root* nil
  "The directory in which to store the blob files")

(defgeneric get-file (blob)
  (:documentation "Return the pathname to the bytes of blob"))

(defmethod get-file ((blob blob))
  (merge-pathnames (princ-to-string (get-id blob)) *blob-root*))

(defun copy-stream (in out &optional (element-type '(unsigned-byte 8)))
  "Copy everything from in to out"
  (let* ((buffer-size 4096)
	 (buffer (make-array buffer-size :element-type element-type)))
    (labels ((read-chunks ()
			  (let ((size (read-sequence buffer in)))
			    (if (< size buffer-size)
				(write-sequence buffer out :start 0 :end size)
			      (progn
				(write-sequence buffer out)
				(read-chunks))))))
      (read-chunks))))

(defgeneric fill-from-stream (blob binary-input-stream)
  (:documentation "Fill the blob's contents with the bytes from binary-input-stream"))

(defmethod fill-from-stream ((blob blob) binary-input-stream)
  "Fill the blob's contents with the bytes from binary-input-stream"
  (with-open-file (out (get-file blob)
		       :direction :output
		       :element-type '(unsigned-byte 8)
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (copy-stream binary-input-stream out)))

(defgeneric copy-to-stream (blob binary-output-stream)
  (:documentation "Copy the bytes from blob to binary-output-stream"))

(defmethod copy-to-stream ((blob blob) binary-output-stream)
  "Copy the bytes from blob to binary-output-stream"
  (with-open-file (in (get-file blob)
		       :direction :input
		       :element-type '(unsigned-byte 8))
    (copy-stream in binary-output-stream)))

(defgeneric fill-from-file (blob pathname)
  (:documentation "Fill the blob's contents with the bytes read from the binary file at pathname"))

(defmethod fill-from-file ((blob blob) pathname)
  "Fill the blob's contents with the bytes read from the binary file at pathname"
  (with-open-file (in pathname :direction :input :element-type '(unsigned-byte 8))
    (fill-from-stream blob in))
  (push (format nil "~a.~a"
                (or (pathname-name pathname) "")
                (or (pathname-type pathname) "")) 
        (get-keywords blob)))

(defgeneric destroy (blob)
  (:documentation "Completely destroy blob (removing its byte data file as well)"))

(defmethod destroy ((blob blob))
  (when (probe-file (get-file blob))
    (delete-file (get-file blob)))
  (push :destroyed (get-keywords blob)))

(defmethod size-from-file ((blob blob))
  (let ((path (get-file blob)))
    (if (probe-file path)
	(with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
	  (file-length in))
      -1)))

(defmethod set-size-from-file ((blob blob))
  (with-slots (size) blob
    (setf size (size-from-file blob))))

(defmethod get-size :before ((blob blob))
  (with-slots (size) blob
    (when (eql size -1)
      (setf size (size-from-file blob)))))
      
;;;; eof
