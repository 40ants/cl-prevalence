;;;; -*- mode: Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Testing XML and S-Expression based Serialization for Common Lisp and CLOS
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-serialization)

(defun serialize-and-deserialize (object)
  (with-input-from-string
    (in (with-output-to-string (out)
	  (serialize-xml object out)))
    (deserialize-xml in))
  (with-input-from-string
    (in (with-output-to-string (out)
	  (serialize-sexp object out)))
    (deserialize-sexp in)))

;; primitives

(assert
 (null (serialize-and-deserialize nil)))

(assert
 (eq (serialize-and-deserialize t)
     t))

(assert
 (= (serialize-and-deserialize 100)
    100))

(assert
 (= (serialize-and-deserialize (/ 3))
    (/ 3)))

(assert
 (= (serialize-and-deserialize pi)
    pi))

(assert
 (= (serialize-and-deserialize (complex 1.5 2.5))
    (complex 1.5 2.5)))

(assert
 (eq (serialize-and-deserialize 'foo)
     'foo))

(assert
 (eq (serialize-and-deserialize :foo)
     :foo))

(assert
 (eq (serialize-and-deserialize 'room)
     'room))

(assert
 (equal (serialize-and-deserialize "Hello")
	"Hello"))

(assert
 (equal (serialize-and-deserialize "Hello <foo> & </bar>!")
	"Hello <foo> & </bar>!"))

;; simple sequences

(assert
 (reduce #'(lambda (x &optional (y t)) (and x y))
	 (map 'list
	      #'eql
	      (serialize-and-deserialize (list 1 2 3))
	      (list 1 2 3))))

(assert
 (equal (serialize-and-deserialize (list 1 2 3))
	(list 1 2 3)))

;; simple objects

(defclass foobar ()
  ((foo :accessor get-foo :initarg :foo)
   (bar :accessor get-bar :initarg :bar)))

(defparameter *foobar* (make-instance 'foobar :foo 100 :bar "Bar"))

(assert
 (let ((foobar (serialize-and-deserialize *foobar*)))
   (and (equal (get-foo foobar) (get-foo *foobar*))
	(equal (get-bar foobar) (get-bar *foobar*))
	(eq (class-of foobar) (class-of *foobar*)))))

;; standard structs

(defstruct foobaz
  foo
  baz)

(defparameter *foobaz* (make-foobaz :foo 100 :baz "Baz"))

(assert
 (let ((foobaz (serialize-and-deserialize *foobaz*)))
   (and (foobaz-p foobaz)
	(equal (foobaz-foo foobaz) (foobaz-foo *foobaz*))
	(equal (foobaz-baz foobaz) (foobaz-baz *foobaz*)))))

;;; hash-tables

(defparameter *hashtable* 
  (let ((hashtable (make-hash-table :test 'equal)))
    (map nil
       #'(lambda (feature) (setf (gethash (symbol-name feature) hashtable) feature))
       *features*)
    hashtable))

(let (h2)
  (setf h2 (serialize-and-deserialize *hashtable*))
  (maphash #'(lambda (k v) (assert (equal v (gethash k h2)))) *hashtable*) 
  (maphash #'(lambda (k v) (assert (equal v (gethash k *hashtable*)))) h2))

;;; eof
