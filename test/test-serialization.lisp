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

(in-package :cl-prevalence-test)

(def-suite test-serialization :in cl-prevalence-test)

(in-suite test-serialization)

(defun serialize-and-deserialize-xml (object)
  (with-input-from-string
    (in (with-output-to-string (out)
	  (serialize-xml object out)))
    (deserialize-xml in)))

(defun serialize-and-deserialize-sexp (object)
  (with-input-from-string
    (in (with-output-to-string (out)
	  (serialize-sexp object out)))
    (deserialize-sexp in)))

;; primitives

(test test-primitive-1
  (is
   (null (serialize-and-deserialize-xml nil))))

(test test-primitive-2
  (is
   (null (serialize-and-deserialize-sexp nil))))

(test test-primitive-3
  (is
   (eq (serialize-and-deserialize-xml t)
       t)))

(test test-primitive-4
  (is
   (eq (serialize-and-deserialize-sexp t)
       t)))

(test test-primitive-5
  (is
   (= (serialize-and-deserialize-xml 100)
      100)))

(test test-primitive-6
  (is
   (= (serialize-and-deserialize-sexp 100)
      100)))

(test test-primitive-7
  (is
   (= (serialize-and-deserialize-xml (/ 3))
      (/ 3))))

(test test-primitive-8
  (is
   (= (serialize-and-deserialize-sexp (/ 3))
      (/ 3))))

(test test-primitive-9
  (is
   (= (serialize-and-deserialize-xml pi)
      pi)))

(test test-primitive-10
  (is
   (= (serialize-and-deserialize-sexp pi)
      pi)))

(test test-primitive-11
  (is
   (= (serialize-and-deserialize-xml (complex 1.5 2.5))
      (complex 1.5 2.5))))

(test test-primitive-12
  (is
   (= (serialize-and-deserialize-sexp (complex 1.5 2.5))
      (complex 1.5 2.5))))

(test test-primitive-13
  (is
   (eq (serialize-and-deserialize-xml 'foo)
       'foo)))

(test test-primitive-14
  (is
   (eq (serialize-and-deserialize-sexp 'foo)
       'foo)))

(test test-primitive-15
  (is
   (eq (serialize-and-deserialize-xml :foo)
       :foo)))

(test test-primitive-16
  (is
   (eq (serialize-and-deserialize-sexp :foo)
       :foo)))

(test test-primitive-17
  (is
   (eq (serialize-and-deserialize-xml 'room)
       'room)))

(test test-primitive-18
  (is
   (eq (serialize-and-deserialize-sexp 'room)
       'room)))

(test test-primitive-19
  (is
   (eq (serialize-and-deserialize-xml '|Unprintable|)
       '|Unprintable|)))

(test test-primitive-20
  (is
   (eq (serialize-and-deserialize-sexp '|Unprintable|)
       '|Unprintable|)))

(test test-uninterned-symbol-sexp
  (let ((sym (gensym)))
    (is (equal (princ-to-string (serialize-and-deserialize-sexp sym))
               (princ-to-string sym)))))

(test test-uninterned-symbol-xml
  (let ((sym (gensym)))
    (is (equal (princ-to-string (serialize-and-deserialize-xml sym))
               (princ-to-string sym)))))

(test test-primitive-21
  (is
   (equal (serialize-and-deserialize-xml "Hello")
	  "Hello")))

(test test-primitive-22
  (is
   (equal (serialize-and-deserialize-sexp "Hello")
	  "Hello")))

(test test-primitive-23
  (is 
   (equal (serialize-and-deserialize-xml "")
	  "")))

(test test-primitive-24
  (is 
   (equal (serialize-and-deserialize-sexp "")
	  "")))

(test test-primitive-25
  (is
   (equal (serialize-and-deserialize-xml #\A)
	  #\A)))

(test test-primitive-26
  (is
   (equal (serialize-and-deserialize-sexp #\A)
	  #\A)))

(test test-primitive-27
  (is
   (equal (serialize-and-deserialize-xml #\<)
	  #\<)))

(test test-primitive-28
  (is
   (equal (serialize-and-deserialize-sexp #\<)
	  #\<)))

(test test-primitive-29
  (is
   (equal (serialize-and-deserialize-xml "Hello <foo> & </bar>!")
	  "Hello <foo> & </bar>!")))

(test test-primitive-30
  (is
   (equal (serialize-and-deserialize-sexp "Hello <foo> & </bar>!")
	  "Hello <foo> & </bar>!")))

;; simple sequences

(test test-simple-sequences-1
  (is
   (reduce #'(lambda (x &optional (y t)) (and x y))
	   (map 'list
		#'eql
		(serialize-and-deserialize-xml (list 1 2 3))
		(list 1 2 3)))))

(test test-simple-sequences-2
  (is
   (reduce #'(lambda (x &optional (y t)) (and x y))
	   (map 'list
		#'eql
		(serialize-and-deserialize-sexp (list 1 2 3))
		(list 1 2 3)))))

(test test-simple-sequences-3
  (is
   (equal (serialize-and-deserialize-xml (list 1 2 3))
	  (list 1 2 3))))

(test test-simple-sequences-4
  (is
   (equal (serialize-and-deserialize-sexp (list 1 2 3))
	  (list 1 2 3))))

(test test-simple-sequences-5
  (is
   (equal (serialize-and-deserialize-xml (cons 1 2))
	  (cons 1 2))))

(test test-simple-sequences-6
  (is
   (equal (serialize-and-deserialize-sexp (cons 1 2))
	  (cons 1 2))))

(test test-simple-sequences-7
  (is 
   (equal (serialize-and-deserialize-xml '(1 2 3 4 5 6 7 8 9 . 0))
	  '(1 2 3 4 5 6 7 8 9 . 0))))

(test test-simple-sequences-8
  (is 
   (equal (serialize-and-deserialize-sexp '(1 2 3 4 5 6 7 8 9 . 0))
	  '(1 2 3 4 5 6 7 8 9 . 0))))

(test test-simple-sequences-9
  (is
   (equal (serialize-and-deserialize-xml (cons 'hi 2))
	  (cons 'hi 2))))

(test test-simple-sequences-10
  (is
   (equal (serialize-and-deserialize-sexp (cons 'hi 2))
	  (cons 'hi 2))))

(defun circular-list (&rest elements)
   (let ((cycle (copy-list elements))) 
     (nconc cycle cycle)))

(test test-circular-list-1
  (is
   (equal (third (serialize-and-deserialize-sexp (circular-list 'a 'b)))
	  'a)))

(test test-circular-list-2
  (is
   (equal (third (serialize-and-deserialize-xml (circular-list 'a 'b)))
	  'a)))

(test test-circular-list-3
  (is
   (equal (serialize-and-deserialize-xml (cons 'hi 2))
	  (cons 'hi 2))))

(test test-circular-list-4
  (is
   (equal (serialize-and-deserialize-sexp (cons 'hi 2))
	  (cons 'hi 2))))

(test test-circular-list-5
  (is
   (equal (third (serialize-and-deserialize-sexp (circular-list 'a 'b)))
	  'a)))

(test test-circular-list-6
  (is
   (equal (third (serialize-and-deserialize-xml (circular-list 'a 'b)))
	  'a)))

;; simple objects

(defclass foobar ()
  ((foo :accessor get-foo :initarg :foo)
   (bar :accessor get-bar :initarg :bar)))

(defparameter *foobar* (make-instance 'foobar :foo 100 :bar "Bar"))

(test test-simple-objects-1
  (let ((foobar (serialize-and-deserialize-xml *foobar*)))
    (is (and (equal (get-foo foobar) (get-foo *foobar*))
	     (equal (get-bar foobar) (get-bar *foobar*))
	     (eq (class-of foobar) (class-of *foobar*))))))

(test test-simple-objects-2
  (let ((foobar (serialize-and-deserialize-sexp *foobar*)))
    (is (and (equal (get-foo foobar) (get-foo *foobar*))
	     (equal (get-bar foobar) (get-bar *foobar*))
	     (eq (class-of foobar) (class-of *foobar*))))))

;; standard structs

(defstruct foobaz
  foo
  baz)

(defparameter *foobaz* (make-foobaz :foo 100 :baz "Baz"))

(test test-standard-structs-1
  (let ((foobaz (serialize-and-deserialize-xml *foobaz*)))
    (is (and (foobaz-p foobaz)
	     (equal (foobaz-foo foobaz) (foobaz-foo *foobaz*))
	     (equal (foobaz-baz foobaz) (foobaz-baz *foobaz*))))))

(test test-standard-structs-2
  (let ((foobaz (serialize-and-deserialize-sexp *foobaz*)))
    (is (and (foobaz-p foobaz)
	     (equal (foobaz-foo foobaz) (foobaz-foo *foobaz*))
	     (equal (foobaz-baz foobaz) (foobaz-baz *foobaz*))))))

;;; hash-tables

(defparameter *hashtable* 
  (let ((hashtable (make-hash-table :test 'equal)))
    (map nil
       #'(lambda (feature) (setf (gethash (symbol-name feature) hashtable) feature))
       *features*)
    hashtable))

(test test-hash-tables-1
  (let (h2)
    (setf h2 (serialize-and-deserialize-xml *hashtable*))
    (maphash #'(lambda (k v) (is (equal v (gethash k h2)))) *hashtable*) 
    (maphash #'(lambda (k v) (is (equal v (gethash k *hashtable*)))) h2)))

(test test-hash-tables-2
  (let (h2)
    (setf h2 (serialize-and-deserialize-sexp *hashtable*))
    (maphash #'(lambda (k v) (is (equal v (gethash k h2)))) *hashtable*) 
    (maphash #'(lambda (k v) (is (equal v (gethash k *hashtable*)))) h2)))

(defparameter *empty-hashtable* (make-hash-table))

(test test-empty-hash-tables-1
  (let (h2)
    (setf h2 (serialize-and-deserialize-xml *empty-hashtable*))
    (maphash #'(lambda (k v) (is (equal v (gethash k h2)))) *empty-hashtable*) 
    (maphash #'(lambda (k v) (is (equal v (gethash k *hashtable*)))) h2)))

(test test-empty-hash-tables-2
  (let (h2)
    (setf h2 (serialize-and-deserialize-sexp *empty-hashtable*))
    (maphash #'(lambda (k v) (is (equal v (gethash k h2)))) *empty-hashtable*) 
    (maphash #'(lambda (k v) (is (equal v (gethash k *hashtable*)))) h2)))


;;; eof
