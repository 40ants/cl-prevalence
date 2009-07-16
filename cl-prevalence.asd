;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id$
;;;;
;;;; The CL-PREVALENCE ASDF system definition
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

(defsystem :cl-prevalence
  :name "CL-PREVALENCE"
  :author "Sven Van Caekenberghe <svc@mac.com>"
  :version "5"
  :maintainer "Sven Van Caekenberghe <svc@mac.com>, Leslie P. Polzer <polzer@gnu.org>"
  :licence "Lesser Lisp General Public License"
  :description "Common Lisp Prevalence Package"
  :long-description "Common Lisp Prevalence is an implementation of Object Prevalence for Common Lisp"

  :components 
  ((:module "src"
      :components ((:file "package")
                   (:module "serialization"
                      :components ((:file "serialization")
                                   (:file "xml" :depends-on ("serialization"))
                                   (:file "sexp" :depends-on ("serialization")))
                      :depends-on ("package"))
                   (:file "prevalence" :depends-on ("serialization"))
                   (:file "managed-prevalence" :depends-on ("prevalence"))
                   (:file "master-slave" :depends-on ("prevalence"))
                   (:file "blob" :depends-on ("managed-prevalence")))))
  :depends-on (:s-xml :s-sysdeps))

