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
  :version "3"
  :maintainer "Sven Van Caekenberghe <svc@mac.com>"
  :licence "Lesser Lisp General Public License"
  :description "Common Lisp Prevalence Package"
  :long-description "Common Lisp Prevalence is an implementation of Object Prevalence for Common Lisp"

  :components 
  ((:module 
    :src
    :components
    ((:file "package")
     (:file "serialization" :depends-on ("package"))
     (:file "prevalence" :depends-on ("serialization"))
     (:file "debug-prevalence" :depends-on ("prevalence"))
     (:file "managed-prevalence" :depends-on ("prevalence"))
     (:file "sysdeps" :depends-on ("package"))
     (:file "master-slave" :depends-on ("prevalence" "sysdeps"))
     (:file "blob" :depends-on ("managed-prevalence")))))
  :depends-on (:s-xml))

;;;; eof
