(defsystem "cl-prevalence-test"
  :name "CL-PREVALENCE-TEST"
  :author "Sven Van Caekenberghe <svc@mac.com>;Plato Wu <wangyi000@yeah.net>"
  :version "2"
  :maintainer "Alexander Artemenko <svetlyak.40wt@gmail.coms>"
  :licence "Lesser Lisp General Public License"
  :description "Common Lisp Prevalence Test Package"
  :long-description "5am test suite for cl-prevalence"
  :depends-on ("cl-prevalence"
               "fiveam")
  :components 
  ((:module "test"
    :components ((:file "package")
                 (:file "test-prevalence")
                 (:file "test-managed-prevalence")
                 (:file "test-master-slave")
                 (:file "test-serialization"))))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :cl-prevalence)) )
