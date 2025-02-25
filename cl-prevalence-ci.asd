(defsystem "cl-prevalence-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :licence "Lesser Lisp General Public License"
  :description "Common Lisp Prevalence Package CI Workflows"
  :long-description "Common Lisp Prevalence is an implementation of Object Prevalence for Common Lisp"
  :depends-on ("40ants-ci/workflow"
               "40ants-ci")
  :components 
  ((:module "src"
    :components ((:file "ci")))))
