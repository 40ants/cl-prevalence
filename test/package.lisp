(defpackage :cl-prevalence-test
  (:use :cl :cl-prevalence :5am :s-serialization)
  (:export
   #:run!
   #:cl-prevalence-test
   )
  )
(in-package :cl-prevalence-test)

(def-suite cl-prevalence-test)


