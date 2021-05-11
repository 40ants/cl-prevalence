(defpackage #:cl-prevalence/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests)
  (:import-from #:40ants-ci/jobs/docs))
(in-package cl-prevalence/ci)


(defworkflow ci
  :on-push-to "master"
  :on-pull-request t
  :by-cron "0 10 * * 1"
  :jobs ((40ants-ci/jobs/run-tests:run-tests
          :coverage t
          ;; :os ("ubuntu-latest"
          ;;      "macos-latest")
          ;; :quicklisp ("quicklisp"
          ;;             "ultralisp")
          ;; :lisp ("sbcl-bin"
          ;;        "ccl-bin")
          )
         (40ants-ci/jobs/linter:linter)))
