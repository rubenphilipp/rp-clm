;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* system
;;; NAME
;;; system
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-09
;;; 
;;; PURPOSE
;;; System definition for rp-clm. 
;;;
;;;
;;; $$ Last modified:  00:19:39 Sun Mar 10 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "rp-clm"
  :description "A collection of Common Lisp generators and instruments for CLM."
  :version "0.0.1"
  :author "Ruben Philipp <me@rubenphilipp.com>"
  :license "GPL Version 2.0 or later"
  :serial t
  :in-order-to ((test-op (test-op "rp-clm/tests")))
  ;;:depends-on ("clm")
  :pathname "src/"
  :components ((:file "package")
               (:file "filters")
               ;; this must be loaded at the end of this list
               (:file "export")))


;;; regression tests
(defsystem "rp-clm/tests"
  :description "Test suite for rp-clm."
  :author "Ruben Philipp <me@rubenphilipp.com>"
  :license "GPL Version 2.0 or later"
  :depends-on ("rp-clm"
               "fiveam")
  :pathname "tests/"
  :perform (test-op (o c) (symbol-call :rp-clm.tests :run-tests))
  :components ((:file "tests")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF rp-clm.asd
