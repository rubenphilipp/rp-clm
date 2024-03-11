;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* rp-clm tests
;;; NAME
;;; rp-clm tests
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-09
;;; 
;;; PURPOSE
;;; Regression test suite for rp-clm. 
;;;
;;; CLASS HIERARCHY
;;;
;;;
;;; $$ Last modified:  23:01:43 Mon Mar 11 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :rp-clm.tests
  (:use :cl :rp-clm :fiveam)
  (:shadow :test)
  (:export :run-tests))

(in-package :rp-clm.tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite rp-clm)
(in-suite rp-clm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro test (name &body body)
  `(5am:test ,name
             ,@body))

(defmacro test-pathname (path)
  `(namestring (asdf::SYSTEM-RELATIVE-PATHNAME :rp-clm
                                               (concatenate 'string
                                                            "tests/"
                                                            ,path))))

(defun run-tests ()
  (run! 'rp-clm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS:

;;; test-biquad-bp1
;;; RP  Sat Mar  9 23:59:55 2024
(test test-biquad-bp1
  (let ((bq (make-biquad-bandpass 200 300)))
    (is (= (clm:mus-order bq) 3))))

;;; test-analyze-spectrum
;;; RP  Mon Mar 11 23:00:43 2024
(test test-analyze-spectrum
      (let* ((infile (test-pathname "noise.wav"))
             (outfile "/tmp/test-ana.csv")
             (res (analyze-spectrum infile :in-samples? nil
                                           :dur .5
                                           :outfile outfile)))
        (is (probe-file outfile))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF tests.lisp
