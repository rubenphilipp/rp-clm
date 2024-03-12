;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* rp-clm/compile
;;; NAME
;;; compile
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-12
;;; 
;;; PURPOSE
;;; This module is used to compile parts of the code in the "clm-way". 
;;;
;;;
;;; $$ Last modified:  18:10:04 Tue Mar 12 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rp-clm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This macro compiles and loads clm-instrument files and stores the compiled
;;; output in the asdf binary directory
;;; Indeed, this is somewhat dirty and should be improved.
;;; RP  Tue Mar 12 18:04:52 2024

(defmacro compile-and-load (name &key (src-ext ".ins"))
  `(let* ((filepath-sans-ext
            (namestring
             (asdf::system-relative-pathname "rp-clm" (concatenate 'string
                                                                   "src/"
                                                                   ,name)))))
     (compile-file (concatenate 'string filepath-sans-ext ,src-ext)
                   :output-file (asdf::apply-output-translations
                                 filepath-sans-ext))
     (load (asdf::apply-output-translations filepath-sans-ext))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compile and load the files (e.g. clm-instruments):

(compile-and-load "analysis")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF compile.lisp
