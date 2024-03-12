;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* rp-clm/utilities
;;; NAME
;;; utilities
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-12
;;; 
;;; PURPOSE
;;; Utility functions. 
;;;
;;; $$ Last modified:  17:05:23 Tue Mar 12 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rp-clm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/trailing-slash
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; DESCRIPTION
;;; This function ensures that a (path) string ends with a trailing slash.
;;; NB: This function is borrowed from Michael Edward's slippery-chicken.
;;;
;;; ARGUMENTS
;;; A string containing the path to be checked and corrected.
;;; 
;;; RETURN VALUE
;;; The path with a trailing slash.
;;;
;;; SYNOPSIS
(defun trailing-slash (path)
  ;;; ****
  (if (> (length path) 0)
    (if (char= #\/ (elt path (1- (length path))))
        path
        (format nil "~a/" path))
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/path-from-same-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; DESCRIPTION
;;; This function returns the full path to a file relative to the directory of
;;; the current lisp file.
;;; NB: This function is borrowed from Michael Edwards's slippery-chicken.
;;; NB2: This function does not work with files which have been loaded via
;;;      ASDF/quicklisp.
;;;
;;; ARGUMENTS
;;; - A string indicating the filename (or pathname) to the file relative to
;;;   the current lisp file.
;;; 
;;; RETURN VALUE
;;; A string with the full path to the file.
;;; 
;;; SYNOPSIS
(defun path-from-same-dir (file)
   ;;; ****
  (concatenate 'string
               (trailing-slash
                (directory-namestring (truename *load-pathname*)))
               file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF utilities.lisp
