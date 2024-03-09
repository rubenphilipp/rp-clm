;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* rp-clm/filters
;;; NAME
;;; filters
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-09
;;; 
;;; PURPOSE
;;; Implementation of various (dsp) filter generators and related methods.  Some
;;; functions are implementations of the procedures described in the Scheme
;;; version of clm/snd
;;; (cf. https://ccrma.stanford.edu/software/snd/snd/sndclm.html and
;;; https://ccrma.stanford.edu/software/snd/).
;;;
;;; CLASS HIERARCHY
;;; none. no classes defined
;;;
;;; $$ Last modified:  22:52:52 Sat Mar  9 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rp-clm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* filters/make-biquad
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-09
;;; 
;;; DESCRIPTION
;;; Make a biquad filter via the filter gen.
;;; Cf. dsp.scm (snd). 
;;;
;;; ARGUMENTS
;;; The biquad coefficients:
;;; - a0
;;; - a1
;;; - a2
;;; - b1
;;; - b2
;;; 
;;; RETURN VALUE
;;; A biquad/filter gen. 
;;;
;;; EXAMPLE
#|
(make-biquad -.1 -.2 -.02 1.2 3.2)
;; =>
#<(filter: order: 3, xcoeffs: [-0.100, -0.200, -0.020], ycoeffs: [#1=0.0, 1.200, 3.200], state: [#1#, #1#, #1#]>
|#
;;; SYNOPSIS
(defun make-biquad (a0 a1 a2 b1 b2)
  ;;; ****
  (let ((xcoeffs (make-double-float-array 3 :initial-contents
                                          (list a0 a1 a2)))
        (ycoeffs (make-double-float-array 3 :initial-contents
                                          (list 0.0 b1 b2))))
    (make-filter 3 xcoeffs ycoeffs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; from dsp.scm
(defun make-local-biquad (a0 a1 a2 gamma beta)
  (make-biquad a0 a1 a2 (* -2.0 gamma) (* 2.0 beta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* filters/make-iir-lowpass
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-09
;;; 
;;; DESCRIPTION
;;; Implementation from dsp.scm (snd). 
;;; 
;;; RETURN VALUE
;;; A filter gen. 
;;;
;;; SYNOPSIS
(defun make-iir-lowpass (fc din)
  ;;; din=(sqrt 2.0) for example (suggested range 0.2.. 10)
  ;;; from dsp.scm (RP  Sat Mar  9 19:35:56 2024)
  ;;; ****
  (let* ((theta (/ (* 2 pi fc) *clm-srate*))
         (beta (let ((d (* (sin theta) (/ (or din (sqrt 2.0)) 2))))
                 (* 0.5 (/ (- 1.0 d) (+ 1.0 d)))))
         (gamma (* (+ 0.5 beta) (cos theta)))
         (alpha (* 0.5 (- (+ 0.5 beta) gamma))))
    (make-local-biquad alpha (* 2.0 alpha) alpha gamma beta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* filters/make-iir-highpass
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-09
;;; 
;;; DESCRIPTION
;;; Implementation from dsp.scm (snd). 
;;; 
;;; RETURN VALUE
;;; A filter gen. 
;;;
;;; SYNOPSIS
(defun make-iir-highpass (fc din)
  ;;; din=(sqrt 2.0) for example (suggested range 0.2.. 10)
  ;;; from dsp.scm (RP  Sat Mar  9 19:35:56 2024)
  ;;; ****
  (let* ((theta (/ (* 2 pi fc) *clm-srate*))
         (beta (let ((d (* (sin theta) (/ (or din (sqrt 2.0)) 2))))
                 (* 0.5 (/ (- 1.0 d) (+ 1.0 d)))))
         (gamma (* (+ 0.5 beta) (cos theta)))
         (alpha (* 0.5 (+ 0.5 beta gamma))))
    (make-local-biquad alpha (* -2.0 alpha) alpha gamma beta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* filters/make-iir-bandpass
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-09
;;; 
;;; DESCRIPTION
;;; Implementation from dsp.scm (snd). 
;;; 
;;; RETURN VALUE
;;; A filter gen. 
;;;
;;; SYNOPSIS
(defun make-iir-bandpass (f1 f2)
  ;;; din=(sqrt 2.0) for example (suggested range 0.2.. 10)
  ;;; from dsp.scm (RP  Sat Mar  9 19:35:56 2024)
  ;;; ****
  (when (>= f1 f2)
    (error "make-iir-bandpass: f1 should be < f2"))
  (let* ((theta (/ (* 2 pi (sqrt (* f1 f2))) *clm-srate*))
         (beta (let ((t2 (tan (/ theta (* 2 (/ (sqrt (* f1 f2)) (- f2 f1)))))))
                 (* 0.5 (/ (- 1.0 t2) (+ 1.0 t2)))))
         (gamma (* (+ 0.5 beta) (cos theta)))
         (alpha (- 0.5 beta)))
    (make-local-biquad alpha 0.0 (- alpha) gamma beta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* filters/make-iir-bandstop
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-09
;;; 
;;; DESCRIPTION
;;; Implementation from dsp.scm (snd). 
;;; 
;;; RETURN VALUE
;;; A filter gen. 
;;;
;;; SYNOPSIS
(defun make-iir-bandstop (f1 f2)
  ;;; din=(sqrt 2.0) for example (suggested range 0.2.. 10)
  ;;; from dsp.scm (RP  Sat Mar  9 19:35:56 2024)
  ;;; ****
  (when (>= f1 f2)
    (error "make-iir-bandstop: f1 should be < f2"))
  (let* ((theta (/ (* 2 pi (sqrt (* f1 f2))) *clm-srate*))
         (beta (let ((t2 (tan (/ theta (* 2 (/ (sqrt (* f1 f2)) (- f2 f1)))))))
                 (* 0.5 (/ (- 1.0 t2) (+ 1.0 t2))))))
    (let ((gamma (* (+ 0.5 beta) (cos theta)))
          (alpha (+ 0.5 beta)))
      (make-local-biquad alpha (* -2.0 gamma) alpha gamma beta))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF filters.lisp
