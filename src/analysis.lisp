;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* rp-clm/analysis
;;; NAME
;;; filters
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-11
;;; 
;;; PURPOSE
;;; This module implements various functions and instruments to analyze
;;; soundfiles. 
;;;
;;; CLASS HIERARCHY
;;;
;;;
;;; $$ Last modified:  23:32:44 Mon Mar 11 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rp-clm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This instrument returns an the fft analysis data of a given file.
;;; Results in dB (norm-type = 0), or linear normalized to 1.0 (norm-type = 1),
;;; or linear unnormalized (norm-type not 0 or 1).
;;; When in-samples?, the timestamp in the csv will be printed in samples
;;; (relative to the sndfile) instead of seconds. 
;;; The results are written to a csv file. 
;;; cf. scentroid.ins and track-rms.ins
;;; RP  Mon Mar 11 16:54:45 2024
;;;
;;; EXAMPLE
#|
(let* ((infile "/Users/rubenphilipp/Desktop/clm/fft/brushes.wav")
       (outfile "/tmp/ana.csv")
       (res (analyze-spectrum infile :in-samples? nil
                                     :fftsize 2048
                                     :norm-type 0
                                     ;;:dur .5
                                     :outfile outfile)))
  res)

(let* ((anafile "/tmp/ana.csv")
       (separator #\comma)
       (csv (cl-csv:read-csv (pathname anafile) :separator separator))
       (row 300)
       (freqs (cdr (first csv)))
       (snapshot (cdr (nth row csv))))
  (vgplot::plot freqs snapshot))
|#

(definstrument analyze-spectrum (file &key
                                 (beg 0.0) (dur nil) (rfreq 100)
                                 (outfile "/tmp/spectrum.csv")
                                 (norm-type 1) in-samples?
                                 (fftsize 4096) (debug nil))
  ;;; ****
  (when (probe-file outfile)
    (warn "analyze-spectrum: The outfile ~a already exists and will be ~
           superseded." outfile)
    (delete-file outfile))
  (let ((fil (open-input* file))
        (outfil (c-open-output-file outfile)))
    (unwind-protect
         (let* ((incr (/ rfreq))
                (fsr (sound-srate file))
                (incrsamps (floor (* incr fsr)))
                (winfun (make-fft-window :type hann-window :size fftsize))
                (start (floor (* beg fsr)))
                (dur (if dur dur (- (sound-duration file) beg)))
                (end (+ start (floor (* dur fsr))))
                ;;(windows (1+ (floor (/ (- end start) incrsamps))))
                (fdr (make-double-float-array fftsize))
                (fdi (make-double-float-array fftsize))
                (fft2 (floor (/ fftsize 2)))
                (binwidth (/ fsr fftsize))
                (filptr 0)
                (i 0)
                (j 0))
           (setf i start)
           (run 
            (loop while (<= i end) do
              (clear-array fdr)
              (clear-array fdi)
              ;; set file pointer
              (setf filptr i)
              ;; prepare and perform the fft
              (dotimes (k fftsize)
                (setf (aref fdr k) (ina filptr fil))
                (incf filptr))
              (spectrum fdr fdi winfun norm-type)
              ;; output header
              (when (= i start)
                (let ((freq 0))
                  (dotimes (k fft2)
                    (if (zerop k)
                        (clm-print outfil "t,")
                        (progn
                          (clm-print outfil "~A" freq)
                          (unless (= k (1- fft2))
                            (clm-print outfil ","))))
                    (incf freq binwidth)))
                (clm-print outfil "~%"))
              (dotimes (k fft2)
                (when (zerop k)
                  ;; add timestamp
                  (clm-print outfil "~A"
                             (if in-samples? i (/ i fsr))))
                (unless (= k (1- fft2))
                  (clm-print outfil ","))
                (clm-print outfil "~A" (aref fdr k)))
              (clm-print outfil "~%")
              ;; increase counters
              (incf i incrsamps)
              (incf j)))
           (close-input fil)
           (c-close outfil)
           outfile))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF analysis.lisp
