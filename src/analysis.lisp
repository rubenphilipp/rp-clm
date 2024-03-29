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
;;; none. no classes defined. 
;;;
;;; $$ Last modified:  19:45:52 Wed Mar 13 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rp-clm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* analysis/analysis->array
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-12
;;; 
;;; DESCRIPTION
;;; This function reads an analysis csv file (generated via analyze-spectrum)
;;; into an array. 
;;;
;;; ARGUMENTS
;;; The path to the csv file containing the analysis data. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :header-row? Include the header row (cf. analyze-spectrum) in the array?
;;;   Default = nil
;;; - :header-col? Include the header column (i.e. the timestamps, cf.
;;;   analyze-spectrum) in the array? Default = nil
;;; 
;;; RETURN VALUE
;;; The array. 
;;;
;;; EXAMPLE
#|
(analysis->array "/tmp/ana.csv"
                 :header-row? t
                 :header-col? t)
;; =>
#(("t" "0.000000" "23.437500" "46.875000" "70.312500" "93.750000" "117.187500"
   "140.625000" "164.062500" "187.500000" "210.937500" "234.375000"
   "257.812500" "281.250000" "304.687500" "328.125000" "351.562500"
   "375.000000" "398.437500" "421.875000" "445.312500" "468.750000"
   "492.187500" "515.625000" "539.062500" "562.500000" "585.937500"
   "609.375000" "632.812500" "656.250000" "679.687500" "703.125000"
   "726.562500" "750.000000" "773.437500" "796.875000" "820.312500" etc...
|#
;;; SYNOPSIS
(defun analysis->array (file &key header-row? header-col?)
  ;;; ****
  (let ((csv (cl-csv:read-csv (pathname file) :separator #\comma)))
    (make-array (if header-row?
                    (length csv)
                    (1- (length csv)))
                :initial-contents
                (cond ((and header-row? (not header-col?))
                       (mapcar #'cdr csv))
                      ((and (not header-row?) header-col?)
                       (cdr csv))
                      ((and header-row? header-col?)
                       csv)
                      (t (mapcar #'cdr (cdr csv)))))))
                       
                                         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* analysis/analyze-spectrum
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-12
;;; 
;;; DESCRIPTION

;;; This instrument performs multiple fft-analyzes of a given sndfile and writes
;;; the results to a csv-file. The time-delta of the starting points of the
;;; analyzes is determined by a frequency (rfreq) which is relative to the
;;; sample-rate of the source sndfile.
;;; The resulting csv-file contains a header that lists the start frequency of
;;; each fft-bin, which -- obviously -- depends on the fftsize. The next rows
;;; contain the analysis data for each bin either in dB, linear normalized or
;;; linear unnormalized (cf. norm-type). The first column is the index of the
;;; respective analysis data, either in seconds (default) or in samples.
;;;
;;; Please note:
;;; Depending on the settings and the duration of the sndfile (or of the segment
;;; to be analyzed), the analysis might take some time and the resulting file
;;; can grow quite big. 
;;; 
;;; 
;;; ARGUMENTS
;;; The path to the sndfile which is to be analyzed (as a string).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :beg. The start time of the analysis (in seconds) in the sndfile.
;;;   Default = 0
;;; - :dur. The duration of the analysis (in seconds), relative to beg. When
;;;   nil, the complete sndfile (from beg) will be analyzed. Default = nil.
;;; - :rfreq. The "sampling-frequency" (in Hz) of the fft-analysis, i.e. the
;;;   number of analyses per second. Default = 100
;;; - :in-channel. The channel of the sndfile to analyze. Default = 0
;;; - :outfile. The output filepath for the csv-file.
;;;   Default = "/tmp/spectrum.csv"
;;; - :norm-type. The normalization method of the amplitudes in the bins.
;;;   In dB (norm-type = 0), or linear normalized to 1.0 (norm-type = 1),
;;;   or linear unnormalized (norm-type not 0 or 1). Default = 0
;;; - :window-type. The window type for the fft (cf. clm doc).
;;;   Default = hann-window
;;; - :in-samples?. When T, the first column (the index) will be given in
;;;   samples instead of seconds. Default = nil
;;; 
;;; RETURN VALUE
;;; The path to the analysis (csv-)file.
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
;; => "/tmp/ana.csv"

;; use vgplot to visualize the analysis:
(let* ((anafile "/tmp/ana.csv")
       (separator #\comma)
       (csv (cl-csv:read-csv (pathname anafile) :separator separator))
       (row 300)
       (freqs (cdr (first csv)))
       (snapshot (cdr (nth row csv))))
  (vgplot::plot freqs snapshot))
|#
;;; SYNOPSIS
(definstrument analyze-spectrum (file &key
                                 (beg 0.0) (dur nil) (rfreq 100)
                                 (in-channel 0)
                                 (outfile "/tmp/spectrum.csv")
                                 (norm-type 0) in-samples?
                                 (fftsize 4096)
                                 (window-type hann-window))
  ;;; ****
  (unless (probe-file file)
    (error "analyze-spectrum: The (snd)file does not exist!"))
  (when (probe-file outfile)
    (warn "analyze-spectrum: The outfile ~a already exists and will be ~
           superseded." outfile)
    (delete-file outfile))
  (let ((fil (open-input* file :channel in-channel))
        (outfil (c-open-output-file outfile)))
    (unwind-protect
         (let* ((incr (/ rfreq))
                (fsr (sound-srate file))
                (incrsamps (floor (* incr fsr)))
                (winfun (make-fft-window :type window-type :size fftsize))
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
                  (dotimes (k (1+ fft2))
                    (if (zerop k)
                        (clm-print outfil "t,")
                        (progn
                          (clm-print outfil "~A" freq)
                          (unless (= k fft2)
                            (clm-print outfil ","))
                          (incf freq binwidth)))))
                (clm-print outfil "~%"))
              ;; output data
              ;; NB: 1+ because of header col
              (dotimes (k fft2)
                (when (zerop k)
                  ;; add timestamp
                  (clm-print outfil "~A"
                             (if in-samples? i (/ i fsr))))
                (unless (= k fft2)
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
