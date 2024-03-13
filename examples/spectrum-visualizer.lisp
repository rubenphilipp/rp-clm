;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; visualize a spectrum
;;; this requires :apparence
;;; cf. http://github.com/rubenphilipp/apparence
;;; RP  Tue Mar 12 22:16:19 2024

(ql:quickload :apparence)

(ql:quickload :parse-float)

(in-package :apparence)

(unless (find-package 'rp-clm)
  (asdf:load-system :rp-clm))

(unless (find-package :serapeum)
  (ql:quickload :serapeum))

(init-kernel (serapeum:count-cpus))

(let* ((sndfile (path-from-same-dir "something.wav"))
       (anafile "/tmp/vis.csv")
       (width 4000)
       (height 1500)
       (fftsize 4096)
       (frame-rate 25)
       (duration (min (clm:sound-duration sndfile) 1.5))
       (frames (floor (* duration frame-rate)))
       (outdir "/tmp/vis/")
       (render-start-time (get-universal-time))
       (snd-ana nil)
       (num-bins nil)
       ;; visualisation start offset
       ;; purpose: exclude low-freq bins
       (bin-offset 2)
       (stroke-col "black")
       ;; logarithmic frequency spectrum?
       (log? nil)
       (fmin 0)
       (fmax (floor (clm:sound-srate sndfile))))
  (let* ((anafil? (probe-file anafile))
         (ana? (not (when anafil?
                      (y-or-n-p "The analysis file ~a already exists. Use this ~
                                 file (y) or redo analysis (n)?" anafile)))))
    (when ana?
      (print "Starting fft analysis...")
      (rp-clm:analyze-spectrum sndfile :rfreq frame-rate :fftsize fftsize
                                       :norm-type 0
                                       :dur duration
                                       :outfile anafile)))
  (print "Processing fft analysis...")
  (setf snd-ana (rp-clm:analysis->array anafile))
  (setf num-bins (- (length (aref snd-ana 0)) bin-offset))
  (ensure-directories-exist outdir)
  (print "Starting image generation...")
  (lparallel:pdotimes (i frames)
    (let* ((canvas (make-svg-toplevel :width width
                                      :height height))
           (outfile (concatenate 'string
                                 outdir
                                 (format nil "~a.png" i)))
           (num-points num-bins)
           (start-time (get-universal-time)))
      ;;; make multiple paths
      (loop with heights = (loop repeat 100
                                 collect (+ 80 (random 50)))
            with y-offsets = (loop repeat (length heights)
                                   collect (+ 100 (random 50)))
            for hght in heights
            for starting-point = (list 0 (rescale (parse-float:parse-float
                                                   (nth bin-offset
                                                        (aref snd-ana 0)))
                                                  -105.0 0.0 0 hght))
            for y-offset in y-offsets
            do
               (svg:draw canvas
                   (:path :d
                     (loop for j from bin-offset to (1- num-points)
                           for j2 from 0
                           with path = (apply #'svg:move-to starting-point)
                           with data = (aref snd-ana i)
                           ;; logarithmic scaling
                           ;; RP  Wed Mar 13 19:12:27 2024
                           for x = (floor (rescale (if log?
                                                       (log (1+ j2) 10)
                                                       j2)
                                                   0
                                                   (if log?
                                                       (log (1+ num-points) 10)
                                                       num-points)
                                                   0 width))
                           ;; scale from unnormalized dB
                           ;; RP  Wed Mar 13 19:45:08 2024
                           for y = (floor (- (+ hght y-offset)
                                             (rescale (parse-float:parse-float
                                                       (nth j data))
                                                      -105.0 0.0 0 hght)))
                           do
                              ;;(format t "~%~a ~a ~%---" x y)))))))
                              (setf path (concatenate 'string
                                                      path
                                                      (svg:line-to x y)))
                           finally
                              (return path))
                     :fill "none" :stroke stroke-col :stroke-width 0.1)))
      (svg->png canvas :outfile outfile)
      (format t "File: ~a~%~
                 Duration: ~a sec~%"
              outfile
              (- (get-universal-time) start-time))))
  (format t "Time elapsed: ~a sec ~%"
          (- (get-universal-time) render-start-time)))

  
(shutdown-kernel)
