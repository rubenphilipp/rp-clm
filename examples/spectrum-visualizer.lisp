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

(let* ((sndfile (path-from-same-dir "brushes.wav"))
       (anafile "/tmp/vis.csv")
       (width 4000)
       (height 2000)
       (frame-rate 25)
       (frames (floor (* (clm:sound-duration sndfile) frame-rate)))
       (outdir "/tmp/vis/")
       (render-start-time (get-universal-time))
       (snd-ana nil)
       (num-windows nil)
       ;; visualisation start offset
       ;; purpose: exclude low-freq bins
       (bin-offset 3)
       (fmin 0)
       (fmax (floor (clm:sound-srate sndfile)))
       (starting-point '()))
  (print "Starting fft analysis...")
  (rp-clm:analyze-spectrum sndfile :rfreq frame-rate :fftsize 4096
                                   :outfile anafile)
  (print "Processing fft analysis...")
  (setf snd-ana (rp-clm:analysis->array anafile))
  (setf num-windows (length (aref snd-ana 0)))
  (setf starting-point (list 0 (rescale (parse-float:parse-float
                                         (nth bin-offset (aref snd-ana 0)))
                                        0.0 1.0 height 0)))
  (ensure-directories-exist outdir)
  (print "Starting image generation...")
  (lparallel:pdotimes (i frames)
    (let* ((canvas (make-svg-toplevel :width width
                                      :height height))
           (outfile (concatenate 'string
                                 outdir
                                 (format nil "~a.png" i)))
           (num-points num-windows)
           (start-time (get-universal-time)))
      ;;(print "draw...")
      (svg:draw canvas
          (:path :d
            (loop for j from bin-offset to (1- num-points)
                  for j2 from 0
                  with path = (apply #'svg:move-to starting-point)
                  with data = (aref snd-ana i)
                  for x = (floor (rescale j2 0 num-points 0 width))
                  ;; note: dimensions are flipped here (because of svg coords)
                  ;; RP  Tue Mar 12 23:28:39 2024
                  for y = (floor (rescale (parse-float:parse-float (nth j data))
                                          0.0 1.0 height 0))
                  do
                     ;;(format t "~%~a ~a ~%---" x y)))))))
                     (setf path (concatenate 'string
                                             path
                                             (svg:line-to x y)))
                  finally
                     (return path))
            :fill "none" :stroke "black" :stroke-width 1))
      (svg->png canvas :outfile outfile)
      (format t "File: ~a~%~
                 Duration: ~a sec~%"
              outfile
              (- (get-universal-time) start-time))))
  (format t "Time elapsed: ~a sec ~%"
          (- (get-universal-time) render-start-time)))

  
(shutdown-kernel)
