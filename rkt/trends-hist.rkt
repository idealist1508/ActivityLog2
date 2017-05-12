#lang racket/base

;; trends-hist.rkt -- aggregate histogram chart
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016 Alex Harsanyi (AlexHarsanyi@gmail.com)
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.


(require
 db
 plot
 racket/class
 racket/match
 racket/gui/base
 racket/math
 racket/format
 racket/string
 racket/list
 "database.rkt"
 "trends-chart.rkt"
 "icon-resources.rkt"
 "widgets.rkt"
 "plot-hack.rkt"
 "sport-charms.rkt"
 "data-frame.rkt"
 "al-widgets.rkt"
 "series-meta.rkt"
 "metrics.rkt"
 "spline-interpolation.rkt"
 "workers.rkt")


;;....................................................... axis selection ....

;; Find an axis that works in SERIES-NAME and return its position in
;; AXIS-LIST.  Return #f is not found
(define (find-axis series-name axis-list)
  (for/first ([(axis index) (in-indexed axis-list)]
              #:when
              (let ((sn (if (list? axis)
                            (string-join
                             (map (lambda (m) (send m series-name)) (cdr axis))
                             "+")
                            (send axis series-name))))
                (equal? series-name sn)))
    index))

;; Axis choices for all non lap swimming sports.
(define default-axis-choices
  (list
   axis-speed
   axis-pace
   axis-speed-zone
   axis-grade
   axis-hr-bpm
   axis-hr-pct
   axis-hr-zone
   axis-cadence
   axis-vertical-oscillation
   axis-stance-time
   axis-stance-time-percent
   axis-vratio
   axis-stride
   axis-power
   axis-power-zone
   axis-left-right-balance
   (list "Torque Effectiveness (%)" axis-left-torque-effectiveness axis-right-torque-effectiveness)
   (list "Pedal Smoothness (%)" axis-left-pedal-smoothness axis-right-pedal-smoothness)
   (list "Platform Center Offset" axis-left-platform-centre-offset axis-right-platform-centre-offset)
   (list "Power Phase Start" axis-left-power-phase-start axis-right-power-phase-start)
   (list "Power Phase End" axis-left-power-phase-end axis-right-power-phase-end)
   (list "Power Phase Angle" axis-left-power-phase-angle axis-right-power-phase-angle)
   (list "Peak Power Phase Start" axis-left-peak-power-phase-start axis-right-peak-power-phase-start)
   (list "Peak Power Phase End" axis-left-peak-power-phase-end axis-right-peak-power-phase-end)
   (list "Peak Power Phase Angle" axis-left-peak-power-phase-angle axis-right-peak-power-phase-angle)
   ))

;; Axis choices for lap swimming
(define swim-axis-choices
  (list
   axis-swim-avg-cadence
   axis-swim-stroke-count
   axis-swim-stroke-length
   axis-swim-swolf
   axis-swim-pace))


;;................................................. hist-chart-settings% ....

(struct hist-params tc-params (start-date end-date sport series zeroes? colors? aspct? bwidth otrim) #:transparent)

(provide hist-chart-settings%)
(define hist-chart-settings%
  (class al-edit-dialog%
    (init-field database
                [default-name "Hist"]
                [default-title "Histogram Chart"])

    (super-new [title "Chart Settings"]
               [icon edit-icon]
               [min-height 10])

    (define series-selector #f)
    (define axis-choices #f)
    ;; determines if the SERIES-SELECTOR contains lap swimming series
    (define lap-swimming-series? #f)
    ;; last selection on the lap swimming series
    (define last-lap-swim-selection #f)
    ;; last selection on the default series
    (define last-non-lap-swim-selection #f)

    (define (install-axis-choices new-choices selection)
      (set! axis-choices
        (sort new-choices string<? #:key
              (lambda (x)
                (if (list? x) (car x) (send x axis-label)))))

      (send series-selector clear)
      (for ([a axis-choices])
        (let ((n (if (list? a) (car a) (send a axis-label))))
          (send series-selector append n)))

      (when (and selection (>= selection 0) (< selection (length axis-choices)))
        (send series-selector set-selection selection)))

    (define (on-sport-selected sport)
      (define lap-swimming?
        (and (eq? (car sport) 5) (eq? (cdr sport) 17)))
      (unless (eq? lap-swimming? lap-swimming-series?)
        (if lap-swimming?
            (begin
              (set! last-non-lap-swim-selection (send series-selector get-selection))
              (install-axis-choices swim-axis-choices last-lap-swim-selection))
            (begin
              (set! last-lap-swim-selection (send series-selector get-selection))
              (install-axis-choices default-axis-choices last-non-lap-swim-selection))))
      (set! lap-swimming-series? lap-swimming?))

    (define name-gb (make-group-box-panel (send this get-client-pane)))
    (define name-field (new text-field% [parent name-gb] [label "Name "]))
    (send name-field set-value default-name)
    (define title-field (new text-field% [parent name-gb] [label "Title "]))
    (send title-field set-value default-title)

    (define time-gb (make-group-box-panel (send this get-client-pane)))
    (define sport-selector
      (new sport-selector% [parent time-gb] [sports-in-use-only? #t]
           [callback on-sport-selected]))
    (define date-range-selector (new date-range-selector% [parent time-gb]))

    (define series-gb (make-group-box-panel (send this get-client-pane)))
    (set! series-selector
          (let ((p (make-horizontal-pane series-gb #f)))
            (send p spacing al-dlg-item-spacing)
            (new choice% [parent p]
                 [label "Data Series: "] [choices '("***************************")])))

    (define other-gb (make-group-box-panel (send this get-client-pane)))
    (define include-zeroes-checkbox
      (let ((p (make-horizontal-pane other-gb #f)))
        (send p spacing al-dlg-item-spacing)
        (new check-box% [parent p] [label "Include Zeroes"])))
    (define color-by-zone-checkbox
      (let ((p (make-horizontal-pane other-gb #f)))
        (send p spacing al-dlg-item-spacing)
        (new check-box% [parent p] [label "Color by Zone"])))
    (define show-as-pct-checkbox
      (let ((p (make-horizontal-pane other-gb #f)))
        (send p spacing al-dlg-item-spacing)
        (new check-box% [parent p] [label "Show as Percentage"])))
    (define bucket-width-field
      (new number-input-field% [parent other-gb]
           [label "Bucket Width "]
           [cue-text "1 to 100"]
           [min-value 1] [max-value 100]
           [stretchable-width #f]))
    (define outlier-trim-field
      (new number-input-field% [parent other-gb]
           [label "Outlier Trim (%) "] [cue-text "0 .. 100%"]
           [min-value 0] [max-value 100]
           [stretchable-width #f]))

    (define/override (has-valid-data?)
      (and
       (send bucket-width-field has-valid-value?)
       (send outlier-trim-field has-valid-value?)))

    (install-axis-choices default-axis-choices #f)

    (define (get-selected-series-name)
      (let* ((index (send series-selector get-selection))
             (axis (list-ref axis-choices index)))
        (if (list? axis)
            (string-join
             (map (lambda (m) (send m series-name)) (cdr axis))
             "+")
            (send axis series-name))))

    (define/public (get-restore-data)
      (hash
       'name (send name-field get-value)
       'title (send title-field get-value)
       'date-range (send date-range-selector get-restore-data)
       'sport (send sport-selector get-selection)
       'series (get-selected-series-name)
       'include-zeroes? (send include-zeroes-checkbox get-value)
       'color-by-zone? (send color-by-zone-checkbox get-value)
       'show-as-pct? (send show-as-pct-checkbox get-value)
       'bucket-width (send bucket-width-field get-converted-value)
       'outlier-trim (send outlier-trim-field get-converted-value)))

    (define/public (restore-from data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (when (hash? data)
        (send name-field set-value (hash-ref data 'name "Hist"))
        (send title-field set-value (hash-ref data 'title "Histogram Chart"))
        (let ((dr (hash-ref data 'date-range #f)))
          (when dr
            (send date-range-selector restore-from dr)))
        (let ((sp (hash-ref data 'sport #f)))
          (when sp
            (send sport-selector set-selected-sport (car sp) (cdr sp))
            (on-sport-selected sp)))
        (let ((series (hash-ref data 'series #f)))
          (when series
            (let ((index (find-axis series axis-choices)))
              (when index
                (send series-selector set-selection index)))))
        (send include-zeroes-checkbox set-value (hash-ref data 'include-zeroes? #f))
        (send color-by-zone-checkbox set-value (hash-ref data 'color-by-zone? #f))
        (send show-as-pct-checkbox set-value (hash-ref data 'show-as-pct? #f))
        (let ((bw (hash-ref data 'bucket-width 'empty)))
          (if (eq? bw 'empty)
              (send bucket-width-field set-value "")
              (send bucket-width-field set-numeric-value bw)))
        (let ((otrim (hash-ref data 'outlier-trim 'empty)))
          (if (eq? otrim 'empty)
              (send outlier-trim-field set-value "")
              (send outlier-trim-field set-numeric-value otrim)))
        ))

    (define/public (show-dialog parent)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (if (send this do-edit parent)
          (get-settings)
          #f))

    (define/public (get-settings)
      (let ((dr (send date-range-selector get-selection)))
        (if dr
            (let ((start-date (car dr))
                  (end-date (cdr dr)))
              (when (eqv? start-date 0)
                (set! start-date (get-true-min-start-date database)))
              (hist-params
               (send name-field get-value)
               (send title-field get-value)
               start-date
               end-date
               (send sport-selector get-selection)
               (get-selected-series-name)
               (send include-zeroes-checkbox get-value)
               (send color-by-zone-checkbox get-value)
               (send show-as-pct-checkbox get-value)
               (let ((bw (send bucket-width-field get-converted-value)))
                 (if (or (not bw) (eq? bw 'empty)) 1 (if (> bw 1) bw 1)))
               (let ((otrim (send outlier-trim-field get-converted-value)))
                 (if (eq? otrim 'empty) 0 otrim))))
            #f)))
    ))

;; Fetch a list of session IDs from the database DB corresponding to
;; parameters in PARAMS (a HIST-PARAMS instance).  Sessions are fetched based
;; on start and end date and the selected sport.
(define (candidate-sessions db params)
  (let ((start (hist-params-start-date params))
        (end (hist-params-end-date params))
        (sport (hist-params-sport params)))
    (fetch-candidate-sessions db (car sport) (cdr sport) start end)))

;; AXIS is a list of series-metadata% instances (one for a single series, two
;; for a dual series), data is a list of histograms (as returned by
;; AGGREGATE-HIST), one for a single series, two for dual series.
(struct hist (axis data) #:transparent)

(define (fetch-data db params progress)
  (let* ((candidates (candidate-sessions db params))
         ;; Series can be "lteff+rteff" for dual series!
         (series (string-split (hist-params-series params) "+")))
    (hist
     (for/list ([s series]) (find-meta-for-series s))
     (for/list ([s series]) (aggregate-hist candidates s #:progress-callback progress)))))

(define (make-render-tree data params)
  (define cbz? (hist-params-colors? params))
  (define aspct? (hist-params-aspct? params))
  (define zeroes? (hist-params-zeroes? params))

  (define dual? (>= (length (hist-axis data)) 2))
  (define axis1 (first (hist-axis data)))
  (define axis2 (and dual? (second (hist-axis data))))
  (define hist1 (first (hist-data data)))
  (define hist2 (and dual? (second (hist-data data))))
  (define bwidth1
    (* (hist-params-bwidth params)
       (send axis1 histogram-bucket-slot)))
  (define bwidth2
    (and axis2
         (* (hist-params-bwidth params)
            (send axis2 histogram-bucket-slot))))
  (define factor-fn (and cbz? (send axis1 factor-fn (hist-params-sport params))))
  (define factor-colors (send axis1 factor-colors))

  (define h1 (expand-histogram hist1
                               #:include-zeroes? zeroes?
                               #:bucket-width bwidth1
                               #:as-percentage? aspct?))
  (define h2 (and hist2
                  (expand-histogram hist2
                                    #:include-zeroes? zeroes?
                                    #:bucket-width bwidth2
                                    #:as-percentage? aspct?)))

  (when (> (hist-params-otrim params) 0)
    (let ((trim (/ (hist-params-otrim params) 100)))
      (set! h1 (trim-histogram-outliers h1 trim))
      (when h2
        (set! h2 (trim-histogram-outliers h2 trim)))))

  (define (get-color axis)
    (let ((color (send axis plot-color)))
      (if (or (not color) (eq? color 'smart))
          '(0 148 255)
          color)))

  (if (zero? (vector-length h1))
      #f
      (list
       (tick-grid)
       (cond (dual?
              (if (zero? (vector-length h2))
                  #f
                  (make-histogram-renderer/dual
                   h1 (send axis1 plot-label)
                   h2 (send axis2 plot-label)
                   #:x-value-formatter (send axis1 value-formatter)
                   #:color1 (get-color axis1)
                   #:color2 (get-color axis2))))
             (factor-fn
              (make-histogram-renderer/factors
               h1 factor-fn factor-colors
               #:x-value-formatter (send axis1 value-formatter)))
             (#t
              (make-histogram-renderer
               h1
               #:x-value-formatter (send axis1 value-formatter)
               #:color (get-color axis1)))))))

(define (insert-plot-snip canvas axis params rt)
  (if rt
      (let* ((aspct? (hist-params-aspct? params))
             (lap-swim? (is-lap-swimming? (hist-params-sport params)))
             (label (if aspct? "pct %" (if lap-swim? "# of lengths" "time (seconds)"))))
        (parameterize ([plot-y-label label]
                       [plot-x-ticks (send axis plot-ticks)]
                       [plot-x-label (send axis axis-label)])
          (plot-snip/hack canvas rt)))
      (begin
        (send canvas set-snip #f)
        (send canvas set-background-message "No data to plot"))))

(provide hist-trends-chart%)
(define hist-trends-chart%
  (class trends-chart%
    (init-field database) (super-new)

    (define cached-data #f)
    (define generation 0)

    (define (get-generation) generation)

    (define/override (make-settings-dialog)
      (new hist-chart-settings%
           [default-name "Histogram"]
           [default-title "Histogram"]
           [database database]))

    (define/override (invalidate-data)
      (set! cached-data #f))

    (define/override (export-data-to-file file formatted?)
      (when cached-data
        (call-with-output-file file export-data-as-csv
          #:mode 'text #:exists 'truncate)))

    (define (export-data-as-csv out)

      (define data cached-data)
      (define params (send this get-params))

      ;; NOTE: this section is the same as MAKE-RENDER-TREE, could use some
      ;; refactoring.
      (define aspct? (hist-params-aspct? params))
      (define zeroes? (hist-params-zeroes? params))

      (define dual? (>= (length (hist-axis data)) 2))
      (define axis1 (first (hist-axis data)))
      (define axis2 (and dual? (second (hist-axis data))))
      (define hist1 (first (hist-data data)))
      (define hist2 (and dual? (second (hist-data data))))
      (define bwidth1
        (* (hist-params-bwidth params)
           (send axis1 histogram-bucket-slot)))
      (define bwidth2
        (and axis2
             (* (hist-params-bwidth params)
                (send axis2 histogram-bucket-slot))))

      (define h1 (expand-histogram hist1
                                   #:include-zeroes? zeroes?
                                   #:bucket-width bwidth1
                                   #:as-percentage? aspct?))
      (define h2 (and hist2
                      (expand-histogram hist2
                                        #:include-zeroes? zeroes?
                                        #:bucket-width bwidth2
                                        #:as-percentage? aspct?)))

      (if (zero? (vector-length h1))
          #f
          (if h2
              (let ((nbuckets (merge-lists (get-histogram-buckets h1) (get-histogram-buckets h2))))
                (set! h1 (normalize-histogram h1 nbuckets))
                (set! h2 (normalize-histogram h2 nbuckets))
              
                (write-string
                 (format "Value, Rank ~a, Rank ~a~%"
                         (send axis1 series-name)
                         (send axis2 series-name))
                 out)
                (for ((item1 h1) (item2 h2))
                  (match-define (vector v1 r1) item1)
                  (match-define (vector v2 r2) item2)
                  (write-string (format "~a, ~a, ~a~%"
                                        v1
                                        (exact->inexact r1)
                                        (exact->inexact r2))
                                out)))
              (begin
                (write-string
                 (format "Value, Rank ~a~%"
                         (send axis1 series-name))
                 out)
                (for ((item1 h1))
                  (match-define (vector v1 r1) item1)
                  (write-string (format "~a, ~a~%"
                                        v1
                                        (exact->inexact r1))
                                out))))))

    (define/override (put-plot-snip canvas)
      (send canvas set-snip #f)
      (send canvas set-background-message "Working...")
      (set! generation (add1 generation))
      (let ((previous-data cached-data)
            (params (send this get-params))
            (saved-generation generation))
        (if params
            (queue-task
             "hist-trends-chart%/put-plot-snip"
             (lambda ()
               (define (report-progress p)
                 (queue-callback
                  (lambda ()
                    (when (= saved-generation (get-generation))
                      (send canvas set-background-message
                            (format "Working (~a %)..." (exact-round (* p 100.0))))))))
               (define data (or previous-data (fetch-data database params report-progress)))
               (define rt (make-render-tree data params))
               (queue-callback
                (lambda ()
                  (when (= saved-generation (get-generation))
                    (set! cached-data data) ; put it back, or put the fresh one here
                    (insert-plot-snip canvas (first (hist-axis data)) params rt))))))
            (begin
              (send canvas set-snip #f)
              (send canvas set-background-message "No params for plot")))))

    ))