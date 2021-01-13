#lang racket
(require plot)

;; power-spikes.rkt -- clear power spikes and re-calculate power related
;; metrics.
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2020, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(require "al-interactive.rkt"
         "../rkt/sport-charms.rkt"
         "../rkt/models/coggan.rkt"
         "../rkt/time-in-zone.rkt"
         math/statistics
         racket/match
         data/queue)

;; Return a mapping of timestamp to trackpoint ID for every trackpoint in the
;; session SID.
(define (get-timestamp-mapping sid)
  (define trackpoint-id-sql
    "select T.id as id,
            T.timestamp as timestamp
       from A_TRACKPOINT T, A_LENGTH L, A_LAP P
      where T.length_id = L.id
        and L.lap_id = P.id
        and P.session_id = ?")

  (for/hash (([id timestamp] (in-query (current-database) trackpoint-id-sql sid)))
    (values timestamp id)))

;; Return a mapping of timestamp to lap section summary ID for each lap in the
;; session SID.
;;
;; NOTE: WE DON'T RETURN LAP IDS!!!!!
(define (get-lap-mapping sid)
  (define lap-ssid-sql
    "select P.summary_id as id,
            P.start_time as timestamp
       from A_LAP P
      where P.session_id = ?")

  (for/hash (([id timestamp] (in-query (current-database) lap-ssid-sql sid)))
    (values timestamp id)))

;; Clear out the power and cycling dynamics values for all track points which
;; are above the CUTOFF power inside the dataframe DF.  This code operates
;; directly on the database, and after running it the data-frame will contain
;; outdated data.
(define (clear-power-spikes df cutoff #:database (db (current-database)))
  (define clear-power-data-sql
    "update A_TRACKPOINT
        set power = null,
            accumulated_power = null,
            left_right_balance = null,
            left_torque_effectiveness = null,
            right_torque_effectiveness = null,
            left_pedal_smoothness = null,
            right_pedal_smoothness = null,
            left_pco = null,
            right_pco = null,
            left_pp_start = null,
            left_pp_end = null,
            right_pp_start = null,
            right_pp_end = null,
            left_ppp_start = null,
            left_ppp_end = null,
            right_ppp_start = null,
            right_ppp_end = null
      where id = ?")
  (define sid (df-get-property df 'session-id))
  (define mapping (get-timestamp-mapping sid))
  (call-with-transaction
   db
   (lambda ()
     (for (([timestamp power] (in-data-frame df "timestamp" "pwr")))
       (when (and power (> power cutoff))
         (query-exec db clear-power-data-sql (hash-ref mapping timestamp))))))
  (log-event 'session-updated-data sid))

;; Store a value in the SECTION_SUMMARY table for the SSID row.  FIELD-NAME is
;; updated to VALUE.
(define (put-section-summary-value db ssid field-name value)
  (query-exec
   db
   (format "update SECTION_SUMMARY set ~a = ? where id = ?" field-name)
   value ssid))

;; Store/Update the Coggan metrics CGMETRICS for session SID.
(define (put-session-cg-metrics sid cgmetrics #:database (db (current-database)))
  (match-define (cg ftp np if tss) cgmetrics)
  (call-with-transaction
   db
   (lambda ()
     (define ssid (query-value db "select summary_id from A_SESSION where id = ?" sid))
     (query-exec
      db
      "update A_SESSION set intensity_factor = ?, training_stress_score = ? where id = ?"
      if tss sid)
     (put-section-summary-value db ssid "normalized_power" np))))

;; Update the section summary SSID based on the data-frame averages from START
;; to STOP.  Updates average and maximum power plus all the average cycling
;; dynamics values.
(define (put-section-summary-stats db ssid df #:start (start 0) #:stop (stop (df-row-count df)))

  (when (df-contains? df "pwr")
    (let ((stats (df-statistics df "pwr" #:start start #:stop stop)))
      (put-section-summary-value db ssid "avg_power" (statistics-mean stats))
      (put-section-summary-value db ssid "max_power" (statistics-max stats))))

  (define (put-avg series dbcol)
    (when (df-contains? df series)
      (let ((stats (df-statistics df series #:start start #:stop stop)))
        (put-section-summary-value db ssid dbcol (statistics-mean stats)))))

  (put-avg "lrbal" "left_right_balance")
  (put-avg "lteff" "avg_left_torque_effectiveness")
  (put-avg "rteff" "avg_right_torque_effectiveness")
  (put-avg "lpsmth" "avg_left_pedal_smoothness")
  (put-avg "rpsmth" "avg_right_pedal_smoothness")
  (put-avg "lpco" "avg_left_pco")
  (put-avg "rpco" "avg_right_pco")
  (put-avg "lpps" "avg_left_pp_start")
  (put-avg "lppe" "avg_left_pp_end")
  (put-avg "rpps" "avg_right_pp_start")
  (put-avg "rppe"  "avg_right_pp_end")

  (put-avg "lppps" "avg_left_ppp_start")
  (put-avg "lpppe" "avg_left_ppp_end")
  (put-avg "rppps" "avg_right_ppp_start")
  (put-avg "rpppe"  "avg_right_ppp_end"))


;; Fix power spikes: power data values above CUTOFF are cleared out and Coggan
;; metrics + averages are recalculated.
(define (do-fixups df cutoff #:database (db (current-database)) #:ftp (ftp (get-athlete-ftp)))
  (call-with-transaction
   db
   (lambda ()
     (define sid (df-get-property df 'session-id))
     (define ssid (query-value db "select summary_id from A_SESSION where id = ?" sid))
     (clear-power-spikes df cutoff #:database db)
     (define ndf (session-df db sid))      ; read it back again
     (define scgm (cg-metrics ndf #:ftp ftp))
     (put-section-summary-stats db ssid ndf)
     (put-session-cg-metrics sid scgm #:database db)
     (define lmapping (get-lap-mapping sid))
     (define laps (df-get-property df 'laps))
     (for ([start (in-vector laps)]
           [end (in-sequences (in-vector laps 1) (in-value #f))])
       (match-define (list sindex eindex)
         (if end
             (df-index-of* df "timestamp" start end)
             (list
              (df-index-of df "timestamp" start)
              (df-row-count df))))
       (define lcgm (cg-metrics ndf #:ftp ftp #:start sindex #:stop eindex))
       (define ssid (hash-ref lmapping start))
       (put-section-summary-value db ssid "normalized_power" (cg-np lcgm))
       (put-section-summary-stats db ssid ndf #:start sindex #:stop eindex)
       (query-exec db "delete from BAVG_CACHE where session_id = ?" sid)
       (query-exec db "delete from HIST_CACHE where session_id = ?" sid)
       (query-exec db "delete from SCATTER_CACHE where session_id = ?" sid)
       (update-some-session-metrics sid db)))))

;; Determine the power which contains (1 - q) percent of the values.  I.e. if
;; q = 0.005, returns the power value where only 0.5% of the samples are
;; higher than that -- the returned value can be used as a spike cutoff point.
(define (cutoff-power df [q 0.005])
  (car (df-quantile df "pwr" q #:weight-series "timer" #:less-than >)))

;; Determine the cutoff power based on Dixons Q test for outlier
;; identification.
;;
;; https://sebastianraschka.com/Articles/2014_dixon_test.html
(define (cutoff-power2 df)
  (match-define (list q25 q75)
    (df-quantile df "pwr" 0.25 0.75 #:weight-series "timer" #:less-than >))
  (define iqr (- q25 q75))
  (printf "q25: ~a, q75: ~a, iqr = ~a~%" q25 q75 iqr)
  (+ q25 (* 2.5 iqr)))



(struct bwdata
  (q1 median q3 lower-whisker uppwer-whisker outliers)
  #:transparent)

(define (samples->bwdata vs [ws #f])
  (let* ([q1 (quantile 0.25 < vs ws)]
         [median (quantile 0.5 < vs ws)]
         [q3 (quantile 0.75 < vs ws)]
         [iqr (- q3 q1)]
         [lower-limit (- q1 (* 1.5 iqr))]
         [upper-limit (+ q3 (* 1.5 iqr))]
         [lower-whisker (foldl
                         (lambda (sample a)
                           (if (> sample lower-limit) (min a sample) a))
                         upper-limit vs)]
         [upper-whisker (foldl
                         (lambda (sample a)
                           (if (< sample upper-limit) (max a sample) a))
                         lower-limit vs)]
         [outliers (filter (lambda (sample) (or (> sample upper-whisker)
                                                (< sample lower-whisker))) vs)])
    (bwdata q1 median q3 lower-whisker upper-whisker outliers)))

(define (box-and-whiskers-renderer
         x data

         #:invert? (invert? #f)

         #:gap (gap (discrete-histogram-gap))

         ;; Rectangles options
         #:box-color (box-color (rectangle-color))
         #:box-style (box-style (rectangle-style))
         #:box-line-color (box-line-color (rectangle-line-color))
         #:box-line-width (box-line-width (rectangle-line-width))
         #:box-line-style (box-line-style (rectangle-line-style))
         #:box-alpha (box-alpha (rectangle-alpha))

         #:show-outliers? (show-outliers? #t)
         #:outlier-color (outlier-color (point-color))
         #:outlier-sym (outlier-sym (point-sym))
         #:outlier-fill-color (outlier-fill-color 'auto)
         #:outlier-size (outlier-size (point-size))
         #:outlier-line-width (outlier-line-width (point-line-width))
         #:outlier-alpha (outlier-alpha (point-alpha))

         #:show-whiskers? (show-whiskers? #t)
         #:whiskers-color (whiskers-color (line-color))
         #:whiskers-width (whiskers-width (line-width))
         #:whiskers-style (whiskers-style (line-style))
         #:whiskers-alpha (whiskers-alpha (line-alpha))

         #:show-median? (show-median? #t)
         #:median-color (median-color (line-color))
         #:median-width (median-width (line-width))
         #:median-style (median-style (line-style))
         #:median-alpha (median-alpha (line-alpha))
         )
  (match-define (bwdata q1 median q3 lower-whisker upper-whisker outliers) data)
  (define half-width (* 1/2 (- 1 gap)))
  (define quater-width (* 1/4 (- 1 gap)))
  (define maybe-invert (if invert? (lambda (x y) (vector y x)) vector))
  (list
   (rectangles
    (list (maybe-invert (ivl (- x half-width) (+ x half-width)) (ivl q1 q3)))
    #:color box-color
    #:style box-style
    #:line-color box-line-color
    #:line-width box-line-width
    #:line-style box-line-style
    #:alpha box-alpha)
   ;; Median line
   (if show-median?
       (lines (list (maybe-invert (- x half-width) median)
                    (maybe-invert (+ x half-width) median))
              #:color median-color
              #:width median-width
              #:style median-style
              #:alpha median-alpha)
       null)

   (if show-whiskers?
       (list
        (lines (list (maybe-invert x lower-whisker) (maybe-invert x q1)
                     (vector +nan.0 +nan.0)
                     (maybe-invert x q3) (maybe-invert x upper-whisker))
               #:color whiskers-color
               #:width whiskers-width
               #:style whiskers-style
               #:alpha whiskers-alpha)
        (lines (list
                (maybe-invert (- x quater-width) lower-whisker)
                (maybe-invert (+ x quater-width) lower-whisker)
                (vector +nan.0 +nan.0)
                (maybe-invert (- x quater-width) upper-whisker)
                (maybe-invert (+ x quater-width) upper-whisker))
               #:color whiskers-color
               #:width whiskers-width
               #:style 'solid
               #:alpha whiskers-alpha))
       null)

   (if show-outliers?
       (points (for/list ([o (in-list outliers)]) (maybe-invert x o))
               #:color outlier-color
               #:sym outlier-sym
               #:fill-color outlier-fill-color
               #:size outlier-size
               #:line-width outlier-line-width
               #:alpha outlier-alpha)
       null)))

(define (make-box-plot-ticks start-x labels)
  (define end-x (+ start-x (length labels)))
  (ticks
   (lambda (low high)
     (for/list ([x (in-range start-x (+ start-x (length labels)))]
                #:when (and (>= x low) (<= x high)))
       (printf "pretick for ~a~%" x)
       (pre-tick x #t)))
   (lambda (low hight pre-ticks)
     (for/list ([t (in-list pre-ticks)])
       #;(list-ref labels (exact-truncate (pre-tick-value t)))
       (~a (pre-tick-value t))
       ))))

(define (make-box-plot df series
                       #:name (name series)
                       #:slot (slot 0)
                       #:weight-series (wseries "timer")
                       #:iqr-scale (iqr-scale 1.5))
  (define-values (vs ws)
    (if wseries
        (for/fold ([vs '()] [ws '()])
                  ([(sample weight) (in-data-frame df series wseries)])
          (if (and sample weight (> sample 0))
              (values (cons sample vs) (cons weight ws))
              (values vs ws)))
        (for/fold ([vs '()] [ws #f])
                  ([sample (in-data-frame df series)])
          (if sample
              (values (cons sample vs) #f)
              (values vs #f)))))
  (define data (samples->bwdata vs ws))
  (printf "data: ~a~%" data)
  (parameterize ([plot-y-ticks no-ticks])
    (plot (box-and-whiskers-renderer 1 data #:show-median? #t #:show-outliers? #t
                                     #:whiskers-style 'short-dash
                                     #:invert? #t
                                     )
           #:y-min -5 #:y-max 5
           #:x-min -10 #:x-max 800
          )))


;; Usage notes:
;;
;; Find a cutoff power for a session id: (cutoff-power (sid->df) 0.005)
;;
;; call do-fixups with the desired cutoff power.
;;
;;
;; NOTE: AL2 will need to be restarted to see the effects.
