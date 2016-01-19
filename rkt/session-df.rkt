#lang racket/base

;; session-df.rkt --create a data-frame% from a session's trackpoints, plus
;; utilities to plot graphs.
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

(require db
         racket/class
         racket/match
         racket/math
         racket/sequence
         racket/vector
         math/statistics
         plot
         "al-profiler.rkt"
         "data-frame.rkt"
         "fmt-util.rkt"
         "sport-charms.rkt")

(provide make-session-data-frame
         extract-data ds-stats add-verticals get-lap-extents get-plot-y-range combine-y-range
         make-plot-renderer make-box-renderer
         make-plot-renderer/factors make-plot-renderer/swim-stroke)


;;.............................................. make-session-data-frame ....

(define fetch-trackpoins
  "select T.timestamp as timestamp,
       T.position_lat as lat,
       T.position_long as lon,
       round(T.altitude, 1) as alt,
       round(T.corrected_altitude, 1) as calt,
       T.distance as dst,
       T.cadence as cad,
       T.speed as spd,
       T.heart_rate as hr,
       T.power as pwr,
       T.vertical_oscillation as vosc,
       T.stance_time as gct,
       T.stance_time_percent as pgct,
       T.left_right_balance as lrbal,
       T.left_torque_effectiveness as lteff,
       T.right_torque_effectiveness as rteff,
       T.left_pedal_smoothness as lpsmth,
       T.right_pedal_smoothness as rpsmth,
       T.left_pco as lpco,
       T.right_pco as rpco,
       T.left_pp_start as lpps,
       T.left_pp_end as lppe,
       T.right_pp_start as rpps,
       T.right_pp_end as rppe,
       T.left_ppp_start as lppps,
       T.left_ppp_end as lpppe,
       T.right_ppp_start as rppps,
       T.right_ppp_end as rpppe
  from A_TRACKPOINT T, A_LENGTH L, A_LAP P
 where T.length_id = L.id
   and L.lap_id = P.id
   and P.session_id = ?
 order by T.timestamp")

(define fetch-trackpoins/swim
  "select L.start_time as timestamp,
       (select max(T.distance) from A_TRACKPOINT T where T.length_id = L.id) as dst,
       SS.total_timer_time as duration,
       SS.avg_speed as spd,
       SS.avg_cadence as cad,
       SS.swim_stroke_id as swim_stroke,
       SS.total_cycles as strokes
  from A_LENGTH L, A_LAP P, SECTION_SUMMARY SS
 where L.lap_id = P.id
   and L.summary_id = SS.id
   and P.session_id = ?
 order by L.start_time")

(define fetch-sport
  "select sport_id, sub_sport_id from A_SESSION where id = ?")

(define fetch-lap-timestamps
  "select P.start_time
  from A_LAP P
 where P.session_id = ?
 order by P.start_time")

;; Create a data-frame% from the session's trackpoints.  Some data series come
;; from the database (e.g. heart rate), some are calculated (e.g. heart rate
;; zone).
(define (make-session-data-frame db session-id)

  (define sport
    (let ([row (query-maybe-row db fetch-sport session-id)])
      (match-define (vector sport-id sub-sport-id) row)
      (vector (if (sql-null? sport-id) #f sport-id)
              (if (sql-null? sub-sport-id) #f sub-sport-id))))
      
  (define is-lap-swim?
    (when sport
      (match-define (vector sport-id sub-sport-id) sport)
      (and (equal? sport-id 5) (equal? sub-sport-id 17))))
    
  (define df
    (make-data-frame-from-query
     db
     (if is-lap-swim? fetch-trackpoins/swim fetch-trackpoins)
     session-id))
  
  (send df put-property 'is-lap-swim? is-lap-swim?)
  (send df put-property 'sport sport)
  (send df put-property 'session-id session-id)

  (let ([laps (query-list db fetch-lap-timestamps session-id)])
    (send df put-property 'laps (list->vector laps)))

  (when (send df contains? "timestamp")
    (send (send df get-series "timestamp") set-sorted #t))

  (add-timer-series df)
  (add-elapsed-series df)
  (add-distance-series df)
  (add-speed-series df)
  (add-pace-series df)
  (add-speed-zone-series df)
  (add-grade-series df)
  (add-hr-pct-series df)
  (add-hr-zone-series df)
  (add-stride-series df)
  (add-vratio-series df)
  (add-power-zone-series df)
  (add-lppa-series df)                  ; left power phase angle
  (add-lpppa-series df)                 ; left peak power phase angle
  (fixup-pp-series df "lpps")
  (fixup-pp-series df "rpps")
  (fixup-pp-series df "lppps")
  (fixup-pp-series df "rppps")
  (add-rppa-series df)
  (add-rpppa-series df)
  (when is-lap-swim?
    (add-swolf-series df))

  (unless is-lap-swim?
    (send df set-default-weight-series "timer"))

  df)

(define (add-timer-series df)
  (when (send df contains? "timestamp" "dst")
    (define stop-points '())
    
    (send df add-derived-series
          "timer"
          '("timestamp" "dst")
          (let ((timer 0))
            (lambda (prev-val val)
              (when prev-val
                (match-define (vector ptimestamp pdst) prev-val)
                (match-define (vector timestamp dst) val)
                (define dt (- timestamp ptimestamp))
                (define dd (if (and dst pdst) (- dst pdst) 0))
                (if (and (> dt 10) (< dd 0.5))
                    ;; Stop point
                    (set! stop-points (cons ptimestamp stop-points))
                    (set! timer (+ timer dt))))
              timer)))

    (send (send df get-series "timer") set-sorted #t)
    (send df put-property 'stop-points (reverse stop-points))))

(define (add-elapsed-series df)
  (when (send df contains? "timestamp")
    (define timestamp0
      (vector-ref (send df select "timestamp") 0))
    (when (and (send df get-property 'is-lap-swim?)
               (send df contains? "duration"))
      ;; Lap swimming timestamp entries are recorded at the end of each
      ;; length, so the first timestamp is at the timestamp of the first
      ;; length minus the duration of the length.
      (set! timestamp0
            (- timestamp0
               (exact-round (vector-ref (send df select "duration") 0)))))
    (send df add-derived-series
          "elapsed"
          '("timestamp")
          (lambda (val)
            (define timestamp (vector-ref val 0))
            (- timestamp timestamp0)))
    (send (send df get-series "elapsed") set-sorted #t)))

(define (add-distance-series df)

  (define (distance-km val)
    (define dst (vector-ref val 0))
    (if dst (m->km dst) #f))

  (define (distance-mi val)
    (define dst (vector-ref val 0))
    (if dst (m->mi dst) #f))

  (define (distance-yards val)
    (define dst (vector-ref val 0))
    (if dst (m->yd dst) #f))

  (define (distance-meters val)
    (vector-ref val 0))

  (when (send df contains? "dst")
    ;; NOTE: the dst series contains #f's on lap swim activities.  Since this
    ;; series is used as a bases for the grahs, we patch the distance series
    ;; to contain valid values.
    (define distance
      (send df map
            '("dst")
            (if (send df get-property 'is-lap-swim?)
                (if (eq? (al-pref-measurement-system) 'metric)
                    distance-meters distance-yards)
                (if (eq? (al-pref-measurement-system) 'metric)
                    distance-km distance-mi))))

    ;; Patch first element
    (or (vector-ref distance 0)
        (vector-set! distance 0 0))
    ;; Patch the rest of the series
    (for ([idx (in-range 1 (vector-length distance))])
      (or (vector-ref distance idx)
          (vector-set! distance idx (vector-ref distance (- idx 1)))))

    (send df add-series
          (new data-series% [name "distance"] [data distance]))
    (send (send df get-series "distance") set-sorted #t)))
  
(define (add-speed-series df)

  (define (speed-km/h val)
    (match-define (vector spd) val)
    (if spd (m/s->km/h spd) spd))
  (define (speed-mi/h val)
    (match-define (vector spd) val)
    (if spd (m/s->mi/h spd) #f))
  (when (send df contains? "spd")
    (send df add-derived-series
          "speed"
          '("spd")
          (if (eq? (al-pref-measurement-system) 'metric)
              speed-km/h speed-mi/h))))

(define (add-pace-series df)

  (define (pace-sec/km val)
    (match-define (vector spd) val)
    (if (and spd (> spd 0.6)) (m/s->sec/km spd) #f))

  (define (pace-sec/mi val)
    (match-define (vector spd) val)
    (if (and spd (> spd 0.6)) (m/s->sec/mi spd) #f))

  (define (pace-sec/100m val)
    (match-define (vector spd) val)
    (if (and spd (> spd 0.6)) (m/s->sec/100m spd) #f))

  (define (pace-sec/100yd val)
    (match-define (vector spd) val)
    (if (and spd (> spd 0.6)) (m/s->sec/100yd spd) #f))

  (when (send df contains? "spd")
    (send df add-derived-series
          "pace"
          '("spd")
          (if (send df get-property 'is-lap-swim?)
              (if (eq? (al-pref-measurement-system) 'metric)
                  pace-sec/100m pace-sec/100yd)
              (if (eq? (al-pref-measurement-system) 'metric)
                  pace-sec/km pace-sec/mi)))))

(define (add-speed-zone-series df)
  (define sid (send df get-property 'session-id))
  (define zones (get-session-sport-zones sid 2))
  (when (and zones (send df contains? "spd"))
    (send df add-derived-series
          "speed-zone"
          '("spd")
          (lambda (val)
            (match-define (vector spd) val)
            (if spd (val->zone spd zones) #f)))))

(define (add-grade-series df)
  (define series (if (send df contains? "calt")
                     "calt"
                     (if (send df contains? "alt")
                         "alt"
                         #f)))
  (when (and series (send df contains? "dst"))
    (send df add-derived-series
          "grade"
          (list "dst" series)
          (lambda (prev-val val)
            (if prev-val
                (let ()
                  (match-define (vector pdst palt) prev-val)
                  (match-define (vector dst alt) val)
                  (if (and pdst palt dst alt)
                      (* 100 (/ (- alt palt) (- dst pdst)))
                      #f))
                0)))))

(define (add-hr-pct-series df)
  (define sid (send df get-property 'session-id))
  (define zones (get-session-sport-zones sid 1))
  (when (and zones (send df contains? "hr"))
    (send df add-derived-series
          "hr-pct"
          '("hr")
          (lambda (val)
            (match-define (vector hr) val)
            (if hr (val->pct-of-max hr zones) #f)))))

(define (add-hr-zone-series df)
  (define sid (send df get-property 'session-id))
  (define zones (get-session-sport-zones sid 1))
  (when (and zones (send df contains? "hr"))
    (send df add-derived-series
          "hr-zone"
          '("hr")
          (lambda (val)
            (match-define (vector hr) val)
            (if hr (val->zone hr zones) #f)))))

(define (add-stride-series df)

  (define (stride-m val)
    (match-define (vector spd cad) val)
    (if (and spd cad (> cad 0))
        (/ (* spd 60) (* 2 cad))
        #f))

  (define (stride-ft val)
    (let ((s (stride-m val)))
      (if s
          (m->ft s)
          #f)))
  
  (when (send df contains? "spd" "cad")
    (send df add-derived-series
          "stride"
          '("spd" "cad")
          (if (eq? (al-pref-measurement-system) 'metric)
              stride-m
              stride-ft))))

(define (add-vratio-series df)

  (define (stride spd cad)
    (if (and spd cad (> cad 0))
        (/ (* spd 60) (* 2 cad))
        #f))
  
  (when (send df contains? "spd" "cad" "vosc")
    (send df add-derived-series
          "vratio"
          '("spd" "cad" "vosc")
          (lambda (val)
            (match-define (vector spd cad vosc) val)
            (if (and spd cad vosc)
                (let ((st (stride spd cad)))
                  (if (and st (> st 0)) (* 100.0 (/ vosc (* st 1000))) #f))
                #f)))))

(define (add-power-zone-series df)
  (define sid (send df get-property 'session-id))
  (define zones (get-session-sport-zones sid 3))
  (when (and zones (send df contains? "pwr"))
    (send df add-derived-series
          "pwr-zone"
          '("pwr")
          (lambda (val)
            (match-define (vector pwr) val)
            (if pwr (val->zone pwr zones) #f)))))

(define (add-lppa-series df)
  (when (send df contains? "lpps" "lppe")
    (send df add-derived-series
          "lppa"
          '("lpps" "lppe")
          (lambda (val)
            (match-define (vector start end) val)
            (if (and start end)
                (let ((angle (- end start)))
                  (if (< angle 0) (+ angle 360) angle))
                #f)))))

;; Change the angle range form 0-360 to -180 .. 180.  This makes it look nicer
;; when angles are arround 0, as they will transition, for example, between
;; -20 and 20 degrees instead of jumping betwrrn 20 and 340
(define (fixup-pp-series df series-name)
  (when (send df contains? series-name)
    (send df add-derived-series
          series-name
          (list series-name)
          (lambda (val)
            (define a (vector-ref val 0))
            (if a (if (> a 180.0) (- a 360) a) #f)))))

(define (add-lpppa-series df)
  (when (send df contains? "lppps" "lpppe")
    (send df add-derived-series
          "lpppa"
          '("lppps" "lpppe")
          (lambda (val)
            (match-define (vector start end) val)
            (if (and start end)
                (let ((angle (- end start)))
                  (if (< angle 0) (+ angle 360) angle))
                #f)))))

(define (add-rppa-series df)
  (when (send df contains? "rpps" "rppe")
    (send df add-derived-series
          "rppa"
          '("rpps" "rppe")
          (lambda (val)
            (match-define (vector start end) val)
            (if (and start end)
                (let ((angle (- end start)))
                  (if (< angle 0) (+ angle 360) angle))
                #f)))))

(define (add-rpppa-series df)
  (when (send df contains? "rppps" "rpppe")
    (send df add-derived-series
          "rpppa"
          '("rppps" "rpppe")
          (lambda (val)
            (match-define (vector start end) val)
            (if (and start end)
                (let ((angle (- end start)))
                  (if (< angle 0) (+ angle 360) angle))
                #f)))))

(define (add-swolf-series df)
  (when (send df contains? "duration" "strokes")
    (send df add-derived-series
          "swolf"
          '("duration" "strokes")
          (lambda (val)
            (match-define (vector duration strokes) val)
            (if (and duration strokes)
                (exact-round (+ duration strokes))
                #f)))))


;;................................................................ other ....

;; Extract data from DATA-FRAME corresponding to X-AXIS and Y-AXIS. The result
;; is intended for plotting on a graph, not for further processing.  A
;; sequence of three values is returned: x value, y value and timestamp (the
;; values are packed in a vector).  Some processing is done on the data, based
;; on the properties of X-AXIS and Y-AXIS:
;;
;; * missing Y values are replaced with the Y-AXIS default missing value, or
;;   dropped if no such value is specified.
;; 
;; * data is filtered if Y-AXIS specifies it (filter-width will be used in
;; that case.
;;
;; * Y values will drop to 0 at stop points if the X-AXIS requests it (this
;; produces nicer graphs).
;;
(define (extract-data data-frame x-axis y-axis (filter-width 0))
  (let ((xseries (send x-axis get-series-name))
        (yseries (send y-axis get-series-name))
        (missing-value (send y-axis get-missing-value))
        (should-filter? (send y-axis should-filter?))
        (base-filter-width (send x-axis get-filter-width))
        (stop-detection? (send x-axis has-stop-detection?)))
    (define data
      (send data-frame select* xseries yseries "timestamp"
            #:filter (lambda (v)
                       (match-define (vector x y t) v)
                       (and x (or y missing-value) t))))
    ;; Missing values items are just selected but they are still #f, replace
    ;; them now.
    (when missing-value
      (for ([d data])
        (unless (vector-ref d 1)
          (vector-set! d 1 missing-value))))

    ;; Filter the data in place, if required.
    (when (and should-filter? (> filter-width 0) (> (vector-length data) 0))
      (let ((fw (* base-filter-width filter-width))
            (px (vector-ref (vector-ref data 0) 0))
            (py (vector-ref (vector-ref data 0) 1)))
        (for ((idx (in-range 1 (vector-length data))))
          (let* ((v (vector-ref data idx))
                 (x (vector-ref v 0))
                 (y (vector-ref v 1))
                 (dt (- x px))
                 (alpha (/ dt (+ dt fw)))
                 (ny (+ (* alpha y) (* (- 1 alpha) py))))
            (vector-set! v 1 ny)
            (set! py ny)
            (set! px x)))))

    ;; Mark stop points by setting the values around the stop point to 0. stop
    ;; points are stored in the data-frame when it is loaded up by
    ;; `make-session-data-frame'.  This is done after any filtering to make
    ;; the "edges" on the graph sharp even when filtering with large widths.
    (when stop-detection?
      (let ([stop-points (send data-frame get-property 'stop-points)])
        (for ([point stop-points])
          (let ([idx (bsearch data point #:key (lambda (v) (vector-ref v 2)))])
            (when idx
              (vector-set! (vector-ref data idx) 1 0)
              (when (< (+ idx 1) (vector-length data))
                (vector-set! (vector-ref data (+ idx 1)) 1 0)))))))

    data))

;; Compute statistics on the Y value of a data-series (as produced by
;; `extract-data').  This is faster than doing the statistics on the
;; data-frame itself.
(define (ds-stats data-series)
  (for/fold ([stats empty-statistics])
            ([item data-series])
    (update-statistics stats (vector-ref item 1))))

;; Duplicate points in DATA (as produced by `extract-data') so that the graphs
;; have a "boxed" appearance.  This is used in all the swim graphs where the
;; data points are for individual lenghts.
(define (add-verticals data)
  (for*/vector ([idx (in-range (vector-length data))]
                [val (in-sequences
                      (if (> idx 0)
                          (let ()
                            (match-define (vector x y t) (vector-ref data (- idx 1)))
                            (match-define (vector nx ny nt) (vector-ref data idx))
                            (list
                             (vector x ny nt)
                             (vector-ref data idx)))
                          (let ()
                            (match-define (vector x y t) (vector-ref data 0))
                            (list
                             (vector 0 y t)
                             (vector x y t)))))])
    val))

;; Return the X values in DATA-SERIES (as produced by `extract-data') that
;; correspond to the start and end of the lap LAP-NUM.  The timestamp of the
;; lap is matched against the third value in DATA-SERIES.
(define (get-lap-extents data-series data-frame lap-num)
  (let* ((laps (send data-frame get-property 'laps))
         (start (vector-ref laps lap-num))
         (end (if (< (+ lap-num 1) (vector-length laps))
                  (vector-ref laps (+ lap-num 1))
                  #f))
         (start-idx
          (or
           (bsearch data-series start #:key (lambda (v) (vector-ref v 2)))
           0))
         (end-idx
          (if end
              (or (bsearch data-series end #:key (lambda (v) (vector-ref v 2)))
                  (- (vector-length data-series) 1))
              (- (vector-length data-series) 1))))
    (cons (vector-ref (vector-ref data-series start-idx) 0)
          (vector-ref (vector-ref data-series end-idx) 0))))

;; Determine the min/max y values for a plot based on STATS (as collected by
;; `ds-stats') and the Y-AXIS.
(define (get-plot-y-range stats y-axis)
  (define high #f)
  (define low #f)
  (let ((range (send y-axis get-y-range)))
    (when range
      (set! low (car range))
      (set! high (cdr range))))
  (define mean (statistics-mean stats))
  (define stddev (statistics-stddev stats))
  (define width 3.0)
  (unless low
    (let ((v (statistics-min stats)))
      (unless (or (nan? v) (nan? mean) (nan? stddev))
        (set! low (* 0.9 (max v (- mean (* width stddev))))))))
  (unless high
    (let ((v (statistics-max stats)))
      (unless (or (nan? v) (nan? mean) (nan? stddev))
        (set! high (* 1.05 (min v (+ mean (* width stddev))))))))
  (cons low high))

;; Combine two Y ranges (as produced by `get-plot-y-range')
(define (combine-y-range yr1 yr2)
  (match-define (cons low1 high1) yr1)
  (match-define (cons low2 high2) yr2)
  (cons (min low1 low2) (max high1 high2)))

;; Create a plot renderer which plots DATA-SERIES (a list of [X Y]) using
;; lines (a "normal" plot).  Y-RANGE is either #f or a cons of the min and max
;; Y values of the graph.
;;
(define (make-plot-renderer data-series y-range
                            #:color (color #f)
                            #:width (width 2)
                            #:alpha (alpha #f)
                            #:label (label #f))
  (let ((kwd '()) (val '()))
    (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
    ;; WARNING: keywords used by keyword-apply have to be sorted

    ;; Use min and max y values for the graphs, if these are specified.
    ;; Otherwise, we let the plot library determine bounds.
    (when y-range
      (add-arg '#:y-min (car y-range))
      (add-arg '#:y-max (cdr y-range)))
    (when width (add-arg '#:width width))
    (when label (add-arg '#:label label))
    (when color (add-arg '#:color color))
    (when alpha (add-arg '#:alpha alpha))
    (keyword-apply lines kwd val data-series '())))

;; Create a plot renderer which plots a box with the bounds START, END,
;; MIN-VAL, MAX-VAL and it is ranged according to AXIS-DEF.  COLOR specifies
;; the color of the box, if #f.
;;
(define (make-box-renderer start end min-val max-val [color "red"])
  (let ((kwd '()) (val '()))
    (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
    ;; WARNING: keywords used by keyword-apply have to be sorted
    (add-arg '#:y-min min-val)
    (add-arg '#:y-max max-val)
    (when color (add-arg '#:color color))
    (add-arg '#:alpha 0.2)

    (keyword-apply
         lines-interval kwd val
         (list (vector start max-val) (vector end max-val))
         (list (vector start min-val) (vector end min-val))
         '())))

;; Create a plot renderer that plots different points in different colors.
;; DATA is produced by `group-samples/factor' and it is a hash table mapping a
;; factor to the points in that category.  FACTOR-COLORS is a list mapping the
;; factor to a color.
;;
(define (make-plot-renderer/factors data y-range factor-colors)
  (for/list ([elt factor-colors])
    (match-define (list factor color) elt)
    (let ((fdata (hash-ref data factor '())))
      (points fdata #:color color #:y-min (car y-range) #:y-max (cdr y-range)))))

;; Given `series', a sequence of values, return return a list of start, end
;; indexes for consecutive values in series.  For example, given '(1 1 1 2 2 3
;; 3 3), it will return (#(0 3) #(3 5) #(5 8)).  This is used to find the
;; ranges of the same swim stroke for coloring a swim graph.
;;
(define (find-ranges series)
  (if (= (sequence-length series) 0)
      '()
      (let ((start 0)
            (item (sequence-ref series 0)))
        ;; NOTE: index will start at 0, but we already removed the first
        ;; element
        (for/list ([(val index)
                    (in-indexed
                     (in-sequences (sequence-tail series 1) (list (gensym))))]
                   #:unless (equal? item val))
          (begin0
              (vector start (+ 1 index) item)
            (set! start (+ 1 index))
            (set! item val))))))

;; Make a plot renderer than plots DATA colorized by SWIM-STROKES.  DATA has
;; already been processed by `add-verticals', SWIM-STROKES is the swim stroke
;; series from the data frame.
;;
(define (make-plot-renderer/swim-stroke data swim-strokes)
  ;; NOTE: data has verticals added, swim-strokes does not.
  (for/list ([range (find-ranges swim-strokes)])
    (match-define (vector start end stroke) range)
    (let ((color (get-swim-stroke-color stroke))
          (first? (equal? start 0))
          (items (vector-copy
                  data
                  (* 2 start)
                  (min (vector-length data) (* 2 end)))))
      (if first?
          (make-plot-renderer items #f #:color color)
          (list
           (make-plot-renderer
            (vector-copy data (- (* 2 start) 1) (+ (* 2 start) 1))
            #f #:color "gray" #:width 0.7)
           (make-plot-renderer items #f #:color color))))))