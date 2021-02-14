#lang racket/base
;; power-spikes.rkt -- clear power spikes and re-calculate power related
;; metrics for an activity
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require "dashboard-common.rkt"
         "../session-df/session-df.rkt"
         "../session-df/series-metadata.rkt"
         "../utilities.rkt"
         "../widgets/box-and-whiskers.rkt"
         data-frame
         db/base
         racket/class
         racket/gui/base
         racket/contract
         racket/match
         racket/math
         racket/list
         racket/format
         plot
         plot-container
         plot-container/hover-util
         "../fmt-util.rkt")

(define outlier-plot%
  (class object%
    (init-field df series [iqr-scale 4.0] [width 100] [height 100])
    (super-new)

    (define md (find-series-metadata series))
    (define samples
      (df-select df series #:filter (lambda (sample) (and sample (> sample 0)))))
    (define plot-data (df-select* df "elapsed" series #:filter valid-only))

    (define max-x (df-ref df (sub1 (df-row-count df)) "elapsed"))
    (define box-plot-gap (* max-x 0.08))
    (define box-plot-room (* max-x 0.05))
    (define box-plot-x (- (+ box-plot-room (* 0.5 box-plot-gap))))
    (define min-x (- (+ box-plot-room box-plot-room box-plot-gap)))
    (define min-y 0)
    (define max-y (* 1.1 (for/fold ([m 0])
                                   ([p (in-vector samples)])
                           (max m p))))

    (define snip
      (parameterize ([plot-y-label (send md axis-label)]
                     [plot-x-label #f]
                     [plot-x-ticks (time-ticks #:formats '("~H:~M"))])
        (plot-snip
         (list
          (lines plot-data #:color (send md plot-color) #:width 1.5))
         #:x-min min-x #:y-min min-y #:y-max max-y
         #:width width #:height height)))

    (define bnw #f)    ; box-and-whiskers data produced by `samples->bnw-data`
    (define outliers #())               ; list of outlier points based on BNW
    (define bnw-renderer #f)
    (define bnw-renderer-highlighted #f)
    (define outlier-renderer #f)

    (define/public (set-iqr-scale new-iqr-scale)
      (set! iqr-scale new-iqr-scale)
      (set! bnw (samples->bnw-data samples #:iqr-scale iqr-scale))
      (define upper-limit (bnw-data-uppwer-whisker bnw))
      (set! outliers
            (df-select*
             df "elapsed" series
             #:filter (lambda (v)
                        (match-define (vector e p) v)
                        (and e p (> p upper-limit)))))
      (set! bnw-renderer
            (box-and-whiskers
             bnw
             #:x box-plot-x
             #:gap box-plot-gap
             #:invert? #f

             #:box-color "Light Sky Blue"
             #:box-line-color "Steel Blue"
             #:box-line-width 1.5

             #:whiskers-color "Slate Gray"
             #:whiskers-width 2.0
             #:whiskers-style 'short-dash

             #:median-color "Dark Red"
             #:median-width 3.0

             #:outlier-color "Salmon"
             #:outlier-sym 'fullcircle4
             ;; #:outlier-size (* 2.0 (point-size))
             #:outlier-line-width 1.5
             ))
      (set! bnw-renderer-highlighted
            (box-and-whiskers
             bnw
             #:x box-plot-x
             #:gap box-plot-gap
             #:invert? #f

             #:box-color "Sky Blue"
             #:box-line-color "Royal Blue"
             #:box-line-width 2.25

             #:whiskers-color "Dark Slate Gray"
             #:whiskers-width 3.0
             #:whiskers-style 'short-dash

             #:median-color "Red"
             #:median-width 4.5

             #:outlier-color "Orange"
             #:outlier-sym 'fullcircle4
             #:outlier-size (* 1.5 (point-size))
             #:outlier-line-width 2.25
             ))
      (set! outlier-renderer
            (points outliers
                    #:color "coral"
                    #:sym 'circle8
                    #:size 5
                    #:line-width 3))

      (when snip
        (send snip set-overlay-renderers
              (flatten (list bnw-renderer outlier-renderer)))))

    (define/private (closest-outlier x y)
      (for/fold ([point #f]
                 [distance #f])
                ([outlier (in-vector outliers)])
        (match-define (vector e p) outlier)
        (define d (let ([dx (/ (- e x) max-x)]
                        [dy (/ (- p y) max-y)])
                    (sqrt (+ (* dx dx) (* dy dy)))))
        (if (or (not distance) (< d distance))
            (values outlier d)
            (values point distance))))

    (define/private (make-hover-label x y point #:outlier? [outlier? #f])
      (match-define (vector duration value) point)
      (if outlier?
          (hover-label
           x y
           (make-hover-badge
            `(("Time" ,(duration->string duration))
              (,(send md name) ,(~a (exact-round value)))
              ("Outlier point"))))
          (hover-label
           x y
           (make-hover-badge
            `(("Time" ,(duration->string duration))
              (,(send md name) ,(~a (exact-round value))))))))

    ;; NOTE: this cannot be a method (e.g. define/private as it is used as a
    ;; callback
    (define (hover-callback snip event x y)
      (define renderers (list bnw-renderer outlier-renderer))
      (cond ((not (good-hover? snip x y event)) (void))
            ((> x 0)
             (let-values ([(outlier distance) (closest-outlier x y)])
               (if (and outlier (< distance 1e-2))
                   ;; Mouse is over (close to) an outlier point, highlight it
                   (set! renderers
                         (append
                          renderers
                          (list
                           (points (list outlier)
                                   #:color "indianred"
                                   #:sym 'fullcircle8
                                   #:size 6
                                   #:line-width 5)
                           (make-hover-label x y outlier #:outlier? #t))))
                   ;; Mouse is just somewhere over the plot, so we highlight
                   ;; the current value
                   (let ([index (df-index-of df "elapsed" x)])
                     (when (and index (< index (df-row-count df)))
                       (define point (df-ref* df index "elapsed" series))
                       (when (vector-ref point 1)
                         (set! renderers
                               (append
                                renderers
                                (list
                                 (hover-vrule (vector-ref point 0))
                                 (make-hover-label x y point))))))))))
            ((and bnw
                  (> x (- box-plot-x (* box-plot-gap 1/2)))
                  (< x (+ box-plot-x (* box-plot-gap 1/2))))
             ;; Mouse is over the box-and-whiskers section
             (match-define (bnw-data q1 median q3 lower-whisker uppwer-whisker o) bnw)
             (set! renderers
                   (append
                    (remove bnw-renderer renderers)
                    (list
                     bnw-renderer-highlighted
                     (hrule q1 #:width 1.5 #:style 'long-dash #:color "Slate Gray")
                     (hrule median #:width 1.5 #:style 'long-dash #:color "Slate Gray")
                     (hrule q3 #:width 1.5 #:style 'long-dash #:color "Slate Gray")
                     (hrule uppwer-whisker #:width 1.5 #:style 'long-dash #:color "Slate Gray")
                     (hover-label
                      x y
                      (make-hover-badge
                       `(("Q1 (25%)" ,(~a (exact-round q1)))
                         ("Median (50%)" ,(~a (exact-round median)))
                         ("Q3 (75%)" ,(~a (exact-round q3)))
                         ("Upper Cutoff" ,(~a (exact-round uppwer-whisker)))
                         ("Outlier Count" ,(~a (length o)))))))))))
      (send snip set-overlay-renderers (flatten renderers)))

    (set-iqr-scale iqr-scale)
    (send snip set-mouse-event-callback hover-callback)

    (define/public (get-snip) snip)
    (define/public (get-cutoff) (bnw-data-uppwer-whisker bnw))
    (define/public (get-outlier-count) (length (bnw-data-outliers bnw)))

    ))

(define power-spikes-dashboard%
  (class object%
    (init-field [min-width 1000] [min-height 625])
    (super-new)

    (define database #f)
    (define df #f)

    (define/private (make-toplevel-dialog parent)
      (new
       (class dialog% (init) (super-new)
         (define/augment (on-close) (on-close-dashboard)))
       [label "FTHR Analysis Dashboard"]
       [min-width min-width]
       [min-height min-height]
       [parent (if parent (send parent get-top-level-window) #f)]))

    (define message-font
      (send the-font-list find-or-create-font 12 'default 'normal 'normal))

    (define toplevel-window (make-toplevel-dialog #f))

    (define plot #f)

    (define dashboard-contents
      (new vertical-panel%
           [parent toplevel-window]
           [spacing 5]
           [border 5]
           [alignment '(left top)]))

    (define headline
      (new pict-canvas%
           [parent dashboard-contents]
           [alignment '(left center)]
           [stretchable-height #f]))

    (define plot-container
      (new plot-container%
           [parent dashboard-contents]))

    (define controls-group-box
      (new group-box-panel%
           [parent dashboard-contents]
           [label "Outlier Filtering"]
           [alignment '(center center)]
           [stretchable-height #f]
           [border 20]
           [spacing 10]))

    (define controls-pane
      (new horizontal-pane%
           [parent controls-group-box]
           [spacing 10]))

    (define iqr-scale-message
      (new message%
           [label "IQR Scale:"]
           [parent controls-pane]))

    (define iqr-scale-value
      (new message%
           [label "      "]
           [parent controls-pane]
           [font message-font]))

    (define iqr-scale-slider
      (new slider%
           [label ""]
           [parent controls-pane]
           [min-value 15]
           [max-value 100]
           [init-value 40]
           [style '(horizontal plain)]
           [callback (lambda (c e) (on-iqr-scale c e))]))

    (define cutoff-message
      (new message%
           [label "Cutoff Value:"]
           [parent controls-pane]))

    (define cutoff-value
      (new message%
           [label "      "]
           [parent controls-pane]
           [font message-font]))

    (define outlier-count-message
      (new message%
           [label "Outlier Count:"]
           [parent controls-pane]))

    (define outlier-count
      (new message%
           [label "      "]
           [parent controls-pane]
           [font message-font]))

    (define/private (on-iqr-scale control event)
      (define v (/ (send control get-value) 10.0))
      (when plot
        (send plot set-iqr-scale v)
        (send iqr-scale-value set-label (~r v #:precision 2))
        (send cutoff-value set-label (~a (exact-round (send plot get-cutoff))))
        (send outlier-count set-label (~a (exact-round (send plot get-outlier-count))))))

    (define (on-close-dashboard)
      (void))

    (define/private (load-data db sid)
      (set! database db)
      (set! df (session-df db sid))
      (define sinfo (get-session-info sid db))
      (send dashboard-contents begin-container-sequence)
      (when sinfo
        (send headline set-pict (and sinfo (pp-session-info/pict sinfo))))
      (define iqr-scale 4.0)            ; TODO: read from config
      (define-values (w h) (send plot-container cell-dimensions 1))
      (set! plot (new outlier-plot%
                      [df df]
                      [series "pwr"]
                      [iqr-scale iqr-scale]
                      [width w]
                      [height h]))
      (send iqr-scale-slider set-value (exact-round (* iqr-scale 10.0)))
      (send iqr-scale-value set-label (~r iqr-scale #:precision 2))
      (send cutoff-value set-label (~a (exact-round (send plot get-cutoff))))
      (send outlier-count set-label (~a (exact-round (send plot get-outlier-count))))
      (send plot-container set-snip (send plot get-snip))
      (send dashboard-contents end-container-sequence))

    (define/public (show-dashboard parent db sid)
      (let ((old-toplevel toplevel-window))
        (let ((toplevel (if parent (make-toplevel-dialog parent) toplevel-window)))
          (send dashboard-contents reparent toplevel)
          (set! toplevel-window toplevel))
        (thread/dbglog (lambda () (load-data db sid)))
        (send toplevel-window show #t) ; will block until finish-dialog is called
        (void)))

    ))

(define (show-power-spikes-dashboard toplevel database sid)
  (define dashboard (new power-spikes-dashboard%))
  (send dashboard show-dashboard toplevel database sid))

(provide/contract
 (show-power-spikes-dashboard (-> (or/c #f (is-a?/c top-level-window<%>))
                                  connection?
                                  exact-positive-integer? any/c)))
