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
         plot
         plot-container
         plot-container/hover-util
         "../fmt-util.rkt")

(define outlier-plot%
  (class object%
    (init-field df series [iqr-scale 4.0])
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
         #:x-min min-x #:y-min min-y #:y-max max-y)))

    (define bnw #f)    ; box-and-whiskers data produced by `samples->bnw-data`
    (define outliers #())               ; list of outlier points based on BNW
    (define bnw-renderer #f)
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
      (set! outlier-renderer
            (points outliers
                    #:color "coral"
                    #:sym 'circle8
                    #:size 5
                    #:line-width 3)))

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
           "Outlier point"
           (format "Time: ~a" (duration->string duration))
           (format "~a: ~a" (send md name) (exact-round value)))
          (hover-label
           x y
           (format "Time: ~a" (duration->string duration))
           (format "~a: ~a" (send md name) (exact-round value)))))

    ;; NOTE: this cannot be a method (e.g. define/private as it is used as a
    ;; callback
    (define (hover-callback snip event x y)
      (define renderers (list bnw-renderer outlier-renderer))
      (when (and (good-hover? snip x y event) (> x 0))
        (let-values ([(outlier distance) (closest-outlier x y)])
          (if (and outlier (< distance 1e-2))
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
      (send snip set-overlay-renderers (flatten renderers)))

    (set-iqr-scale iqr-scale)
    (send snip set-overlay-renderers (flatten (list bnw-renderer outlier-renderer)))
    (send snip set-mouse-event-callback hover-callback)

    (define/public (get-snip) snip)

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

    (define toplevel-window (make-toplevel-dialog #f))

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

    (define plot #f)

    (define (on-close-dashboard)
      (void))

    (define/private (load-data db sid)
      (set! database db)
      (set! df (session-df db sid))
      (define sinfo (get-session-info sid db))
      (send dashboard-contents begin-container-sequence)
      (when sinfo
        (send headline set-pict (and sinfo (pp-session-info/pict sinfo))))
      (set! plot (new outlier-plot% [df df] [series "pwr"]))
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
