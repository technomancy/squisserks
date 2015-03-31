#lang racket
(require 2htdp/image 2htdp/universe)
(require "draw.rkt")
(provide draw-fly keys-fly fly tick-fly)

(define (scene-offset x y)
  (let* ([one-scene (starify scene 25)]
         [scene (above (beside one-scene one-scene)
                       (beside one-scene one-scene))])
    (crop (modulo x width) (modulo y height) width height scene)))

(define ship (overlay (isosceles-triangle 30 15 "solid" "aquamarine")
                      (circle 20 "outline" "aquamarine")))

(define (draw-ship now)
  (match (now 'mode-data)
    [(hash-table ('x x) ('y y) ('dir dir) _ ...)
     (underlay (scene-offset x y)
               (rotate dir ship))]))

(define (draw-sun scene size color m)
  (let ([x (dict-ref m 'x)]
        [y (dict-ref m 'y)]
        [sun (circle size "solid" color)])
    (if (and (< (- width) (* 2 x) width)
             (< (- height) (* 2 y) height))
        (underlay/xy scene
                     (- (/ width 2) x (/ size 2))
                     (- (/ height 2) y (/ size 2)) sun)
        scene)))

(define (draw-fly now)
  (let ([system (dict-ref (now 'systems) (now 'system))])
    (draw-dash (draw-sun (draw-ship now) (system 'size) (system 'color)
                         (now 'mode-data))
               (format "x: ~s | y: ~s"
                       (dict-ref (now 'mode-data) 'x)
                       (dict-ref (now 'mode-data) 'y)))))



(define (tick-fly now)
  (match (now 'mode-data)
    [(hash-table ('x x) ('y y) ('dy dy) ('dx dx) _ ...)
     (now 'mode-data (hash-set* (now 'mode-data)
                                'x (round (+ x dx))
                                'y (round (+ y dy))))]))




(define engine 3)
(define max-speed 25)

(define (accel mode-data)
  (match mode-data
    [(hash-table ('dy dy) ('dx dx) ('dir dir) _ ...)
     (let ([ddx (* engine (sin (degrees->radians dir)))]
           [ddy (* engine (cos (degrees->radians dir)))])
       (if (< (+ dx dy) max-speed)
           (hash-set* mode-data
                      'dx (round (- dx ddx))
                      'dy (round (- dy ddy)))
           mode-data))]))

(define (keys-fly now pressed)
  (let* ([m (now 'mode-data)]
         [m (cond [(key=? pressed "up") (accel m)]
                  [(key=? pressed "left") (hash-update m 'dir (curryr + 10))]
                  [(key=? pressed "right") (hash-update m 'dir (curryr - 10))]
                  [else m])])
    (now 'mode-data m)))



(define (fly now)
  (let ([now (now 'mode 'fly)])
    (now 'mode-data #hash((x . 300) (y . 300)
                          (dx . 0) (dy . 0)
                          (dir . 0)))))
