#lang racket
(require 2htdp/image 2htdp/universe)
(require "draw.rkt")
(provide draw-fly keys-fly fly tick-fly)

(define (scene-offset x y)
  (let* ([one-scene (starify scene 25)]
         [scene (above (beside one-scene one-scene)
                       (beside one-scene one-scene))])
    (crop (modulo x width) (modulo y height) width height scene)))

(define ship (overlay (isosceles-triangle 60 30 "solid" "aquamarine")
                      (circle 40 "outline" "aquamarine")))

(define (draw-fly now)
  (match (now 'mode-data)
    [(hash-table ('x x) ('y y) ('dir dir) _)
     (underlay (scene-offset x y)
               (rotate dir ship))]))

(define (tick-fly now)
  (match (now 'mode-data)
    [(hash-table ('x x) ('y y) ('dir dir) ('speed speed))
     (let ([dx 5] [dy 5])
       (now 'mode-data (hash-set* (now 'mode-data)
                                  'x (+ x dx) 'y (+ y dy))))]))

(define (keys-fly now pressed)
  (let* ([m (now 'mode-data)]
         [m (cond [(key=? pressed "up") (hash-update m 'speed add1)]
                  [(key=? pressed "down") (if (positive? (hash-ref m 'speed))
                                              (hash-update m 'speed sub1)
                                              m)]
                  [(key=? pressed "left") (hash-update m 'dir (curryr + 10))]
                  [(key=? pressed "right") (hash-update m 'dir (curryr - 10))]
                  [else now])])
    (now 'mode-data m)))

(define (fly now)
  (let ([now (now 'mode 'fly)])
    (now 'mode-data #hash((x . 0) (y . 0)
                          (dir . 0) (speed . 1)))))
