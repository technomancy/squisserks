#lang racket
(require 2htdp/image)
(provide width height dash-height starify draw-dash scene)

(define width 1024)
(define dash-height 40)
(define height (- 786 dash-height))

(define scene (empty-scene width height "black"))
(define stars (list (circle 1 "solid" "white")
                    (circle 1 "solid" "gray")
                    (circle 2 "solid" "white")
                    (circle 2 "solid" "gray")
                    (overlay ;; (circle 3 "outline" "white")
                     (line 0 7 "white")
                     (line 7 0 "white"))))

(define (starify scene star-count)
  (if (zero? star-count)
      scene
      (let ([x (random width)]
            [y (random height)]
            [star (list-ref stars (random (length stars)))])
        (starify (underlay/xy scene x y star) (sub1 star-count)))))

(define (draw-dash scene now)
  (let ([dash (rectangle width dash-height "solid" "black")]
        [text (text (format "Credits: ~s | Fuel: ~s"
                            (now 'credits) (now 'fuel))
                    14 "white")])
    (above scene (underlay/xy dash 10 10 text))))
