#lang racket
(require "fstruct.rkt" 2htdp/image 2htdp/universe)

(fstruct now (worlds mode world ship))

(fstruct ship (x y th image))

(fstruct world (x y name color connections))

(define width 1024)
(define height 786)
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

(define (world-image world)
  (beside (circle 8 "solid" (world 'color))
          (text (world 'name) 10 "white")))

(define (draw-worlds scene now)
  (for/fold ([scene (starify scene 25)]) ([world (dict-values (now 'worlds))])
    (underlay/xy scene
                 (- (world 'x) 8)
                 (- (world 'y) 8) (world-image world))))

(define (draw-connections scene now)
  (for/fold ([scene (starify scene 25)]) ([world (dict-values (now 'worlds))])
    (for/fold ([scene scene]) ([connect (world 'connections)])
      (let ([target (dict-ref (now 'worlds) connect)])
        (add-line scene
                  (world 'x) (world 'y)
                  (target 'x) (target 'y) "gray")))))

(define selector-image (rectangle 24 24 "outline" "white"))

(define (draw-selector scene now)
  (let ([world (dict-ref (now 'worlds) (now 'world))])
    (underlay/xy scene (- (world 'x) 12) (- (world 'y) 12)
                 selector-image)))

(define (draw-map now)
  (draw-selector (draw-worlds (draw-connections scene now) now) now))

(define (draw now)
  (draw-map now))

(define (key now pressed)
  now)

(module+ main
  (big-bang (now (for/fold ([h #hash()]) ([world (map (curry apply world)
                                                       (with-input-from-file
                                                         "worlds.rktd" read))])
                   (dict-set h (world 'name) world)) 'map "Andoria" false)
            (on-key key)
            (to-draw draw)))

