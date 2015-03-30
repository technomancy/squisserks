#lang racket
(require "fstruct.rkt" 2htdp/image 2htdp/universe)
(require "map.rkt" "fly.rkt")

(fstruct now (systems mode mode-data
                      system credits fuel cargo))

(fstruct system (x y name color connections))



(define (draw now)
  (random-seed (apply + (map char->integer (string->list (now 'system)))))
  (case (now 'mode)
    ['map (draw-map now)]
    ['fly (draw-fly now)]))



(define (key now pressed)
  (cond [(key=? pressed " ") (fly now)]
        [(key=? "`" pressed) (begin (printf "~s~n" now) now)]
        [(key=? "escape" pressed) (now 'mode 'map)]
        [else (case (now 'mode)
                ['map (keys-map now pressed)]
                ['fly (keys-fly now pressed)])]))

(define (tick now)
  (case (now 'mode)
    ['map now]
    ['fly (tick-fly now)]))



(module+ main
  (big-bang (now (for/fold ([h #hash()]) ([system (map (curry apply system)
                                                       (with-input-from-file
                                                         "systems.rktd" read))])
                   (dict-set h (system 'name) system)) 'map #hash()
                   "Andoria" 0 100 '())
            (on-key key)
            (to-draw draw)
            (on-tick tick)))
