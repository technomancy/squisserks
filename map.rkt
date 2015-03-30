#lang racket
(require 2htdp/image 2htdp/universe)
(require "draw.rkt")
(provide draw-map keys-map)

(define (index-of l x)
  (for/or ([y l] [i (in-naturals)] #:when (equal? x y)) i))

(define (after-cycle lst target)
  (list-ref lst (modulo (add1 (index-of lst target)) (length lst))))

(define/match (dict-ref-in d . args)
  [(d (list (list k))) (dict-ref d k)]
  [(d (list ks)) (dict-ref-in (dict-ref d (first ks)) (rest ks))]
  [(d (list (list k) not-found)) (dict-ref d k not-found)]
  [(d (list ks not-found)) (dict-ref-in (dict-ref d (first ks)) (rest ks) not-found)])



(define (system-image system)
  (beside (circle 8 "solid" (system 'color))
          (text (system 'name) 10 "white")))

(define (draw-systems scene now)
  (for/fold ([scene scene]) ([system (dict-values (now 'systems))])
    (underlay/xy scene
                 (- (system 'x) 8)
                 (- (system 'y) 8) (system-image system))))

(define (draw-connections scene now)
  (for/fold ([scene scene]) ([system (dict-values (now 'systems))])
    (for/fold ([scene scene]) ([connect (system 'connections)])
      (let ([target (dict-ref (now 'systems) connect)])
        (add-line scene
                  (system 'x) (system 'y)
                  (target 'x) (target 'y) "gray")))))

(define selector-image (rectangle 24 24 "outline" "white"))

(define current-system-image (beside (line 6 4 "white")
                                     (line -6 4 "white")))

(define (draw-nav scene now)
  (let* ([system (dict-ref (now 'systems) (now 'system))]
         [scene (underlay/xy scene (- (system 'x) 6) (- (system 'y) 15)
                             current-system-image)]
         [target-name (dict-ref (now 'mode-data) 'nav-target
                                (lambda _ false))])
    (if target-name
        (let ([target (dict-ref (now 'systems) target-name)])
          (underlay/xy scene (- (target 'x) 12) (- (target 'y) 12)
                       selector-image))
        scene)))

(define (draw-map now)
  (draw-dash (draw-nav (draw-systems (draw-connections (starify scene 30) now) now) now) now))



(define (next-target now)
  (let ([target-name (dict-ref (now 'mode-data) 'nav-target (lambda _ false))]
        [connections (dict-ref (dict-ref (now 'systems) (now 'system))
                               'connections)])
    (now 'mode-data (dict-set (now 'mode-data) 'nav-target
                              (if target-name
                                  (after-cycle connections target-name)
                                  (first connections))))))

(define (jump-target now)
  (let ([target (dict-ref (now 'mode-data) 'nav-target false)])
    (if target
        (let* ([now (now 'system target)]
               [now (now 'mode-data (dict-remove (now 'mode-data) 'nav-target))])
          (now 'fuel (- (now 'fuel) 1)))
        now)))

(define (keys-map now pressed)
  (cond [(key=? pressed "\t") (next-target now)]
        [(key=? pressed "\r") (jump-target now)]
        [else now]))
