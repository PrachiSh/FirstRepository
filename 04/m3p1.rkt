#lang racket

;;screensaver displaying wo rectangle in scene
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)



(define-struct world ( rec1 rec2 paused?))
;; A world is
;; (make-world image image boolean?
;; Interpretation
;; two rectangles and a paused? to describe the state of rectangles

(define REC-IMAGE  (rectangle 50 60 "outline" blue))


(define recspeed 8)

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(place-image (rectangle 50 60 "outline" blue) 200 200)


  
  