;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mod1q1try) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;;main function

(define (main initial-pos)
  (big-bang (make-world initial-pos false)
            (on-tick world-after-tick 0.5)
            (on-key world-after-key-event)
            (on-draw world-to-scene)))

;;rectangle image
(define rec-image (rectangle 60 50 "outline" "blue"))

;;rectangle fall
(define recspeed 8)

;;CANVAS
(define canvas-width 400)
(define canvas-height 300)
(define empty-canvas (empty-scene canvas-width canvas-height))
(define rec1-x-cord (- 400 200))
(define rec2-y-cord (- 400 300))


;;data definitions
(define-struct world (rec1 rec2 paused?))

(define-strcut rec (x-pos y-pos))

(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (rec-after-tick (world-rec1 w))
       (rec-after-tick (world-rec2 w))
       (world-paused? w))))

(define (cat-after-tick c)
  (make-cat
   (rec-x-pos c)
   (+ (cat-y-pos c) recspeed)))

(define (world-to-scene w)
  (place-rec
    (world-rec1 w)
    (place-rec
      (world-rec2 w)
      empty-canvas)))

  (define (place-rec c s)
  (place-image
    rec-image
    (rec-x-pos c) (rec-y-pos c)
    s))


(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")
     (world-with-paused-toggled w)]
    [else w]))

  (define (world-with-paused-toggled w)
  (make-world
   (world-rec1 w)
   (world-rec2 w)
   (not (world-paused? w))))

  (define (initial-world y)
  (make-world
    (make-rec rec1-x-cord y false)
    (make-rec rec2-x-cord y false)
    false))

  


