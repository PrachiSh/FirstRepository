#lang racket
;;falling cat
;;cat fall from top of a scene
;;starts with (main 0(

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

(define CAT-IMAGE (bitmap "cat.png"))
;;catspeed

(define CATSPEED 8)

;;canvas dimentions
(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CAT-X-COORD (/ CANVAS-WIDTH 2))

;;data definition
(define-struct world (pos paused?))


;;examples for world
(define unpaused-world-at-20 (make-world 20 false))  
(define paused-world-at-20   (make-world 20 true))
(define unpaused-world-at-28 (make-world 28 false))  
(define paused-world-at-28   (make-world 28 true))

;;is-paused-key-event? : keyevent -> boolean
;;Given: a keyevent
;;Returens: true if paused
(define (is-pause-key-event? ke)
  (key=? ke " "))

;;example
(define pause-key-event " ")
(define non-pause-key-event "q")

;;world-after-tick: world -> world
;;Given: world w
;;retuen: the world after a tick
(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world (+ (world-pos w) CATSPEED)
             (world-paused? w))))

;;tests

;;world-to-scene: world->scene
;;given: a world
;;return: scene with world
(define (world-to-scene w)
  (place-image CAT-IMAGE CAT-X-COORD
               (world-pos w)
               EMPTY-CANVAS))


;;world-fter-key-event: keyevent -> World
(define (world-after-key-event w kev)
  (if (is-pause-key-event? kev)
    (world-with-paused-toggled w)
    w))

;;world-with-paused-toggled: world -> world
(define (world-with-paused-toggled w)
  (make-world
   (world-pos w)
   (not (world-paused? w))))

;;tests
(begin-for-test
  (check-equal?
    (world-after-key-event paused-world-at-20 pause-key-event)
    paused-world-at-20
    "after pause key, a paused world should become unpaused")


  (check-equal?
    (world-after-key-event paused-world-at-20 non-pause-key-event)
    paused-world-at-20
    "after a non-pause key, a paused world should be unchanged")

  (check-equal?
    (world-after-key-event unpaused-world-at-20 non-pause-key-event)
    unpaused-world-at-20
    "after a non-pause key, an unpaused world should be unchanged"))