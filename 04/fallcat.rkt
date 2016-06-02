;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fallcat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

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
(begin-for-test
  (check-equal? 
    (world-after-tick unpaused-world-at-20) 
    unpaused-world-at-28
    "in unpaused world, the cat should fall CATSPEED pixels and world should still be unpaused")

  (check-equal? 
    (world-after-tick paused-world-at-20)
    paused-world-at-20
    "in paused world, cat should be unmoved"))

;;world-to-scene: world->scene
;;given: a world
;;return: scene with world
(define (world-to-scene w)
  (place-image CAT-IMAGE CAT-X-COORD
               (world-pos w)
               EMPTY-CANVAS))

;;tests
(define image-at-20 (place-image CAT-IMAGE CAT-X-COORD 20 EMPTY-CANVAS))
(begin-for-test
  (check-equal? 
    (world-to-scene unpaused-world-at-20)
    image-at-20
    "test of (world-to-scene unpaused-world-at-20)")

  (check-equal?
    (world-to-scene paused-world-at-20)
    image-at-20
    "test of (world-to-scene paused-world-at-20)"))

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
    unpaused-world-at-20
    "after pause key, a paused world should become unpaused")


  (check-equal?
    (world-after-key-event paused-world-at-20 non-pause-key-event)
    paused-world-at-20
    "after a non-pause key, a paused world should be unchanged")

  (check-equal?
    (world-after-key-event unpaused-world-at-20 non-pause-key-event)
    unpaused-world-at-20
    "after a non-pause key, an unpaused world should be unchanged"))

(main 0)