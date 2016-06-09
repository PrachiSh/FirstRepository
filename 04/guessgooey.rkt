;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname guessgooey) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;GUSSEING GOONEY
;;It is number gussing game
;; starts with (start lower-limit upper-limit)

;#lang racket
(require rackunit)
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;CONSTANTS

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 200)

(define HELP-TEXT
  (text "up for larger numbers, down for smaller number" 11 "blue"))
(define HELP-TEXT2
  (text "press = when your number is gussed, q to quit" 11 "blue"))

(define MT_SC
  (place-image/align HELP-TEXT 80 50 "left" "top"
  (place-image/align HELP-TEXT2 80 150 "left" "bottom"
  (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))))

(struct interval (small big))

(define interval-10-to-80 ( interval 10 80))
(define interval-10-to-44 (interval 10 44))
(define interval-46-to-80 (interval 80 46))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; START FUNCTION
;;strat : integer integer -> intervals
;;GIVEN : lower limit and upper limit of the interval
;;EFFECT: run simulation, intervals starts to change according to key events
;;RETURNS : intervals to find number
(define (start lower upper)
  (big-bang (interval lower upper)
  (on-key deal-with-guess)
  (to-draw render)
 (stop-when single? render-last-scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;KEY EVENTS FUNCTIONS

;;deal-with-guess : world keyevent -> world
;;GIVEN : a world and a key event
;;RETURN : a world followed by a key event
(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        ((key=? key "=") (stop-with w))))

;;smaller : world -> world
;;GIVEN : a world after a key event "down"
;;RETURN: chnage the upper limit of interval
(define (smaller w)
  (interval (interval-small w)
            (max (interval-small w) (sub1 (guess w)))))

(begin-for-test
  (check-equal?
  (smaller interval-10-to-80) (interval 10 44)))


;;bigger : world -> world
;;GIVEN : a world after key event "up"
;;RETURN : change the lower limit of interval
(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w)))
            (interval-big w)))


(begin-for-test
 (check-equal?
  (bigger interval-10-to-80) interval-46-to-80))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;guess world -> interger
;;GIVEN : a world with intervals
;;RETURNS : average of intervals
(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;WORLD INTO CANVAS

;;render : world -> image
;;GIVEN : a world
;RETURN : image of world with gussed number
(define (render w)
  (overlay (text (number->string (guess w)) 20 "green" ) MT_SC))

;;reder-last-scene: world -> image
;;GIVEN : a world
;;RETURN :  a iamge to end the game
(define (render-last-scene w)
  (overlay (text "END" 20 "red" ) MT_SC))

;;single : world -> world
;;GIVEN : a world
;;RETURN : a world when lower limit and upper limit of interval is equal
(define (single? w )
(= (interval-small w) (interval-big w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(start 0 100)






        