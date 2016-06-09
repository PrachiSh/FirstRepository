;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mod3q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;screensaver-1
;;two moving rectangle
;;moves tanget to edge of canvas when hit the border of canvas.

;;start with (main 0)

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;MAIN FUCTION.
;;main : Integer -> world
;;GIVEN : the initial x-position y-position and x-velocity y-velocity of rectangle
;;EFFECT : run the simulation, starting with rectangle moving
;;RETURN : the final state of the world

(define (main initial-pos)
  (big-bang (initial-world initial-pos)
            (on-tick world-after-tick 1)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)
            ))

;;CONSTANTS

(define RECH 50)
(define RECW 60)

(define REC-IMAGE (rectangle RECH RECW "outline" "blue"))

(define RECVEL1-X -12)
(define RECVEL1-Y 20)
(define RECVEL2-X 23)
(define RECVEL2-Y -14)

(define HALF-REC-WIDTH  (/ (image-width  REC-IMAGE) 2))
(define HALF-REC-HEIGHT (/ (image-height REC-IMAGE) 2))

;;dimensions of the canvas

(define canvas-width 400)
(define canvas-height 300)
(define empty-canvas (empty-scene canvas-width canvas-height))
(define REC1-X-CORD 200)
(define REC1-Y-CORD 200)
(define REC2-X-CORD 200)
(define REC2-Y-CORD 100)

;;DATA DEFINITIONS

(define-struct world (rec1 rec2 paused?))
;;A world is a (make-world rectangle rectangle boolean?)
;;rec1 and rec2 are two rectangles
;;paused? describes whether or not the world is paused

;;template:
;;world-fn : world -> ??
;; (define (world-fn w)
;; (...(world-rec1 w) (world-rec2 w) (world-paused? w)))

(define-struct rec ( x-pos y-pos recvel-x recvel-y selected? ))
;;A cat is a (make-cat Integer Integer Integer Integer)
;;Interpretation:
;;x-pos, y-pos gives the position of the rectangle
;;recvel-x, recvel-y gives the velocity the rectangle

;;template:
;; cat-fn : rec -> ??
;; (define (rec-fn r)
;; (....(rec-x-pos r) (rec-y-pos r) (rec-recvel-x r) (rec-recvel-y r)))


;;END DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;world-after-tick : world -> world
;;GIVEN : a world w
;;RETURN : the world that should folow w after a tick
;;STRATEGY: use template for world on w

(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (rec-after-tick (world-rec1 w))
       (rec-after-tick (world-rec2 w))
       (world-paused? w)
       )))

;;rec-after-tick : rec -> rec
;;GIVEN : the state of a rec r
;;RETURNS : the state of the given rec after a tick

(define (rec-after-tick r)
  (cond [(or (< (rec-x-pos r) (/ RECW 2)) ( > (rec-x-pos r) (- canvas-width (/ RECW 2))))  (make-rec
         (+ (rec-x-pos r) (* (rec-recvel-x r) -1)) (rec-y-pos r) (* (rec-recvel-x r) -1) (rec-recvel-y r) false)]
        [(or (< (rec-y-pos r) (/ RECH 2)) (> (rec-y-pos r) (- canvas-height (/ RECH 2)))) (make-rec
           (rec-x-pos r) (+ (rec-y-pos r) (* (rec-recvel-y r) -1)) (rec-recvel-x r) (* (rec-recvel-y r) -1) false )]
        [else (make-rec
   (+ (rec-x-pos r) (rec-recvel-x r))
   (+ (rec-y-pos r) (rec-recvel-y r))
    (rec-recvel-x r)
   (rec-recvel-y r) false)])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;world-to-scene : world ->scene
;;GIVEN : a world w
;;RETURN : a scene that portrays the given world
;;STRATEGY : use template for world w

(define (world-to-scene w)
  (place-rec
   (world-rec1 w)
   (place-rec
    (world-rec2 w)
    empty-canvas)))

;;place-rec :rec scene -> scene
;;GIVEN: a rec r and scene s
;;RETURNS : a scene like s but with the given rec painted on it.

(define (place-rec c s)
  (place-image
    REC-IMAGE (rec-x-pos c) (rec-y-pos c)
    s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; STRATEGY: use template for World on w

(define (world-after-mouse-event w mx my mev)
  (make-world
    (rec-after-mouse-event (world-rec1 w) mx my mev)
    (rec-after-mouse-event (world-rec2 w) mx my mev)
    (world-paused? w)))


;; rec-after-mouse-event : rectangle Integer Integer MouseEvent -> rectangle
;; GIVEN: a rectangle and a description of a mouse event
;; RETURNS: the rectangle that should follow the given mouse event

(define (rec-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down") (rec-after-button-down r mx my)]
    [(mouse=? mev "drag") (rec-after-drag r mx my)]
    [(mouse=? mev "button-up") (rec-after-button-up r mx my)]
    [else r]))

;; rec-after-button-down : rectangle Integer Integer -> rectangle
;;GIVEN : a rectangle and a location
;; RETURNS: the rectangle following a button-down at the given location.

(define (rec-after-button-down r x y)
  (if (in-rec? r x y)
      (make-rec (rec-x-pos r) (rec-y-pos r) (rec-recvel-x r) (rec-recvel-y r) true)
      r))
;;rec-after-drag : rectangle Integer Integer -> rectangle
;;GIVEN : a rectangle and a location
;;RETURNS: the rectangle following a drag at the given location

(define (rec-after-drag r x y)
  (cond [(rec-selected? r)
      (make-rec x y (rec-recvel-x r) (rec-recvel-y r) true)]
      [else r]))

;;cat-after-button-up : rectangle Integer Integer -> rectangle
;; GIVEN : a rectangle and a location
;; RETURNS: the rectangle following a button-up at the given location
 (define (rec-after-button-up r x y)
  (if (rec-selected? r)
      (make-rec  x y (rec-recvel-x r) (rec-recvel-y r)  true)
      r))

(define (in-rec? r x y)
  (and
    (<= 
      (- (rec-x-pos r) HALF-REC-WIDTH)
      x
      (+ (rec-x-pos r) HALF-REC-WIDTH))
    (<= 
      (- (rec-y-pos r) HALF-REC-HEIGHT)
      y
      (+ (rec-y-pos r) HALF-REC-HEIGHT))))




;; initial-world : initial -> world
;;RETURNS : a world with two rectangles at a given x and y coordinates
 (define (initial-world y)
 (make-world
 (make-rec REC2-X-CORD REC2-Y-CORD RECVEL2-X RECVEL2-Y false )
 (make-rec REC1-X-CORD REC1-Y-CORD RECVEL1-X RECVEL1-Y false)
  false))
  
(main 0)