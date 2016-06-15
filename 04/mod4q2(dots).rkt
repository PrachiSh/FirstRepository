;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |mod4q2(dots)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;screensaver-4
;;rectangles moving according to velocity
;;velocities can be change with up,down,left,right keys

;;starts with (main 0)

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;CONSTANTS

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 500)

(define RECW 60)
(define RECH 50)

(define RECX 200)
(define RECY 100)

(define RECVEL-X 1)
(define RECVEL-Y 1)

(define REC-IMAGE (rectangle RECW RECH "outline" "blue"))

(define HALF-REC-WIDTH  (/ (image-width  REC-IMAGE) 2))
(define HALF-REC-HEIGHT (/ (image-height REC-IMAGE) 2))

(define empty-canvas (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define CIR-IMAGE (circle 5 "solid" "black"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct world (LOR))
;;A world is a (make-world list)
;;LOR is a list of rectangle
; LOR is
;  -- empty
;  -- (cons rec LOR)

(define-struct rec( REC-IMAGE x-pos y-pos vel-x  vel-y is-selected? LOD pen-down?))
;;A rec is a (make-rec image integer integer integer integer boolean?)
;;x-pos , y-pos gives the position of rectabgle
;;vel-x, vel-y describes the velocity of rectabgle

(define-struct dot ( CIR-IMAGE x-dot y-dot))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;MAIN FUNCTION
;;main :Integer -> world
;;GIVEN : list
;;RETURNS : the final state of world

(define (main initial-pos)
  (big-bang (initial-world initial-pos)
            (on-tick world-after-tick 1)
            (on-mouse world-after-mouse-event)
            (on-key world-after-key-event)
            (on-draw draw-world)))

;;initial-world world -> world
;;GIVEN : a world
;;RETURNS : an empty world
(define (initial-world w)
 (make-world  empty))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;KEY EVENT

;;world-after-key-event : world keyevent -> world
;;GIVEN : a world and a keyevent
;;RETURN : a world and a keaywvwnt`
(define (world-after-key-event w k)
  (cond [(key=? k "n" ) (updated-world w)]
        [else (recvel-after-key-event w k)]))

;;updated-world : world -> list of rectangle
;;GIVEN :  a world
;;RETURN : a list of rectangle
(define (updated-world w)
  (make-world
   (cons (make-rec REC-IMAGE (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) 0 0 false empty false) (world-LOR w))))

;;recvel-after-key-event : world keyevent -> list and a key event
;;GIVEN : a world and a keyevent
;;RETURN : a list and a keyevent
(define (recvel-after-key-event w k)
  (make-world
   (velocity-after-key-event (world-LOR w) k)))

;;velocity-after-keyevent : list, keyevent -> rectangle and a keyevent
;;GIVEN : a world and a keyevent
;;RETURN : a rectabgle and  a keyevent
(define (velocity-after-key-event l k)
  (cond[(empty? l) empty]
       [else (cons (lorvel-after-key-event (first l) k) (velocity-after-key-event (rest l) k))]))

;;lorvel-after-key-evnt : rectangle keyevent -> rectangle
;;GIVEN : a rectangle and a keyevent
;;RETURN : a rectangle
(define (lorvel-after-key-event r k)
  (cond[(key=? k "up") (in-y-vel r)]
       [(key=? k "down") (de-y-vel r)]
       [(key=? k "left") (de-x-vel r)]
       [(key=? k "right") (in-x-vel r)]
       [(key=? k "d") (rec-pen-down r )]
       [(key=? k "u") (rec-pen-up r)]))

(define (rec-pen-down r)
  (if (rec-is-selected? r)
  (make-rec REC-IMAGE (rec-x-pos r) (rec-y-pos r) (rec-vel-x r) (rec-vel-y r) true (rec-LOD r) true)
  r))

(define (rec-pen-up r)
  (if (rec-is-selected? r)
      (make-rec REC-IMAGE (rec-x-pos r) (rec-y-pos r) (rec-vel-x r) (rec-vel-y r) true empty false)
  r))


;;in-y-vel : rectangle -> rectabgle
;;GIVEN : a rectanglr
;;RETURN : a rectangle with incresed y-velocity
(define (in-y-vel r)
  (if (rec-is-selected? r)
       (make-rec  REC-IMAGE (rec-x-pos r) (rec-y-pos r) (rec-vel-x r) (+ (rec-vel-y r) 2) true empty false)
      r))

;;de-y-vel : rectangle -> rectabgle
;;GIVEN : a rectanglr
;;RETURN : a rectangle with decresed y-velocity
(define (de-y-vel r)
  (if (rec-is-selected? r)
  (make-rec REC-IMAGE (rec-x-pos r) (rec-y-pos r) (rec-vel-x r) (- (rec-vel-y r) 2) true empty false)
  r))

;;in-y-vel : rectangle -> rectabgle
;;GIVEN : a rectanglr
;;RETURN : a rectangle with incresed x-velocity 
(define (de-x-vel r)
  (if (rec-is-selected? r)
  (make-rec REC-IMAGE (rec-x-pos r) (rec-y-pos r) (- (rec-vel-x r) 2) (rec-vel-y r) true empty false)
  r))
;;in-y-vel : rectangle -> rectabgle 
;;GIVEN : a rectanglr
;;RETURN : a rectangle with decresded x-velocity
(define (in-x-vel r)
  (if (rec-is-selected? r)
   (make-rec REC-IMAGE (rec-x-pos r) (rec-y-pos r) (+ (rec-vel-x r) 2) (rec-vel-y r) true empty false)
  r))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ON-DRAW

;; draw-world world -> world -> list
;;GIVEN : a world
;;RETRUNS : list
(define (draw-world w)
  (draw-list (world-LOR w)))

  ;(draw-dots-outer (draw-list (world-LOR w)) (world-LOR w))

;;draw-list : list -> scene
;;GIVEN : a list
;;RETURN : a scene with rectangles with there velocity at center 
(define (draw-list l)
  (cond [(empty? l) empty-canvas]
        [else  (draw-dots-outer ( place-image  (overlay-velocity l) (rec-x-pos (first l)) (rec-y-pos (first l)) (draw-list (rest l))) (first l))]))

  (define (draw-dots-outer scene r)
    (cond [(empty? (rec-LOD r)) scene]
          [else  (draw-inner (rec-LOD r) scene) ])
          
    )

(define (draw-inner lod scene)
  (cond [(empty? lod) scene]
        [else (place-image (dot-CIR-IMAGE (first lod)) (dot-x-dot (first lod)) (dot-y-dot (first lod)) (draw-inner (rest lod) scene))]))

;;overlay-velocity : list -> image
;;GIVEN : list
;;RETURN : an image of velocity on rectangles
(define (overlay-velocity l)
  (overlay (text (string-append "(" (number->string (rec-vel-x (first l))) "," (number->string (rec-vel-y (first l))) ")")  10 "red") REC-IMAGE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ON-TICK

;; world-adter-tick : world -> world
;;GIVEN : a world
;;RETURN :  list of rectangle
(define (world-after-tick w )
  (make-world
   (lor-after-tick (world-LOR w))))

;;lor-after-tick : list -> rectangle
;;GIVEN : a list of rectangle
;;RETURN : rectangle
(define (lor-after-tick l)
  (cond[(empty? l) empty]
       [else (cons (rec-after-tick (first l)) (lor-after-tick (rest l)))]))



;;rec-after-tick: a rectangle -> rectangle
;;GIVEN : a rectagle
;;RETURN : a rectangle with tangential motion
(define (rec-after-tick r)
  (cond [(rec-is-selected? r) (selected-rec r)]
        [(or (< (rec-x-pos r) (/ RECW 2)) ( > (rec-x-pos r) (- CANVAS-WIDTH (/ RECW 2))))  (checking-width r)]
        [(or (< (rec-y-pos r) (/ RECH 2)) (> (rec-y-pos r) (- CANVAS-HEIGHT (/ RECH 2)))) (checking-height r) ]
        [else (tick-rec r)]))

;;selected-rec :rectangle-> rectangle
;;GIVEN : rectangle
;;return : rectangle 
(define (selected-rec r)
  (make-rec REC-IMAGE (rec-x-pos r) (rec-y-pos r) (rec-vel-x r) (rec-vel-y r) true (cons (make-dot CIR-IMAGE (rec-x-pos r) (rec-y-pos r)) (rec-LOD r)) false))

;;cheching-width :rectangle-> rectangle
;;GIVEN : rectangle
;;return : rectangle with tangential motion along x-aix
(define (checking-width r)
  (make-rec REC-IMAGE
         (+ (rec-x-pos r) (* (rec-vel-x r) -1)) (rec-y-pos r) (* (rec-vel-x r) -1) (rec-vel-y r) false (cons (make-dot CIR-IMAGE (+ (rec-x-pos r) (* (rec-vel-x r) -1)) (rec-y-pos r)) (rec-LOD r)) false) )

;;cheching-height :rectangle-> rectangle

;;GIVEN : rectangle
;;return : rectangle with tangential motion along y-aix
(define (checking-height r)
  (make-rec REC-IMAGE
           (rec-x-pos r) (+ (rec-y-pos r) (* (rec-vel-y r) -1)) (rec-vel-x r) (* (rec-vel-y r) -1) false (cons (make-dot CIR-IMAGE (rec-x-pos r)  (+ (rec-y-pos r) (* (rec-vel-y r) -1))) (rec-LOD r))  false))

;;tick-rec :rectangle-> rectangle
;;GIVEN : rectangle
;;return : rectangle motion with a tick
(define (tick-rec r)
   (make-rec REC-IMAGE
   (+ (rec-x-pos r) (rec-vel-x r)) (+ (rec-y-pos r) (rec-vel-y r)) (rec-vel-x r) (rec-vel-y r) false (cons (make-dot CIR-IMAGE  (+ (rec-x-pos r) (rec-vel-x r)) (+ (rec-y-pos r) (rec-vel-y r))) (rec-LOD r)) false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;MOUSE EVENTS

;;world-after-mouse-event : world x-pos y-pos mouse-event -> a list of rectangle,x-pos y-pos mpouse event
;;GIVEN : world x-pos y-pos mouse event
;;RETURN :  a list of rectangle,x-pos y-pos mpouse event
(define (world-after-mouse-event w mx my mev)
  (make-world
   (lor-after-mouse-event (world-LOR w) mx my mev)))

;;lor-after-mouse-event : LOR x-pos y-pos mouse event -> rectangle x-pos y-pos mouse event
;;GIVEN : LOR  x-pos y-pos mpouse event
;;RETURN : rectangle x-pos y-pos mouse event
(define (lor-after-mouse-event l mx my mev)
  (cond [(empty? l) empty]
   [else (cons (rec-after-mouse-event (first l) mx my mev ) (lor-after-mouse-event (rest l) mx my mev))]))

;;rec-after-mouse-event : rectangle x-pos y-pos mouse event -> rectangle x-pos y-pos
;;GIVEN : LOR  x-pos y-pos mouse event
;;RETURN : rectangle x-pos y-pos
(define (rec-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down") (rec-after-button-down r mx my)]
    [(mouse=? mev "drag") (rec-after-drag r mx my)]
    [(mouse=? mev "button-up") (rec-after-button-up r mx my)]
    [else r]))

;;rec-afte-button-down : rectangle x-pos y-pos
;;GIVEN : rectangle, x-pos y-pos of mouse
;;RETURN : a rectangle
(define (rec-after-button-down r x y)
  (cond [(in-rec? r x y) (make-rec  REC-IMAGE (rec-x-pos r) (rec-y-pos r) (rec-vel-x r) (rec-vel-y r) true empty false)]
        [else r]))

;;rec-afte-drag : rectangle x-pos y-pos
;;GIVEN : rectangle, x-pos y-pos of mouse
;;RETURN : a rectangle at x-pos y-pos
(define (rec-after-drag r x y)
  (cond [(rec-is-selected? r)
      (make-rec REC-IMAGE x y (rec-vel-x r) (rec-vel-y r) true empty false)]
      [else r]))

;;rec-afte-button-up : rectangle x-pos y-pos
;;GIVEN : rectangle, x-pos y-pos of mouse
;;RETURN : a rectangle at x-pos and y-pos
(define (rec-after-button-up r x y)
  (if (rec-is-selected? r)
      (make-rec REC-IMAGE x y (rec-vel-x r) (rec-vel-y r) false empty false)
      r))

;;in-rec? : rectangle x-pos y-pox
;;GIVEN : rectangle x-pos y-pox
;;RETURN : check if mouse is on the rectangle
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


(main 0)