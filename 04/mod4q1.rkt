;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mod4q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 500)

(define RECW 60)
(define RECH 50)

(define RECX 200)
(define RECY 100)

(define RECVEL-X -12)
(define RECVEL-Y 20)

(define (main initial-pos)
  (big-bang (initial-world initial-pos)
            (on-mouse world-after-mouse-event)
            (on-key world-after-key-event)
            (on-draw draw-world)))

(define REC-IMAGE (rectangle RECW RECH "outline" "blue"))
(define CIR-IMAGE (circle 30 "outline" "red"))
(define ELP-IMAGE (ellipse 30 60 "solid" "blue"))

(define HALF-REC-WIDTH  (/ (image-width  REC-IMAGE) 2))
(define HALF-REC-HEIGHT (/ (image-height REC-IMAGE) 2))



(define empty-canvas (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define-struct world (LOR))

(define-struct rec( REC-IMAGE x-pos y-pos vel-x vel-y selected?))

(define (initial-world w)
 (make-world  empty))

(define (world-after-key-event w k)
  (cond [(key=? k "n" ) (updated-world  w)]))

(define (updated-world w)
  (make-world
   (cons (make-rec REC-IMAGE (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) 0 0 true) (world-LOR w))))

(define (draw-world w)
  (draw-list (world-LOR w )))

(define (draw-list l)
  (cond [(empty? l) empty-canvas]
        [(equal? (length l) 1) (place-image CIR-IMAGE 100 100 empty-canvas)]
        [else (place-image ELP-IMAGE 150 100 empty-canvas)]))

 
(define (place-rec c)
  (place-image REC-IMAGE (rec-x-pos c) (rec-y-pos c) empty-canvas))

 
(define (world-after-mouse-event w mx my mev)
  (make-world
   (lor-after-mouse-event (world-LOR w) mx my mev)))


(define (lor-after-mouse-event l mx my mev)
  (cond [(empty? l) empty]
   [else (cons (rec-after-mouse-event (first l)mx my mev ) (lor-after-mouse-event (rest l) mx my mev))]))

(define (rec-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down") (rec-after-button-down r mx my)]
    [(mouse=? mev "drag") (rec-after-drag r mx my)]
    [(mouse=? mev "button-up") (rec-after-button-up r mx my)]
    [else r]))

(define (rec-after-button-down r x y)
  (if (in-rec? r x y)
      (make-rec  REC-IMAGE (rec-x-pos r) (rec-y-pos r) (rec-vel-x r) (rec-vel-y r) true)
      r))

(define (rec-after-drag r x y)
  (cond [(rec-selected? r)
      (make-rec REC-IMAGE x y (rec-vel-x r) (rec-vel-y r) true)]
      [else r]))

(define (rec-after-button-up r x y)
  (if (rec-selected? r)
      (make-rec REC-IMAGE x y (rec-vel-x r) (rec-vel-y r)  true)
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


(main 0)