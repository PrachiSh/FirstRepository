;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Mod3q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(define (main initial-pos)
  (big-bang (initial-world initial-pos)
            (on-tick world-after-tick 0.5)
            (on-draw world-to-scene)
            ))

;;define rectangle
(define RECH 50)
(define RECW 60)
(define rec-image (rectangle RECH RECW "outline" "blue"))

(define-struct rec ( x-pos y-pos recvel-x recvel-y ))

(define RECVEL1-X -12)
(define RECVEL1-Y 20)
(define RECVEL2-X 23)
(define RECVEL2-Y -14)




;;define canvas
(define canvas-width 400)
(define canvas-height 300)
(define empty-canvas (empty-scene canvas-width canvas-height))
(define rec1-x-cord 200)
(define rec1-y-cord 200)
(define rec2-x-cord 200)
(define rec2-y-cord 100)




(define-struct world (rec1 rec2 paused?))


(define image-of-paused-world-at-20
  (place-image rec-image 150 20
    (place-image rec-image 300 35
      empty-image)))

(define (place-rec c s)
  (place-image
    rec-image (rec-x-pos c) (rec-y-pos c)
    s))

(define (world-to-scene w)
  (place-rec
   (world-rec1 w)
   (place-rec
    (world-rec2 w)
    empty-canvas)))
 

(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (rec-after-tick (world-rec1 w))
       (rec-after-tick (world-rec2 w))
       (world-paused? w)
       )))

(define (rec-after-tick r)
  (cond [(or (< (rec-x-pos r) (/ RECW 2)) ( > (rec-x-pos r) (- canvas-width (/ RECW 2))))  (make-rec
         (+ (rec-x-pos r) (* (rec-recvel-x r) -1)) (rec-y-pos r) (* (rec-recvel-x r) -1) (rec-recvel-y r))]
        [(or (< (rec-y-pos r) (/ RECH 2)) (> (rec-y-pos r) (- canvas-height (/ RECH 2)))) (make-rec
           (rec-x-pos r) (+ (rec-y-pos r) (* (rec-recvel-y r) -1)) (rec-recvel-x r) (* (rec-recvel-y r) -1))]
        [else (make-rec
   (+ (rec-x-pos r) (rec-recvel-x r))
   (+ (rec-y-pos r) (rec-recvel-y r))
    (rec-recvel-x r)
   (rec-recvel-y r))]))




        ;; (if (or (< (rec-x-pos r) (/ RECW 2)) ( > (rec-x-pos r) (- canvas-width (/ RECW 2))))
        ;;(make-rec
        ;; (rec-x-pos r) (rec-y-pos r) (* (rec-recvel-x r) -1) (rec-recvel-y r))
      ;;(if (or (< (rec-y-pos r) (/ RECH 2)) (> (rec-y-pos r) (- canvas-height (/ RECH 2))))
        ;;  (make-rec
          ;; (rec-x-pos r) (rec-y-pos r) (rec-recvel-x r) (* (rec-recvel-y r) -1))
  ;;(make-rec
  ;; (+ (rec-x-pos r) (rec-recvel-x r))
   ;;(+ (rec-y-pos r) (rec-recvel-y r))
   ;; (rec-recvel-x r)
   ;;(rec-recvel-y r)))))

 (define (initial-world y)
 (make-world
 (make-rec rec2-x-cord rec2-y-cord RECVEL2-X RECVEL2-Y)
 (make-rec rec1-x-cord rec1-y-cord RECVEL1-X RECVEL1-Y)
         
  false))
  
(main 0)