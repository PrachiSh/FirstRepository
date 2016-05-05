#lang racket
(define-struct point( x y))
(make-point 5 3)
;;<point>
(point? (make-point 5 3 ))
;;t
(point-x (make-point 5 3))
;;5
(point? true)
;;f
