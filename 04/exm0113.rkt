
#lang racket
;; distance: real real-> real
;;disatnce pf apoint from origin
;;gives: value of a point
;;
(define (distance x y)
  ;;
  (sqrt (+(expt x 2)(expt y 2))))
(distance 4 -5)