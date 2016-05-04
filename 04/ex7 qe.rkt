#lang racket
(define (sq b a c)(/ (* 2 a)(+(- b) (sqrt(- (* b b)(* 4 a c))))))