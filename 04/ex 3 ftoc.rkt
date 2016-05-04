#lang racket
(define (ftoc f) (* (- f 32) (/ 5 9)))

(ftoc 32)