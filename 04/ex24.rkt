#lang racket
 (define (sum lst)
      (cond
        [(empty? lst) 1]
        [else (* (first lst) ( sum (rest lst)))]))

(sum (list 2 3 4))