#lang racket
(define s (string #\h #\e #\l #\l #\o #\w #\o #\r #\l #\d))
   (string-set! s 5 #\_)
s
